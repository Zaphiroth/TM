# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM
# Purpose:      TM curves fitting and optimal solution
# programmer:   Zhe Liu
# Date:         12-14-2018
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

##------------------------------------------------------------------------------
##--                 Loading the required packages
##------------------------------------------------------------------------------

options(scipen = 200,
        mongodb = list("host" = "mongodb://localhost:27017"))

suppressPackageStartupMessages({
  library(openxlsx)
  library(RODBC)
  library(plm)
  library(dynlm)
  library(data.table)
  library(stringi)
  library(stringr)
  library(DT)
  library(tidyverse)
  library(mongolite)
  library(grid)
  library(gridExtra)
  # library(CVXR)
  library(rSymPy)
  library(DEoptim)
  library(DEoptimR)
})

sympyStart()

##------------------------------------------------------------------------------
##--                 Loading information
##------------------------------------------------------------------------------

##-- intermedia
db_data <- mongo(collection = "intermedia", db = "TMIST",
                 url = options()$mongodb$host,
                 verbose = FALSE, options = ssl_options())

##-- models
model_info <- db_data$find('{"uuid" : "all_new"}')

curves_orig <- model_info$curves[[1]]
curves <- list()
for (name in curves_orig$curve_name) {
  
  curve <- filter(curves_orig, curve_name == name)$curve_data
  curves[[name]] <- data.frame(x = curve[[1]]$x, y = curve[[1]]$y)
}

weightages_orig <- model_info$weightage[[1]]
weightages <- list()
for (name in weightages_orig$up_index) {
  
  weightage <- filter(weightages_orig, up_index == name)$weight
  weightages[[name]] <- weightage[[1]]
}

##-- product
# db_product <- mongo(collection = "products", db = "TMIST",
#                     url = options()$mongodb$host,
#                     verbose = FALSE, options = ssl_options())
# 
# products <- db_product$find() %>%
#   select(-date) %>%
#   mutate(prod_unit_price = as.numeric(prod_unit_price),
#          prod_unit_cost = as.numeric(prod_unit_cost),
#          prod_code = as.integer(prod_code)) %>%
#   filter(prod_name == "口服抗生素")

##-- hospital
# db_hospital <- mongo(collection = "hospitals", db = "TMIST",
#                      url = options()$mongodb$host,
#                      verbose = FALSE, options = ssl_options())
# 
# hospital_code <- db_hospital$find() %>%
#   select(hosp_name,hosp_code) %>%
#   mutate(hosp_code = as.integer(hosp_code),
#          big = c(1, 0, 1, 0, 0, 0, 0, 0, 0, 1))

##-- potentials
db_potential <- mongo(collection = "constrats", db = "TMIST",
                      url = options()$mongodb$host,
                      verbose = FALSE, options = ssl_options())

potentials <- db_potential$find() %>%
  select(-date) %>%
  mutate(hosp_code = as.numeric(hosp_code),
         prod_code = as.numeric(prod_code),
         phrase_0 = as.numeric(phrase_0),
         phrase_1 = as.numeric(phrase_1),
         phrase_2 = as.numeric(phrase_2),
         phrase_3 = as.numeric(phrase_3),
         phrase_4 = as.numeric(phrase_4)) %>%
  filter(prod_code == 1)
  # gather(phase, potential, -hosp_code, -prod_code) %>%
  # mutate(phase = as.numeric(substr(phase, 8, 8)),
  #        potential = as.numeric(potential),
  #        potential = ifelse(is.na(potential), 0, potential),
  #        hosp_code = as.integer(hosp_code),
  #        prod_code = as.integer(prod_code)) %>%
  # left_join(hospital_code, by = "hosp_code") %>%
  # left_join(select(products, prod_code, prod_name), by ="prod_code") %>%
  # select(hosp_name, hosp_code, big, prod_name, prod_code, phase, potential) %>%
  # filter(prod_name == "口服抗生素", phase == 1)

##-- salesmen
# db_salesmen <- mongo(collection = "salesmen", db = "TMIST",
#                      url = options()$mongodb$host,
#                      verbose = FALSE, options = ssl_options())
# 
# salesmen <- db_salesmen$find() %>%
#   select(-date)

##-- budget
# db_budget <- mongo(collection = "budget", db = "TMIST",
#                    url = options()$mongodb$host,
#                    verbose = FALSE, options = ssl_options())
# 
# budgets <- db_budget$find() %>%
#   mutate(phase = as.integer(phrase),
#          total_budget = as.numeric(one_product_budget)) %>%
#   select(phase, total_budget)

##-- target
# db_target <- mongo(collection = "target", db = "TMIST",
#                    url = options()$mongodb$host,
#                    verbose = FALSE, options = ssl_options())
# 
# targets <- db_target$find()

##-- last phrase information
db_info <- db_data$find('{"uuid" : "cebe92eb-ad0f-4053-a1c6-8aaa1594a480"}')

last_info <- db_info$inter[[1]]$data[[1]]

# last_report <- db_info$inter_data$report[[1]][1, 1:4]
# colnames(last_report) <- c("phase", "sales", "profit", "team")

# price <- 89

pp_hospitals <- last_info %>%
  select(hosp_name,
         hosp_code,
         prod_name,
         prod_code,
         real_revenue,
         real_volume,
         sr_sales_performance,
         deployment_quality_index,
         customer_relationship_index,
         promotional_support_index,
         sales_performance,
         offer_attractiveness,
         acc_offer_attractiveness) %>%
  distinct()
colnames(pp_hospitals)[5:13] <- paste0("pp_", colnames(pp_hospitals)[5:13])

pp_salesmen <- last_info %>%
  select(salesmen,
         sales_level,
         real_revenue_by_sr,
         real_volume_by_sr,
         sr_acc_revenue,
         sales_skills_index,
         product_knowledge_index,
         motivation_index,
         sr_acc_field_work,
         target_revenue_realization_by_sr) %>%
  distinct()
colnames(pp_salesmen)[3:10] <- paste0("pp_", colnames(pp_salesmen)[3:10])

##-- arguments
business_input <- data.frame(hosp_name = c("人民医院", "军区医院", "中日医院", "铁路医院", "海港医院", "第六医院", "小营医院", "西河医院", "光华医院", "大学医院"),
                             budget = paste0("a", c(1:10)),
                             salesmen = c("小宋", "小木", "小兰", "小白", "小木", "小青", "小兰", "小白", "小宋", "小青"),
                             prod_value = paste0("b", c(1:10)),
                             visit_hours = paste0("c", c(1:10)))

management_input <- data.frame(salesmen = c("小宋", "小白", "小青", "小木", "小兰", "经理"),
                               sales_level = c("senior", "middle", "junior", "junior", "middle", "manager"),
                               能力辅导 = c(paste0("d", c(1:5)), paste0("d", c(1:5), collapse = " + ")),
                               实地协防 = c(paste0("e", c(1:5)), paste0("e", c(1:5), collapse = " + ")),
                               团队例会 = "f",
                               KPI分析 = c(0, 0, 0, 0, 0, "g"),
                               行政工作 = c(0, 0, 0, 0, 0, "h"),
                               产品培训 = c(paste0("i", c(1:5)), 0))

##------------------------------------------------------------------------------
##--                 Function
##------------------------------------------------------------------------------

curves.linear <- paste0("curve", c(6, 19, 20, 21, 22, 24, 46, 47, 48))
curves.logistic <- paste0("curve", c(3, 9, 25, 40, 51, 52))

##-- fitting plots
for (curve in names(curves)) {
  
  if (curve %in% curves.linear) {
    fml <- as.formula("y ~ x")
  } else if (curve %in% curves.logistic) {
    fml <- as.formula("y ~ x + I(x^2) + I(x^3)")
  } else {
    fml <- as.formula("y ~ x + I(x^2)")
  }
  
  png(file = paste0("curves_plot/", curve, ".png"))
  
  fit <- ggplot(curves[[curve]], aes(curves[[curve]]$x, curves[[curve]]$y)) +
    geom_point() +
    labs(x = "x", y = "y") +
    geom_smooth(method = "glm", formula = fml, color = "indianred")
  print(fit)
  
  dev.off()
}

##-- models
functions <- list()

for (curve in names(curves)) {
  
  if (curve %in% curves.linear) {
    functions[[curve]] <- glm(y ~ x, data = curves[[curve]])
  } else if (curve %in% curves.logistic) {
    curve1 <- curves[[curve]] %>%
      mutate(y = y - min(y),
             y = y / max(y),
             y = ifelse(y == 0, exp(-10), y))
    # functions[[curve]] <- glm(y ~ x, family = binomial(link = "logit"), data = curve1)
    functions[[curve]] <- glm(y ~ x + I(x^2) + I(x^3), data = curves[[curve]])
  } else{
    functions[[curve]] <- glm(y ~ x + I(x^2), data = curves[[curve]])
  }
}

##-- functions
curve.func <- function(curve, x) {
  
  if (curve %in% curves.linear) {
    z1 <- functions[[curve]]$coefficients[1]
    z2 <- functions[[curve]]$coefficients[2]
    paste0(z1, " + ", z2, " * ", x)
  } else if (curve %in% curves.logistic) {
    z1 <- functions[[curve]]$coefficients[1]
    z2 <- functions[[curve]]$coefficients[2]
    # paste0("1 / (1 + exp(", z1, " +", z2, " * ", x, ")) * ", max(curves[[curve]]$y) - min(curves[[curve]]$y), " + ", min(curves[[curve]]$y))
    z3 <- functions[[curve]]$coefficients[3]
    z4 <- functions[[curve]]$coefficients[4]
    paste0(z1, " + ", z2, " * ", x, " + ", z3, "*", x, "**2", " + ", z4, " * ", x, "**3")
  } else {
    z1 <- functions[[curve]]$coefficients[1]
    z2 <- functions[[curve]]$coefficients[2]
    z3 <- functions[[curve]]$coefficients[3]
    paste0(z1, " + ", z2, " * ", x, " + ", z3, " * ", x, "**2")
  }
}

##------------------------------------------------------------------------------
##--                 Variables
##------------------------------------------------------------------------------

##-- variables
for (i in 1:10) {
  assign(paste0("a", i), Var(paste0("a", i)))
  assign(paste0("b", i), Var(paste0("b", i)))
  assign(paste0("c", i), Var(paste0("c", i)))
  assign(paste0("y", i), Var(paste0("y", i)))
  assign(paste0("c_prop", i), Var(paste0("c_prop", i)))
}

for (i in 1:5) {
  assign(paste0("d", i), Var(paste0("d", i)))
  assign(paste0("e", i), Var(paste0("e", i)))
  assign(paste0("i", i), Var(paste0("i", i)))
  assign(paste0("l", i), Var(paste0("l", i)))
}

f <- Var("f")
g <- Var("g")
h <- Var("h")

p1 <- Var("p1")
p2 <- Var("p2")
k1 <- Var("k1")
k2 <- Var("k2")
j1 <- Var("j1")

for (i in 1:6) {
  assign(paste0("n", i), Var(paste0("n", i)))
}

for (i in 2:7) {
  assign(paste0("m", i), Var(paste0("m", i)))
}

c_sum <- Var("c_sum")
sympy("c_sum = c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10")
c_prop <- Var("c_prop")

a <- c(a1, a2, a3, a4, a5, a6, a7, a8, a9, a10)
b <- c(b1, b2, b3, b4, b5, b6, b7, b8, b9, b10)
c <- c(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10)
d <- c(d1, d2, d3, d4, d5)
e <- c(e1, e2, e3, e4, e5)
i <- c(i1, i2, i3, i4, i5)

##------------------------------------------------------------------------------
##--                 Loop
##------------------------------------------------------------------------------

hosp_men <- data.frame(hosp = 1:10,
                       big = c(1, 0, 1, 0, 0, 0, 0, 0, 0, 1),
                       No = c(1, 4, 5, 2, 4, 3, 5, 2, 1, 3),
                       salesmen = c("小宋", "小木", "小兰", "小白", "小木", "小青", "小兰", "小白", "小宋", "小青"))

for (z in 1:10) {
  
  x = hosp_men[z, 1]
  big = hosp_men[z, 2]
  y = hosp_men[z, 3]
  
  if (big == 1) {
    w = "curve39"
  } else {
    w = "curve35"
  }
  
  sympy("c_prop = ", paste0("c", x)," / c_sum")
  
  sympy("p1 = ", paste0(curve.func("curve42", paste0("(e", y, "+", pp_salesmen$pp_sr_acc_field_work[y], ")")), " + ", pp_salesmen$pp_sales_skills_index[y]))
  sympy("p2 = ", paste0(curve.func("curve43", paste0("d", y)), " + ", pp_salesmen$pp_sales_skills_index[y]))
  p3 <- predict(functions[["curve44"]],
                data.frame(x = predict(functions[["curve11"]], 
                                       data.frame(x = pp_salesmen$pp_sr_acc_revenue[y])))) + pp_salesmen$pp_sales_skills_index[y]
  
  sympy("n1 = ", paste0(0.2, " * (", p1, ") + ", 0.3, " * (", p2, ") + ", 0.5 * p3))
  sympy("n2 = ", paste0(curve.func("curve26", paste0("i", y)), " + ", pp_salesmen$pp_product_knowledge_index[y]))
  sympy("n3 = ", paste0("(c", x, " - 2 * ", predict(functions[["curve12"]], data.frame(x = pp_salesmen$pp_motivation_index[y])), " * ",c_prop, ") / 2.5"))
  sympy("n4 = ", paste0(curve.func("curve5", h), " + ", pp_hospitals$pp_deployment_quality_index[x]))
  sympy("n5 = ", paste0(curve.func("curve7", f), " + ", pp_hospitals$pp_deployment_quality_index[x]))
  sympy("n6 = ", paste0("(", curve.func("curve8", g), ") * ", pp_hospitals$pp_deployment_quality_index[x]))
  
  m1 <- predict(functions[["curve32"]], data.frame(x = pp_salesmen$pp_motivation_index[y])) + pp_hospitals$pp_sr_sales_performance[x]
  sympy("m2 = ", paste0(curve.func("curve34", paste0("(", n1, ")")), " + ", pp_hospitals$pp_sr_sales_performance[x]))
  sympy("m3 = ", paste0(curve.func("curve33", paste0("(", n2, ")")), " + ", pp_hospitals$pp_sr_sales_performance[x]))
  sympy("m4 = ", paste0(curve.func(w, paste0("(", n3, ")")), " + ", pp_hospitals$pp_sr_sales_performance[x]))
  sympy("m5 = ", paste0(0.3, " * (", n4, ") + ", 0.3, " * (", n5, ") + ", 0.4, " * (", n6, ")"))
  sympy("m6 = ", paste0(curve.func("curve26", paste0("i", y)), " + ", pp_salesmen$pp_product_knowledge_index[y]))
  sympy("m7 = ", paste0(curve.func("curve30", paste0("(", 100 / pp_hospitals$pp_real_revenue[x], " * a", x, ")"))))
  
  sympy("l1 = ", paste0(0.15, " * (", m1, ") + ", 0.3, " * (", m2, ") + ", 0.05, " * (", m3, ") + ", 0.5, " * (", m4, ")"))
  sympy("l2 = ", paste0(curve.func("curve40", paste0("(e", y, " / 10)")), " + ", pp_hospitals$pp_sales_performance[x]))
  sympy("l3 = ", paste0(curve.func("curve41", paste0("(", m5, ")")), " + ", pp_hospitals$pp_sales_performance[x]))
  sympy("l4 = ", paste0(curve.func("curve2", paste0("(", m6, ")")), " + ", predict(functions[["curve4"]],
                                                                                   data.frame(x = pp_hospitals$pp_customer_relationship_index[x]))))
  sympy("l5 = ", paste0(curve.func("curve3", paste0("(", m7, ")")), " + ", predict(functions[["curve4"]],
                                                                                   data.frame(x = pp_hospitals$pp_customer_relationship_index[x]))))
  
  sympy("k1 = ", paste0(0.75, " * (", l1, ") + ", 0.05, " * (", l2, ") + ", 0.2, " * (", l3, ")"))
  sympy("k2 = ", paste0(0.35, " * (", l4, ") + ", 0.65, " * (", l5, ")"))
  
  sympy("j1 = ", paste0(0.6 * 100, " * (", curve.func("curve25", paste0("(", k1, ")")), ") + ", 0.4 * 100, " * (", curve.func("curve19", paste0("(", k2, ")")), ")"))
  j2 <- pp_hospitals$pp_offer_attractiveness[x]
  
  sympy(paste0("y", x), " = ", paste0(potentials$phrase_2[x] / 100, " * (", curve.func("curve51", paste0("(", 0.8, " * (", j1, ") + ", 0.2 * j2, ")")), ")"))
}

##------------------------------------------------------------------------------
##--                 Calculation by genetic algorithm
##------------------------------------------------------------------------------

yy <- sympy("yy = -(y1 + y2 + y3 + y4 + y5 + y6 + y7 + y8 + y9 + y10)")

##-- by DEoptim
set.seed(1234)
num_core <- parallel::detectCores()
num_core_use <- ceiling(num_core - 1)
cl <- parallel::makeCluster(num_core_use)

fcn1 <- list(
  obj = function(x) {
    a1 <- x[1]; a2 <- x[2]; a3 <- x[3]; a4 <- x[4]; a5 <- x[5]; a6 <- x[6]; a7 <- x[7]; a8 <- x[8]; a9 <- x[9]; a10 <- x[10]
    c1 <- x[11]; c2 <- x[12]; c3 <- x[13]; c4 <- x[14]; c5 <- x[15]; c6 <- x[16]; c7 <- x[17]; c8 <- x[18]; c9 <- x[19]; c10 <- x[20]
    d1 <- x[21]; d2 <- x[22]; d3 <- x[23]; d4 <- x[24]; d5 <- x[25]
    e1 <- x[26]; e2 <- x[27]; e3 <- x[28]; e4 <- x[29]; e5 <- x[30]
    f <- x[31]; g <- x[32]; h <- x[33]
    i1 <- x[34]; i2 <- x[35]; i3 <- x[36]; i4 <- x[37]; i5 <- x[38]
    
    if (a1 + a2 + a3 + a4 +a5 + a6 + a7 + a8 + a9 + a10 > 100 |
        c1 + c9 + d1 + f + i1 > 100 |
        c2 + c5 + d4 + f + i4 > 100 |
        c3 + c7 + d5 + f + i5 > 100 |
        c4 + c8 + d2 + f + i2 > 100 |
        c6 + c10 + d3 + f + i3 > 100 |
        e1 - c1 > 0 |
        e4 - c2 > 0 |
        e5 - c3 > 0 |
        e2 - c4 > 0 |
        e4 - c5 > 0 |
        e3 - c6 > 0 |
        e5 - c7 > 0 |
        e2 - c8 > 0 |
        e1 - c9 > 0 |
        e3 - c10 > 0 |
        d1 + d2 + d3 + d4 + d5 + e1 + e2 + e3 + e4 + e5 + f + g + h > 100) {
      r <- Inf
    } else {
      eval(parse(text = yy))
    }
    return(r)
  },
  lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0),
  upper = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100)
)

system.time(result1 <- 
              DEoptim(fn = fcn1$obj,
                      lower = fcn1$lower,
                      upper = fcn1$upper,
                      control = DEoptim.control(itermax = 10000000,
                                                trace = 1000,
                                                c = 0.4,
                                                parallelType = 1,
                                                cluster = cl)))

##-- by DEoptimR
fcn <- list(
  obj = function(x) {
    a1 <- x[1]; a2 <- x[2]; a3 <- x[3]; a4 <- x[4]; a5 <- x[5]; a6 <- x[6]; a7 <- x[7]; a8 <- x[8]; a9 <- x[9]; a10 <- x[10]
    c1 <- x[11]; c2 <- x[12]; c3 <- x[13]; c4 <- x[14]; c5 <- x[15]; c6 <- x[16]; c7 <- x[17]; c8 <- x[18]; c9 <- x[19]; c10 <- x[20]
    d1 <- x[21]; d2 <- x[22]; d3 <- x[23]; d4 <- x[24]; d5 <- x[25]
    e1 <- x[26]; e2 <- x[27]; e3 <- x[28]; e4 <- x[29]; e5 <- x[30]
    f <- x[31]; g <- x[32]; h <- x[33]
    i1 <- x[34]; i2 <- x[35]; i3 <- x[36]; i4 <- x[37]; i5 <- x[38]
    eval(parse(text = yy))
  },
  lower = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
            0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
            0, 0, 0, 0, 0, 0, 0, 0),
  upper = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100),
  con = function(x) {
    a1 <- x[1]; a2 <- x[2]; a3 <- x[3]; a4 <- x[4]; a5 <- x[5]; a6 <- x[6]; a7 <- x[7]; a8 <- x[8]; a9 <- x[9]; a10 <- x[10]
    c1 <- x[11]; c2 <- x[12]; c3 <- x[13]; c4 <- x[14]; c5 <- x[15]; c6 <- x[16]; c7 <- x[17]; c8 <- x[18]; c9 <- x[19]; c10 <- x[20]
    d1 <- x[21]; d2 <- x[22]; d3 <- x[23]; d4 <- x[24]; d5 <- x[25]
    e1 <- x[26]; e2 <- x[27]; e3 <- x[28]; e4 <- x[29]; e5 <- x[30]
    f <- x[31]; g <- x[32]; h <- x[33]
    i1 <- x[34]; i2 <- x[35]; i3 <- x[36]; i4 <- x[37]; i5 <- x[38]
    c(a1 + a2 + a3 + a4 +a5 + a6 + a7 + a8 + a9 + a10 - 100,
      c1 + c9 + d1 + f + i1 - 100, c2 + c5 + d4 + f + i4 - 100, c3 + c7 + d5 + f + i5 - 100, c4 + c8 + d2 + f + i2 - 100, c6 + c10 + d3 + f + i3 - 100,
      # d1 - c1, d4 - c2, d5 - c3, d2 - c4, d4 - c5, d3 - c6, d5 - c7, d2 - c8, d1 - c9, d3 - c10,
      e1 - c1, e4 - c2, e5 - c3, e2 - c4, e4 - c5, e3 - c6, e5 - c7, e2 - c8, e1 - c9, e3 - c10,
      # f - c1, f - c2, f - c3, f - c4, f - c5, f - c6, f - c7, f - c8, f - c9, f - c10,
      # i1 - c1, i4 - c2, i5 - c3, i2 - c4, i4 - c5, i3 - c6, i5 - c7, i2 - c8, i1 - c9, i3 - c10,
      d1 + d2 + d3 + d4 + d5 + e1 + e2 + e3 + e4 + e5 + f + g + h - 100)
  }
)

system.time(result <- JDEoptim(lower = fcn$lower,
                               upper = fcn$upper,
                               fn = fcn$obj,
                               constr = fcn$con,
                               tol = 1e-3,
                               maxiter = 10000000,
                               trace = TRUE,
                               triter = 1000))
















