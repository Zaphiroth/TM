# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM
# Purpose:      TM curves fitting and optimal solution
# programmer:   Zhe Liu
# Date:         25-03-2019
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

management_input <- data.frame(salesmen = c("小宋", "小木", "小兰", "小白", "小青", "经理"),
                               sales_level = c("senior", "junior", "middle", "junior", "middle", "manager"),
                               能力辅导 = c(paste0("d", c(1:5)), paste0("d", c(1:5), collapse = " + ")),
                               实地协防 = c(paste0("e", c(1:5)), paste0("e", c(1:5), collapse = " + ")),
                               团队例会 = "f",
                               KPI分析 = c(0, 0, 0, 0, 0, "g"),
                               行政工作 = c(0, 0, 0, 0, 0, "h"),
                               产品培训 = c(paste0("i", c(1:5)), 0))

##------------------------------------------------------------------------------
##--                 Fitting plot
##------------------------------------------------------------------------------

curves.linear <- paste0("curve", c(6, 19, 20, 21, 22, 24, 46, 47, 48))
curves.logistic <- paste0("curve", c(3, 9, 25, 40, 51, 52))

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

##------------------------------------------------------------------------------
##--                 Piecewise function
##------------------------------------------------------------------------------

curve.func <- function(curve, x0) {
  
  if (!is.numeric(x0))
    return(NULL)
  
  variables <- curves[[curve]]
  x <- variables$x
  y <- variables$y
  
  if (x0 <= x[1]) {
    y0 <- y[1]
  } else if (x0 >= x[length(x)]) {
    y0 <- y[length(y)]
  } else if (length(x[which(x == x0)]) == 1) {
    y0 <- variables$y[which(x == x0)]
  } else if (length(x[which(x == x0)]) == 0) {
    ord <- c(x, x0)
    ord <- ord[order(ord)]
    
    x1 <- ord[which(ord == x0) - 1]
    x2 <- ord[which(ord == x0) + 1]
    
    y1 <- variables$y[which(x == x1)]
    y2 <- variables$y[which(x == x2)]
    
    y0 <- (x0 - x1) / (x2 - x1) * (y2 - y1) + y1
  }
  
  return(round(y0, 4))
}

##------------------------------------------------------------------------------
##--                 Expression
##------------------------------------------------------------------------------

hosp_men <- data.frame(hosp = 1:10,
                       big = c(1, 0, 1, 0, 0, 0, 0, 0, 0, 1),
                       salesmen = c("小宋", "小木", "小兰", "小白", "小木", "小青", "小兰", "小白", "小宋", "小青"))

c_sum <- "c1 + c2 + c3 + c4 + c5 + c6 + c7 + c8 + c9 + c10"

for (z in 1:10) {
  
  hosp = hosp_men$hosp[z]
  big = hosp_men$big[z]
  man = hosp_men$salesmen[z]
  
  if (big == 1) {
    w = "curve39"
  } else {
    w = "curve35"
  }
  
  no = switch(as.character(man),
              "小宋" = 1,
              "小木" = 2,
              "小兰" = 3,
              "小白" = 4,
              "小青" = 5)
  
  c_prop <- paste0("c", hosp, " / (", c_sum, ")")
  
  p1 <- paste0("curve.func('curve42', e", no, "+", pp_salesmen$pp_sr_acc_field_work[no], ")", " + ", pp_salesmen$pp_sales_skills_index[no])
  p2 <- paste0("curve.func('curve43', d", no, ") + ", pp_salesmen$pp_sales_skills_index[no])
  p3 <- curve.func("curve44", curve.func("curve11", pp_salesmen$pp_sr_acc_revenue[no])) + pp_salesmen$pp_sales_skills_index[no]
  
  n1 <- paste0(0.2, " * (", p1, ") + ", 0.3, " * (", p2, ") + ", 0.5 * p3)
  n2 <- paste0("curve.func('curve26', i", no, ") + ", pp_salesmen$pp_product_knowledge_index[no])
  n3 <- paste0("(c", hosp, " - 2 * ", curve.func("curve12", pp_salesmen$pp_motivation_index[no]), " * ",c_prop, ") / 2.5")
  n4 <- paste0("curve.func('curve5', h) + ", pp_hospitals$pp_deployment_quality_index[hosp])
  n5 <- paste0("curve.func('curve7', f) + ", pp_hospitals$pp_deployment_quality_index[hosp])
  n6 <- paste0("curve.func('curve8', g) * ", pp_hospitals$pp_deployment_quality_index[hosp])
  
  m1 <- curve.func("curve32", pp_salesmen$pp_motivation_index[no]) + pp_hospitals$pp_sr_sales_performance[hosp]
  m2 <- paste0("curve.func('curve34', ", n1, ") + ", pp_hospitals$pp_sr_sales_performance[hosp])
  m3 <- paste0("curve.func('curve33', ", n2, ") + ", pp_hospitals$pp_sr_sales_performance[hosp])
  m4 <- paste0("curve.func('", w, "', ", n3, ") + ", pp_hospitals$pp_sr_sales_performance[hosp])
  m5 <- paste0(0.3, " * (", n4, ") + ", 0.3, " * (", n5, ") + ", 0.4, " * (", n6, ")")
  m6 <- paste0("curve.func('curve26', i", no, ")", " + ", pp_salesmen$pp_product_knowledge_index[no])
  m7 <- paste0("curve.func('curve30', ", 100 / pp_hospitals$pp_real_revenue[hosp], " * a", hosp, ")")
  
  
  l1 <- paste0(0.15 * m1, " + ", 0.3, " * (", m2, ") + ", 0.05, " * (", m3, ") + ", 0.5, " * (", m4, ")")
  l2 <- paste0("curve.func('curve40', e", no, " / 2)", " + ", pp_hospitals$pp_sales_performance[hosp])
  l3 <- paste0("curve.func('curve41', ", m5, ")", " + ", pp_hospitals$pp_sales_performance[hosp])
  l4 <- paste0("curve.func('curve2', ", m6, ")", " + ", curve.func("curve4", pp_hospitals$pp_customer_relationship_index[hosp]))
  l5 <- paste0("curve.func('curve3', ", m7, ")", " + ", curve.func("curve4", pp_hospitals$pp_customer_relationship_index[hosp]))
  
  k1 <- paste0(0.75, " * (", l1, ") + ", 0.05, " * (", l2, ") + ", 0.2, " * (", l3, ")")
  k2 <- paste0(0.35, " * (", l4, ") + ", 0.65, " * (", l5, ")")
  
  j1 <- paste0(0.6 * 100, " * (", "curve.func('curve25', ", k1, ")) + ", 0.4 * 100, " * (curve.func('curve19', ", k2, ")", ")")
  j2 <- pp_hospitals$pp_offer_attractiveness[hosp]
  
  assign(paste0("y", hosp), paste0(potentials$phrase_2[hosp] / 100, " * curve.func('curve51', ", 0.8, " * (", j1, ") + ", 0.2 * j2, ")"))
}

yy <- paste0("-(", paste(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, sep = " + "), ")")

##------------------------------------------------------------------------------
##--                 Solutions
##------------------------------------------------------------------------------

##----
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
            150, 150, 150, 150, 150, 150, 150, 150, 150, 150,
            15, 30, 30, 15, 15, 15, 15, 15),
  
  con = function(x) {
    a1 <- x[1]; a2 <- x[2]; a3 <- x[3]; a4 <- x[4]; a5 <- x[5]; a6 <- x[6]; a7 <- x[7]; a8 <- x[8]; a9 <- x[9]; a10 <- x[10]
    c1 <- x[11]; c2 <- x[12]; c3 <- x[13]; c4 <- x[14]; c5 <- x[15]; c6 <- x[16]; c7 <- x[17]; c8 <- x[18]; c9 <- x[19]; c10 <- x[20]
    d1 <- x[21]; d2 <- x[22]; d3 <- x[23]; d4 <- x[24]; d5 <- x[25]
    e1 <- x[26]; e2 <- x[27]; e3 <- x[28]; e4 <- x[29]; e5 <- x[30]
    f <- x[31]; g <- x[32]; h <- x[33]
    i1 <- x[34]; i2 <- x[35]; i3 <- x[36]; i4 <- x[37]; i5 <- x[38]
    
    c(a1 + a2 + a3 + a4 +a5 + a6 + a7 + a8 + a9 + a10 - 100,
      c1 + c9 + d1 + f + i1 - 100, c2 + c5 + d2 + f + i2 - 100, c3 + c7 + d3 + f + i3 - 100, c4 + c8 + d4 + f + i4 - 100, c6 + c10 + d5 + f + i5 - 100,
      e1 - c1 - c9, e2 - c2 - c5, e3 - c3 - c7, e4 - c4 - c8, e5 - c6 - c10,
      d1 + d2 + d3 + d4 + d5 + e1 + e2 + e3 + e4 + e5 + f + g + h - 100)
  },
  
  initial1 = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
               40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
               5, 5, 5, 5, 5,
               12, 12, 12, 12, 12,
               5, 5, 5,
               10, 10, 10, 10, 10),
  
  initial2 = c(14, 10, 18, 13, 2, 10, 5, 4, 3, 20,
               62, 56, 62, 66, 23, 18, 24, 13, 22, 70,
               7, 9, 5, 9, 4,
               13, 8, 7, 10, 7,
               7, 7, 7,
               2, 5, 2, 5, 1)
)

system.time(result <- JDEoptim(lower = fcn$lower,
                               upper = fcn$upper,
                               fn = fcn$obj,
                               constr = fcn$con,
                               eps = 0.1,
                               tol = 0.1,
                               maxiter = 1000000,
                               trace = TRUE,
                               triter = 100,
                               add_to_init_pop = fcn$initial2))

##----
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
            150, 150, 150, 150, 150, 150, 150, 150, 150, 150,
            15, 30, 30, 15, 15, 15, 15, 15),
  initial1 = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10,
               40, 40, 40, 40, 40, 40, 40, 40, 40, 40,
               5, 5, 5, 5, 5,
               12, 12, 12, 12, 12,
               5, 5, 5,
               10, 10, 10, 10, 10),
  initial2 = c(14, 10, 18, 13, 2, 10, 5, 4, 3, 20,
               62, 56, 62, 66, 23, 18, 24, 13, 22, 70,
               7, 9, 5, 9, 4,
               13, 8, 7, 10, 7,
               7, 7, 7,
               2, 5, 2, 5, 1)
)

system.time(result1 <-
              DEoptim(fn = fcn1$obj,
                      lower = fcn1$lower,
                      upper = fcn1$upper,
                      control = DEoptim.control(itermax = 10000000,
                                                trace = 1000,
                                                # initialpop = fcn1$initial1,
                                                c = 0.4,
                                                parallelType = 1,
                                                cluster = cl)))



















