# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# ProjectName:  TM
# Purpose:      TM test
# programmer:   Zhe Liu
# Date:         12-05-2018
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
})

##------------------------------------------------------------------------------
##--                 Loading database
##------------------------------------------------------------------------------

##-- report
report.org <- mongo(collection = "report", db = "TMIST_New",
                    url = options()$mongodb$host,
                    options = ssl_options())

report.user <- list()
for (i in c(1:6, 8)) {
  
  report.user[[i]] <- report.org$find(query = paste('{"user_id" : "', "course_user", i,'"}', sep = ""), fields = '{}')
}

##-- assessment
assessment.org <- mongo(collection = "assessment", db = "TMIST_New",
                        url = options()$mongodb$host,
                        options = ssl_options())

assessment.user <- list()
for (i in c(1:6, 8)) {
  
  assessment.user[[i]] <- assessment.org$find(query = paste('{"user_id" : "', "course_user", i,'"}', sep = ""), fields = '{}')
}

##-- inputs
inputs.org <- mongo(collection = "inputs", db = "TMIST_New",
                    url = options()$mongodb$host,
                    options = ssl_options())

inputs.user <- list()
for (i in c(1:6, 8)) {
  
  inputs.user[[i]] <- inputs.org$find(query = paste('{"uuid" : "', report.user[[i]][["uuid"]],'"}', sep = ""), fields = '{}')
}

##------------------------------------------------------------------------------
##--                 Extraction
##------------------------------------------------------------------------------

##-- decision
inputs.decision <- list()
for (i in c(1:6, 8)) {
  
  v <- c()
  for (j in 11:20) {
    
    v[j-10] <- inputs.user[[i]][["decision"]][[1]][["sales"]][[j]]$prod_value
  }
  
  h <- c()
  for (j in 11:20) {
    
    h[j-10] <- inputs.user[[i]][["decision"]][[1]][["visit_hours"]][[j]]$prod_hours
  }
  
  inputs.decision[[i]] <- list()
  inputs.decision[[i]][["user_id"]] = inputs.user[[i]][["user_id"]]
  inputs.decision[[i]][["uuid"]] = inputs.user[[i]][["uuid"]]
  inputs.decision[[i]][["decision"]] <- data.frame(user_id = inputs.user[[i]][["user_id"]],
                                                   uuid = inputs.user[[i]][["uuid"]],
                                                   hosp_code = inputs.user[[i]][["decision"]][[1]][["hosp_code"]][11:20],
                                                   hosp_name = inputs.user[[i]][["decision"]][[1]][["hosp_name"]][11:20],
                                                   budget = inputs.user[[i]][["decision"]][[1]][["budget"]][11:20],
                                                   salesmen = inputs.user[[i]][["decision"]][[1]][["salesmen"]][11:20],
                                                   prod_name = rep(inputs.user[[i]][["decision"]][[1]][["sales"]][[11]][["prod_name"]], times = 10),
                                                   prod_value = v,
                                                   visit_hours = h)
}

##-- management
inputs.management <- list()
for (i in c(1:6, 8)) {
  
  n <- list()
  
  for (j in 1:5) {
    
    n[[j]] <- c(inputs.user[[i]][["management"]][[1]][["apply"]][[7]][["days"]][j],
                inputs.user[[i]][["management"]][[1]][["apply"]][[8]][["days"]][j],
                inputs.user[[i]][["management"]][[1]][["apply"]][[9]][["days"]],
                0,
                0,
                inputs.user[[i]][["management"]][[1]][["apply"]][[12]][["days"]][j])
  }
  n[[6]] <- c(sum(inputs.user[[i]][["management"]][[1]][["apply"]][[7]][["days"]]),
              sum(inputs.user[[i]][["management"]][[1]][["apply"]][[8]][["days"]]),
              inputs.user[[i]][["management"]][[1]][["apply"]][[9]][["days"]],
              inputs.user[[i]][["management"]][[1]][["apply"]][[10]][["days"]],
              inputs.user[[i]][["management"]][[1]][["apply"]][[11]][["days"]],
              0)
  
  inputs.management[[i]] <- list()
  inputs.management[[i]][["user_id"]] = inputs.user[[i]][["user_id"]]
  inputs.management[[i]][["uuid"]] = inputs.user[[i]][["uuid"]]
  inputs.management[[i]][["management"]] <- data.frame(user_id = inputs.user[[i]][["user_id"]],
                                                       uuid = inputs.user[[i]][["uuid"]],
                                                       project_code = inputs.user[[i]][["management"]][[1]][["project_code"]][7:12],
                                                       project_name = inputs.user[[i]][["management"]][[1]][["project_name"]][7:12],
                                                       小宋 = n[[1]],
                                                       小兰 = n[[2]],
                                                       小木 = n[[3]],
                                                       小白 = n[[4]],
                                                       小青 = n[[5]],
                                                       经理 = n[[6]])
}

##-- report
report <- list()
for (i in c(1:6, 8)) {
  
  report[[i]] <- list()
  
  report[[i]][["user_id"]] <- report.user[[i]][["user_id"]]
  report[[i]][["uuid"]] <- report.user[[i]][["uuid"]]
  report[[i]][["report_name"]] <- report.user[[i]][["report"]][[1]][["report_name"]]
  report[[i]][["report"]] <- list()
  
  for (j in 1:13) {
    
    report[[i]][["report"]][[j]] <- report.user[[i]][["report"]][[1]][["result"]][[j]]
  }
  
  report[[i]][["rep_change"]] <- bind_rows(report.user[[i]][["report"]][[1]][["result"]][[4]],
                                           report.user[[i]][["report"]][[1]][["result"]][[5]],
                                           report.user[[i]][["report"]][[1]][["result"]][[6]],
                                           report.user[[i]][["report"]][[1]][["result"]][[7]])
}

##-- assessment
assessment <- list()
for (i in c(1:6, 8)) {
  
  assessment[[i]] <- list()
  
  assessment[[i]][["user_id"]] <- assessment.user[[i]][["user_id"]]
  assessment[[i]][["uuid"]] <- assessment.user[[i]][["uuid"]]
  assessment[[i]][["overall_score"]] <- assessment.user[[i]][["result"]][["phase_2"]][["overall_score"]]
  assessment[[i]][["assess_results"]] <- assessment.user[[i]][["result"]][["phase_2"]][["assess_results"]][[1]] %>%
    mutate(overall_score = assessment.user[[i]][["result"]][["phase_2"]][["overall_score"]])
  assessment[[i]][["final_revenue_info"]] <- assessment.user[[i]][["result"]][["phase_2"]][["final_revenue_info"]]
  assessment[[i]][["achievement_info"]] <- assessment.user[[i]][["result"]][["phase_2"]][["achievement_info"]][[1]]
  assessment[[i]][["market_share_info"]] <- assessment.user[[i]][["result"]][["phase_2"]][["market_share_info"]][[1]]
  assessment[[i]][["team_ability_info"]] <- assessment.user[[i]][["result"]][["phase_2"]][["team_ability_info"]]
  
  assessment[[i]][["index_change"]] <- rbind(t(assessment.user[[i]][["result"]][["phase_2"]][["final_revenue_info"]]),
                                             t(assessment.user[[i]][["result"]][["phase_2"]][["achievement_info"]][[1]][3:5]),
                                             t(assessment.user[[i]][["result"]][["phase_2"]][["market_share_info"]][[1]][3:5]),
                                             t(assessment.user[[i]][["result"]][["phase_2"]][["team_ability_info"]])) %>%
    rename("1")
}

##-- output
for (i in c(1:6, 8)) {
  
  wb <- createWorkbook()
  
  addWorksheet(wb, "input.decision")
  addWorksheet(wb, "input.management")
  addWorksheet(wb, "市场销售报告_商业价值")
  addWorksheet(wb, "市场销售报告_销售业绩")
  addWorksheet(wb, "代表报告_时间分配")
  addWorksheet(wb, "代表报告_指标变化")      #
  addWorksheet(wb, "经理报告_职员成本")
  addWorksheet(wb, "经理报告_时间分配")
  addWorksheet(wb, "分配报告_资源分配")
  addWorksheet(wb, "销售报告_销售额每客户")
  addWorksheet(wb, "销售报告_销售额每代表")
  addWorksheet(wb, "销售报告_销售额每产品")
  addWorksheet(wb, "assess_results")
  addWorksheet(wb, "index_change")

  writeData(wb, "input.decision", inputs.decision[[i]][["decision"]])
  writeData(wb, "input.management", inputs.management[[i]][["management"]])
  writeData(wb, "市场销售报告_商业价值", report[[i]][["report"]][[1]])
  writeData(wb, "市场销售报告_销售业绩", report[[i]][["report"]][[2]])
  writeData(wb, "代表报告_时间分配", report[[i]][["report"]][[3]])
  writeData(wb, "代表报告_指标变化", report[[i]][["rep_change"]])
  writeData(wb, "经理报告_职员成本", report[[i]][["report"]][[8]])
  writeData(wb, "经理报告_时间分配", report[[i]][["report"]][[9]])
  writeData(wb, "分配报告_资源分配", report[[i]][["report"]][[10]])
  writeData(wb, "销售报告_销售额每客户", report[[i]][["report"]][[11]])
  writeData(wb, "销售报告_销售额每代表", report[[i]][["report"]][[12]])
  writeData(wb, "销售报告_销售额每产品", report[[i]][["report"]][[13]])
  writeData(wb, "assess_results", assessment[[i]][["assess_results"]])
  writeData(wb, "index_change", assessment[[i]][["index_change"]], rowNames = TRUE, colNames = FALSE)
  
  saveWorkbook(wb, paste("test_results/course_user", i, ".xlsx", sep = ""), overwrite = TRUE)
}

for (i in c(1:6, 8)) {
  print(assessment[[i]][["overall_score"]])
}
















