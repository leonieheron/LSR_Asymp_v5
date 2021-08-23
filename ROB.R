##################################################
# Risk of bias visualisation                     #  
##################################################
rm(list=ls())

#download package
install.packages("devtools")
devtools::install_github("mcguinlu/robvis")


rob_data <- robvis::data_rob2
rob_data$D6 <- rob_data$Overall
rob_data$Overall <- NULL
rob_data

#summary table giving traffic lights for each domain in each study
trafficlight_rob <- robvis::rob_traffic_light(data = rob_data,
                                      tool = "Generic",
                                      psize = 10,
                                      overall = FALSE)

trafficlight_rob +
  ggplot2::labs(
    caption = "  Domains:
  D1: Representativeness of the sample (selection bias)
  D2: Characteristics of non-respondents (selection bias)
  D3: Symptom assessment (information bias)
  D4: Recording of symptoms (information bias)
  D5: Classification of asymptomatic status (misclassification bias)
  D6: Selective reporting of symptoms status (attrition bias)
                "
  ) 


#summary plot giving an overview of risk of bias from all studies

names(rob_data) <- c("Representativeness of the sample (selection bias)",
                     "D2: Characteristics of non-respondents (selection bias)",
                     "D3: Symptom assessment (information bias)",
                     "D4: Recording of symptoms (information bias)",
                     "D5: Classification of asymptomatic status (misclassification bias)",
                     "D6: Selective reporting of symptoms status (attrition bias)")
summary_rob <- robvis::rob_summary(data = rob_data, tool = "ROB2")

summary_rob
###make changes to ggplot to relabel domains