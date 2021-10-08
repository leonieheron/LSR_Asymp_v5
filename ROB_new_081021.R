##################################################
# Risk of bias visualisation                     #  
##################################################
rm(list=ls())

#download package
#install.packages("devtools")
#devtools::install_github("mcguinlu/robvis")
library(dplyr)
install.packages("ggpubr")
library("ggpubr")

#use the package as a new source. Some functions of the package are changed by HI
source("robisfunctions.R")

# get the data directly from redcap:
urlrob <- "https://redcap.ispm.unibe.ch/api/"
tokenrob <- "F2725F15FE84D2832E2793BB23B0A62B"
formDatarob <- list("token"=tokenrob,
                    content='report',
                    format='csv',
                    report_id='283',
                    csvDelimiter='',
                    rawOrLabel='raw',
                    rawOrLabelHeaders='raw',
                    exportCheckboxLabel='false',
                    returnFormat='csv'
)
response_rob <- httr::POST(urlrob, body = formDatarob, encode = "form")
rob_records <- httr::content(response_rob)

rob_records$authoryear <- paste0(rob_records$author_1, " (", rob_records$year, ")")
rob_records <- rob_records[rob_records$record_id < "5566", ]

df_rob <- rob_records %>%
  select(record_id,author_1, authoryear,quest___1, q3_sar, setting2, 
         rob_1, rob_2, rob_3, rob_4, rob_5, rob_6, 
         risk_of_bias_update_3_complete)
df_rob['setting'] <- NA


df_rob <- df_rob %>%
  mutate(setting = ifelse(setting2 == 1, "Contact investigation", setting),
         setting = ifelse(setting2 == 2, "Contact investigation, aggregated", setting),
         setting = ifelse(setting2 == 3, "Outbreak investigation", setting),
         setting = ifelse(setting2 == 4, "Statistical model", setting),
         setting = ifelse(setting2 == 5, "Screening", setting),
         setting = ifelse(setting2 == 6, "Hospitalised adults", setting),
         setting = ifelse(setting2 == 7, "Hospitalised children", setting),
         setting = ifelse(setting2 == 8, "Hospitalised children and adults", setting),
         setting = ifelse(setting2 == 9, "Screening: institutional setting", setting),
         setting = ifelse(setting2 == 10, "Screening: community setting", setting),
         setting = ifelse(setting2 == 11, "Screening: occupational", setting))

#Filtered by Q1
q1_rob <- df_rob %>%
  filter(!is.na(rob_1)) %>%
  filter(record_id != 122) %>%
  filter(quest___1 == 1)  %>%
  select(authoryear, rob_1, rob_2, rob_3, rob_4, rob_5, rob_6, record_id, quest___1, q3_sar, setting) %>%
  rename(Study = authoryear, D1 = rob_1, D2 = rob_2, D3 = rob_3, D4 = rob_4,
         D5 = rob_5, D6 = rob_6)


q1_rob$D1 <- ifelse(q1_rob$D1 == 1, "High", ifelse(q1_rob$D1 == 2, "Unclear", "Low"))
q1_rob$D2 <- ifelse(q1_rob$D2 == 1, "High", ifelse(q1_rob$D2 == 2, "Unclear", "Low"))
q1_rob$D3 <- ifelse(q1_rob$D3 == 1, "High", ifelse(q1_rob$D3 == 2, "Unclear", "Low"))
q1_rob$D4 <- ifelse(q1_rob$D4 == 1, "High", ifelse(q1_rob$D4 == 2, "Unclear", "Low"))
q1_rob$D5 <- ifelse(q1_rob$D5 == 1, "High", ifelse(q1_rob$D5 == 2, "Unclear", "Low"))
q1_rob$D6 <- ifelse(q1_rob$D6 == 1, "High", ifelse(q1_rob$D6 == 2, "Unclear", "Low"))

####Plots filtered by settings#########

#1. "Contact investigation" and Contact investigation, aggregated
contactinvestigation <- q1_rob %>% 
  filter(setting == "Contact investigation" | setting == "Contact investigation, aggregated") %>%
  select(Study, D1, D2, D3, D4, D5, D6)
#Result: 9 records included for this

contactinvestigation_rob_trafficlight <- rob_traffic_light(data = contactinvestigation,
                                                           tool = "Generic",
                                                           psize = 10,
                                                           overall = FALSE)

p1 <- contactinvestigation_rob_trafficlight +
  ggplot2::labs(
    caption = " ") + 
  ggplot2::ggtitle("Contact investigation") +
  ggplot2::theme(legend.position = "none")


# 3. "Outbreak investigation"
outbreakinv <- q1_rob %>% 
  filter(setting == "Outbreak investigation") %>%
  select(Study, D1, D2, D3, D4, D5, D6)
#Records: 23 for outbreak investigation

outbreakinv_rob_trafficlight <- rob_traffic_light(data = outbreakinv,
                                                  tool = "Generic",
                                                  psize = 10,
                                                  overall = FALSE)


p2 <- outbreakinv_rob_trafficlight +
  ggplot2::labs(
    caption = "  
    Domains:
  D1: Representativeness of the sample (selection bias)
  D2: Characteristics of non-respondents (selection bias)
  D3: Symptom assessment (information bias)
  D4: Recording of symptoms (information bias)
  D5: Classification of asymptomatic status (misclassification bias)
  D6: Selective reporting of symptoms status (attrition bias)")

p2 <- p2 + 
  ggplot2::ggtitle("Outbreak investigation")

p3 <- ggarrange(p1,p2,heights = c(1, 1.7), ncol = 1, nrow = 2)
#If anybody wants to Export it (plot p3) as a png pls use 900x1500 dimensions



#####For now there are no records related to below settings (until 8)#######
# 4. "Statistical model"
statsisticalmodel <- q1_rob %>%
  filter(setting == "Statistical model") %>%
  select(Study, D1, D2, D3, D4, D5, D6,setting)


# 5. "Screening"
screeningall <- q1_rob %>%
  filter(setting == "Screening") %>%
  select(Study, D1, D2, D3, D4, D5, D6)

# 6. "Hospitalised adults"
hospitalisedadults <- q1_rob %>% 
  filter(setting == "Hospitalised adults") %>%
  select(Study, D1, D2, D3, D4, D5, D6,setting)

# 7. "Hospitalised children"
hospitalisedchildren <- q1_rob %>% 
  filter(setting == "Hospitalised children") %>%
  select(Study, D1, D2, D3, D4, D5, D6,setting)

# 8. "Hospitalised children and adults"
hospitalisedchildrenandadults <- q1_rob %>% 
  filter(setting == "Hospitalised children and adults") %>%
  select(Study, D1, D2, D3, D4, D5, D6,setting)

######################################################################

# 9. "Screening: institutional setting"
institutional <- q1_rob %>% 
  filter(setting == "Screening: institutional setting") %>%
  select(Study, D1, D2, D3, D4, D5, D6)
#Records: 21 studies

institutional_rob_trafficlight <- rob_traffic_light(data = institutional,
                                                    tool = "Generic",
                                                    psize = 10,
                                                    overall = FALSE)
p4 <- institutional_rob_trafficlight +
  ggplot2::labs(
    caption = " ") + 
  ggplot2::ggtitle("Screening: institutional setting") +
  ggplot2::theme(legend.position = "none")

# 10. "Screening: community setting"
communityset <- q1_rob %>% 
  filter(setting == "Screening: community setting") %>%
  select(Study, D1, D2, D3, D4, D5, D6)
#Records: 17 studies

communityset_rob_trafficlight <- rob_traffic_light(data = communityset,
                                                   tool = "Generic",
                                                   psize = 10,
                                                   overall = FALSE)

p5 <- communityset_rob_trafficlight +
  ggplot2::labs(
    caption = " ") + 
  ggplot2::ggtitle("Screening: community setting") +
  ggplot2::theme(legend.position = "none")


# 11. "Screening: occupational"
occupationalset <- q1_rob %>% 
  filter(setting == "Screening: occupational") %>%
  select(Study, D1, D2, D3, D4, D5, D6)
#Records: 12 studies

occupationalset_rob_trafficlight <- rob_traffic_light(data = occupationalset,
                                                      tool = "Generic",
                                                      psize = 10,
                                                      overall = FALSE)


p6 <- occupationalset_rob_trafficlight +
  ggplot2::labs(
    caption = "
    Domains:
  D1: Representativeness of the sample (selection bias)
  D2: Characteristics of non-respondents (selection bias)
  D3: Symptom assessment (information bias)
  D4: Recording of symptoms (information bias)
  D5: Classification of asymptomatic status (misclassification bias)
  D6: Selective reporting of symptoms status (attrition bias)")

p6 <- p6 + 
  ggplot2::ggtitle("Screening: occupational")


p7 <- ggarrange(p4,p5,p6, heights = c(1.1, 1, 1), ncol = 1, nrow = 3)

#If anybody wants to Export it (plot p7) as a png pls use 1400 x 2200 dimensions

###ROB analysis for Q2.1
q2_rob <- df_rob %>%
  filter(!is.na(rob_1)) %>%
  filter(record_id != 122) %>%
  filter(q3_sar == 1)  %>%
  select(authoryear, rob_1, rob_2, rob_3, rob_4, rob_5, rob_6) %>%
  rename(Study = authoryear, D1 = rob_1, D2 = rob_2, D3 = rob_3, D4 = rob_4,
         D5 = rob_5, D6 = rob_6)

q2_rob$D1 <- ifelse(q2_rob$D1 == 1, "High", ifelse(q2_rob$D1 == 2, "Unclear", "Low"))
q2_rob$D2 <- ifelse(q2_rob$D2 == 1, "High", ifelse(q2_rob$D2 == 2, "Unclear", "Low"))
q2_rob$D3 <- ifelse(q2_rob$D3 == 1, "High", ifelse(q2_rob$D3 == 2, "Unclear", "Low"))
q2_rob$D4 <- ifelse(q2_rob$D4 == 1, "High", ifelse(q2_rob$D4 == 2, "Unclear", "Low"))
q2_rob$D5 <- ifelse(q2_rob$D5 == 1, "High", ifelse(q2_rob$D5 == 2, "Unclear", "Low"))
q2_rob$D6 <- ifelse(q2_rob$D6 == 1, "High", ifelse(q2_rob$D6 == 2, "Unclear", "Low"))

q2_plot_ds <- q2_rob %>% 
  select(Study, D1, D2, D3, D4, D5, D6)

q2_rob_trafficlight <- rob_traffic_light(data = q2_plot_ds,
                                                    tool = "Generic",
                                                    psize = 10,
                                                    overall = FALSE)


p8 <- q2_rob_trafficlight +
  ggplot2::labs(
    caption = "
    Domains:
  D1: Representativeness of the sample (selection bias)
  D2: Characteristics of non-respondents (selection bias)
  D3: Symptom assessment (information bias)
  D4: Recording of symptoms (information bias)
  D5: Classification of asymptomatic status (misclassification bias)
  D6: Selective reporting of symptoms status (attrition bias)")

p8 <- p8 + 
  ggplot2::ggtitle("Question 2.1")

#dimension 700x400