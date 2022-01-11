##install all needed packages
library("dplyr")
library("ggpubr")

#use the robvis package as a new source. Some functions of the package are changed according to our review
source("robisfunctions.R")

#use getDataREDCap.R to get data from REDCap
source("getDataREDCap.R")

#formDatarob is from getDataREDCap script
response_rob <- httr::POST(urlrob, body = formDatarob, encode = "form")
rob_records <- httr::content(response_rob)


published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
#include additional study identified from ref list
additional <- 11099
rob_records <- rob_records %>%
  filter(record_id <= 5296 |
           record_id %in% published_preprints |
           record_id %in% additional)



#prepare a dataset

df_rob <- rob_records %>%
  select(record_id,author_1, quest___1, q3_sar, setting2, 
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
  select(author_1, rob_1, rob_2, rob_3, rob_4, rob_5, rob_6, record_id, quest___1, q3_sar, setting) %>%
  rename(Study = author_1, D1 = rob_1, D2 = rob_2, D3 = rob_3, D4 = rob_4,
         D5 = rob_5, `6` = rob_6)

q1_rob$D1 <- ifelse(q1_rob$D1 == 1, "High", ifelse(q1_rob$D1 == 2, "Unclear", "Low"))
q1_rob$D2 <- ifelse(q1_rob$D2 == 1, "High", ifelse(q1_rob$D2 == 2, "Unclear", "Low"))
q1_rob$D3 <- ifelse(q1_rob$D3 == 1, "High", ifelse(q1_rob$D3 == 2, "Unclear", "Low"))
q1_rob$D4 <- ifelse(q1_rob$D4 == 1, "High", ifelse(q1_rob$D4 == 2, "Unclear", "Low"))
q1_rob$D5 <- ifelse(q1_rob$D5 == 1, "High", ifelse(q1_rob$D5 == 2, "Unclear", "Low"))
q1_rob$`6` <- ifelse(q1_rob$`6` == 1, "High", ifelse(q1_rob$`6` == 2, "Unclear", "Low"))

####Plots filtered by settings#########

#1. "Contact investigation" and "Contact investigation, aggregated"
contactinvestigation <- q1_rob %>% 
  filter(setting == "Contact investigation" | setting == "Contact investigation, aggregated") %>%
  select(Study, D1, D2, D3, D4, D5, `6`)

contactinvestigation_rob_trafficlight <- rob_traffic_light(data = contactinvestigation,
                                                           tool = "Generic",
                                                           psize = 10,
                                                           overall = FALSE)

p1 <- contactinvestigation_rob_trafficlight +
  ggplot2::labs(
    caption = " ") + 
  ggplot2::ggtitle("Contact investigation") +
  ggplot2::theme(legend.position = "none")

# p1 <- annotate_figure(p1, top = text_grob("Question 1", 
#                                           color = "black", face = "bold", size = 21))
dev.new()
tiff("test.tiff", width = 600, height = 400)
#pdf("q1_1_new.pdf", width = 1000, height = 1500)
p1
graphics.off()



# 3. "Outbreak investigation"
outbreakinv <- q1_rob %>% 
  filter(setting == "Outbreak investigation") %>%
  select(Study, D1, D2, D3, D4, D5, `6`)

outbreakinv_rob_trafficlight <- rob_traffic_light(data = outbreakinv,
                                                  tool = "Generic",
                                                  psize = 10,
                                                  overall = FALSE)
p2 <- outbreakinv_rob_trafficlight +
  ggplot2::labs(
    caption = " ") + 
  ggplot2::ggtitle("Outbreak investigation") +
  ggplot2::theme(legend.position = "none")

#p3 <- ggarrange(p1,p2,heights = c(1, 1.7), ncol = 1, nrow = 2)


dev.new()
tiff("test2.tiff", width = 600, height = 900)
p2
graphics.off()


# 9. "Screening: institutional setting"
institutional <- q1_rob %>% 
  filter(setting == "Screening: institutional setting") %>%
  select(Study, D1, D2, D3, D4, D5, `6`)
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

dev.new()
tiff("test3.tiff", width = 600, height = 710)
p4
graphics.off()



# 10. "Screening: community setting"
communityset <- q1_rob %>% 
  filter(setting == "Screening: community setting") %>%
  select(Study, D1, D2, D3, D4, D5, `6`)
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

dev.new()
tiff("test4.tiff", width = 600, height = 520)
p5
graphics.off()


# 11. "Screening: occupational"
occupationalset <- q1_rob %>% 
  filter(setting == "Screening: occupational") %>%
  select(Study, D1, D2, D3, D4, D5, `6`)
#Records: 12 studies

occupationalset_rob_trafficlight <- rob_traffic_light(data = occupationalset,
                                                      tool = "Generic",
                                                      psize = 10,
                                                      overall = FALSE)


p6 <- occupationalset_rob_trafficlight +
  ggplot2::labs(
    caption = "
    Domains

    Selection Bias:
  1: Representativeness of the sample
  2: Characteristics of non-respondents
    Information Bias:
  3: Symptom assessment
  4: Recording of symptoms
    Misclassification bias:
  5: Classification of asymptomatic status
    Attrition bias:
  6: Selective reporting of symptoms status")

p6 <- p6 + 
  ggplot2::ggtitle("Screening: occupational")


#p7 <- ggarrange(p4,p5,p6, heights = c(1, 0.8, 0.8), ncol = 1, nrow = 3)

dev.new()
tiff("test5.tiff", width = 600, height = 620)
p6
graphics.off()


###ROB analysis for Q2.1
q2_rob <- df_rob %>%
  filter(q3_sar == 1)  %>%
  select(author_1, rob_1, rob_2, rob_3, rob_4, rob_5, rob_6) %>%
  rename(Study = author_1, D1 = rob_1, D2 = rob_2, D3 = rob_3, D4 = rob_4,
         D5 = rob_5, `6` = rob_6)

q2_rob$D1 <- ifelse(q2_rob$D1 == 1, "High", ifelse(q2_rob$D1 == 2, "Unclear", "Low"))
q2_rob$D2 <- ifelse(q2_rob$D2 == 1, "High", ifelse(q2_rob$D2 == 2, "Unclear", "Low"))
q2_rob$D3 <- ifelse(q2_rob$D3 == 1, "High", ifelse(q2_rob$D3 == 2, "Unclear", "Low"))
q2_rob$D4 <- ifelse(q2_rob$D4 == 1, "High", ifelse(q2_rob$D4 == 2, "Unclear", "Low"))
q2_rob$D5 <- ifelse(q2_rob$D5 == 1, "High", ifelse(q2_rob$D5 == 2, "Unclear", "Low"))
q2_rob$`6` <- ifelse(q2_rob$`6` == 1, "High", ifelse(q2_rob$`6` == 2, "Unclear", "Low"))

q2_plot_ds <- q2_rob %>% 
  select(Study, D1, D2, D3, D4, D5, `6`)

q2_rob_trafficlight <- rob_traffic_light(data = q2_plot_ds,
                                         tool = "Generic",
                                         psize = 10,
                                         overall = FALSE)


p8 <- q2_rob_trafficlight +
  ggplot2::labs(
    caption = "
    Domains

    Selection Bias:
  1: Representativeness of the sample
  2: Characteristics of non-respondents
    Information Bias:
  3: Symptom assessment
  4: Recording of symptoms
    Misclassification bias:
  5: Classification of asymptomatic status
    Attrition bias:
  6: Selective reporting of symptoms status")

p8 <- p8 + 
  ggplot2::ggtitle("Question 2.1") +
  theme(plot.title = element_text(hjust = 0.5)) + theme(plot.title = element_text(size = 20, face = "bold"))

dev.new()
tiff("test6.tiff", width = 600, height = 420)
p8
graphics.off()

