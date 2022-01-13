
#download packages
library(meta)
library(readxl)
library(tidyverse)
library(httr) # use to retrieve data from REDCap
library(kableExtra)
library(flextable)
library(dplyr)
library(RCurl)
library(tidyr)
library(ggplot2)
library(metafor)


#download data
# get the data directly from redcap:
# report #155 is Q1:
url <- "https://redcap.ispm.unibe.ch/api/"
token <- "F2725F15FE84D2832E2793BB23B0A62B"
formData <- list("token"=token,
                 content='report',
                 format='csv',
                 report_id='155',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")

##########################################
# Q1 forest plot
##########################################

asymptomaticQ1 <- httr::content(response)


settings=c("Contact investigation",
           "Contact investigation, aggregated",
           "Outbreak investigation",
           "Statistical model",
           "Screening",
           "Hospitalised adults",
           "Hospitalised children",
           "Hospitalised adults & children",
           "Screening: institutional setting",
           "Screening: community setting",
           "Screening: occupational")


asymptomaticQ1$setting2<-factor(asymptomaticQ1$setting2, levels=1:11, labels=settings)
#combine contact investigation and contact investigation aggregated
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Contact investigation, aggregated"] <- "Contact investigation"

# minor cleaning
asymptomaticQ1[asymptomaticQ1==9999]=NA


#regroup settings
asymptomaticQ1$setting2 <- as.character(asymptomaticQ1$setting2)
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Contact investigation"] <- "Contact and outbreak investigations"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Outbreak investigation"] <- "Contact and outbreak investigations"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Screening: community setting"] <- "Screening"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Screening: institutional setting"] <- "Screening"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Screening: occupational"] <- "Screening"

asymptomaticQ1$setting = asymptomaticQ1$setting2

  


data_long1 <- gather(asymptomaticQ1, cluster, total, c(q1_c1_total,q1_c2_total,q1_c3_total), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(record_id, author_1, setting, total, id, cluster)
data_long2 <-gather(asymptomaticQ1, cluster, events, c(q1_c1_event,q1_c2_event,q1_c3_event), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(events, id)

data_Q1 = merge(data_long1, data_long2, by="id")
data_Q1 = data_Q1[!is.na(data_Q1$total),]
data_Q1$cluster=factor(data_Q1$cluster,labels=c("1","2","3"),levels=c("q1_c1_total","q1_c2_total","q1_c3_total"))

data_Q1[data_Q1$record_id %in% asymptomaticQ1[is.na(asymptomaticQ1$q1_c2_event),]$record_id,]$cluster=NA

asymptomaticQ1=data_Q1

asymptomaticQ1$label=paste0(asymptomaticQ1$author_1, " (2020)",ifelse(!is.na(asymptomaticQ1$cluster),paste0(" cluster:",asymptomaticQ1$cluster),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

asymptomaticQ1$label=paste0(asymptomaticQ1$author_1,ifelse(!is.na(asymptomaticQ1$cluster),paste0(" [cluster:",asymptomaticQ1$cluster,"]"),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

#change clusters to descriptions
asymptomaticQ1$label[asymptomaticQ1$label == "Harada S [cluster:1]"] <- "Harada S [Patients]"
asymptomaticQ1$label[asymptomaticQ1$label == "Harada S [cluster:2]"] <- "Harada S [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "Kennelly SP [cluster:1]"] <- "Kennelly SP [Nursing home residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Kennelly SP [cluster:2]"] <- "Kennelly SP [Nursing home staff]"
asymptomaticQ1$label[asymptomaticQ1$label == "van Buul LW [cluster:1]"] <- "van Buul LW [Nursing home residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "van Buul LW [cluster:2]"] <- "van Buul LW [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "Theuring S [cluster:1]"] <- "Theuring S [School students and staff]"
asymptomaticQ1$label[asymptomaticQ1$label == "Theuring S [cluster:2]"] <- "Theuring S [Household members]"
asymptomaticQ1$label[asymptomaticQ1$label == "van den Besselaar JH [cluster:1]"] <- "van den Besselaar JH [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "van den Besselaar JH [cluster:2]"] <- "van den Besselaar JH [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Taylor J [cluster:1]"] <- "Taylor J [Healthcare personnel]"
asymptomaticQ1$label[asymptomaticQ1$label == "Taylor J [cluster:2]"] <- "Taylor J [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Ladhani SN [cluster:1]"] <- "Ladhani SN [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Ladhani SN [cluster:2]"] <- "Ladhani SN [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "#5551 Vohra [cluster:1]"] <- "#5551 Vohra [Presurgical patients]"
asymptomaticQ1$label[asymptomaticQ1$label == "#5551 Vohra [cluster:2]"] <- "#5551 Vohra [Undergoing chemotherapy]"
asymptomaticQ1$label[asymptomaticQ1$label == "#6526 Garibaldi [cluster:1]"] <- "#6526 Garibaldi [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "#6526 Garibaldi [cluster:2]"] <- "#6526 Garibaldi [Staff]"



data=asymptomaticQ1[order(asymptomaticQ1$setting,1/(1/asymptomaticQ1$events+1/(asymptomaticQ1$total-asymptomaticQ1$events))),]

data[is.na(data$setting),]$record_id
data=data[!is.na(data$setting),]

#run forest plot with all records

asym_plot<-metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    print.byvar = FALSE,
                    comb.fixed = FALSE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

tiff(filename = "Q1_update5.tiff",
     width = 4000, height = 12000,
     res = 400)
forest(asym_plot, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 


#cut data at end of jan 31

#published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
# data <- data %>%
#   filter(record_id <= 5296 | record_id %in% published_preprints)

#10.12.21 - updated HI
published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
#include additional study identified from ref list
additional <- 11099
data <- data %>%
  filter(record_id <= 5296 | 
           record_id %in% published_preprints | 
           record_id %in% additional)

#calculate median and iqr of all estimates of proportion
data <- data %>%
  mutate(prop = events/total)
data %>% filter(setting == "Screening") %>%
  summary()
data %>% filter(setting == "Contact and outbreak investigations") %>%
  summary()
summary(data$prop)


#=======
#  filter(record_id <= 5296 |
#           record_id %in% published_preprints |
#           record_id %in% additional)
#>>>>>>> d75025fce8ca3dac3ed1954e753400e950a265d1


asym_plot<-metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    print.byvar = FALSE,
                    comb.fixed = FALSE,
                    overall.hetstat = FALSE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

tiff(filename = "Q1.tiff",
     width = 4000, height = 9500,
     res = 400)
forest(asym_plot, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       overall.hetstat = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 

pdf("Q1.pdf", width = 10, height = 24)
forest(asym_plot, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       overall.hetstat = FALSE,
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 
 
####
#Q1 - all studies, not in subgroups
asym_plot<-metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    prediction = TRUE,
                    print.byvar = FALSE,
                    overall.hetstat = FALSE,
                    comb.random = TRUE, comb.fixed = FALSE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

tiff(filename = "Q1_notstratified.tiff",
     width = 4000, height = 9000,
     res = 400)
forest(asym_plot, sortvar = total, #sorted by study precision
       col.square = "darkblue",
       overall.hetstat = FALSE,
       just="left", colgap.studlab="1cm",
       predict=TRUE, comb.random = TRUE, comb.fixed = FALSE)
dev.off() 
pdf("Q1_notstratified.pdf", width = 10, height = 23)
forest(asym_plot, sortvar = total, #sorted by study precision
       col.square = "darkblue",
       overall.hetstat = FALSE,
       just="left", colgap.studlab="1cm",
       predict=TRUE, comb.random = TRUE, comb.fixed = FALSE)
dev.off() 

#####
#subgroup analysis for rob


#download data
# get the data directly from redcap:
# report #155 is Q1:
url <- "https://redcap.ispm.unibe.ch/api/"
token <- "F2725F15FE84D2832E2793BB23B0A62B"
formData <- list("token"=token,
                 content='report',
                 format='csv',
                 report_id='155',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")

#studies removed at high risk of selection bias
#studies 443, 597, 1960, 2802, 2907, 3921, 5068, 5238, 170, 2987, 5086, 1225, 4880
high_risk_select_bias <-c(443, 597, 1960, 2802, 2907, 3921, 5068, 5238, 170, 2987, 5086, 1225, 4880)
data_selectionbias <- data %>%
  filter(!record_id %in% high_risk_select_bias)
data_selectionbias <- data %>%
  

#run metaprop and forest plot


asym_plot_select_bias<-metaprop(events,total,data=data_selectionbias,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    overall.hetstat = FALSE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot_select_bias

tiff(filename = "Q1_select_bias.tiff",
     width = 3000, height = 6300,
     res = 300)
forest(asym_plot_select_bias, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       overall.hetstat = FALSE,
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

pdf(filename = "Q1_select_bias.tiff")
forest(asym_plot_select_bias, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       overall.hetstat = FALSE,
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 


#subgroup analyses by ROB assessments

#download rob data

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
rob_records <- rob_records %>%
  select(1:11) %>%
  filter(risk_of_bias_update_3_complete == 2)

published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
#include additional study identified from ref list
additional <- 11099
rob_records <- rob_records %>%
  filter(record_id <= 5296 | 
           record_id %in% published_preprints | 
           record_id %in% additional)

#merge with dataset
data_rob <- left_join(data, rob_records, by = "record_id")
names(data_rob)


#1. selection bias - stratify by rob assessment for representativeness
#and characteristics of non-respondents
#prepare data
data_rob_1_screen <- data_rob %>%
  filter(setting == "Screening") %>%
  mutate(rob_select = ifelse(rob_1 == 3 & rob_2 == 3, "Low risk of selection bias", "Unclear or high risk of selection bias"))
data_rob_1_contact <- data_rob %>%
  filter(setting == "Contact and outbreak investigations") %>%
  mutate(rob_select = ifelse(rob_1 == 3 & rob_2 == 3, "Low risk of selection bias", "Unclear or high risk of selection bias"))

#conduct meta-analysis - screening studies

asym_plot_rob_1_screen<-metaprop(events,total,data=data_rob_1_screen,sm = "PLOGIT", 
                          studlab=label, byvar=rob_select, prediction = TRUE, 
                          print.byvar = FALSE, comb.fixed = FALSE,
                          overall.hetstat = FALSE,
                          control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_1_screen

tiff(filename = "Q1_rob_select_screen.tiff",
     width = 2700, height = 4500,
     res = 300)
forest(asym_plot_rob_1_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       overall.hetstat = FALSE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_select_screen.pdf", height = 15, width = 10)
forest(asym_plot_rob_1_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       overall.hetstat = FALSE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#conduct meta-analysis - contact and outbreak

asym_plot_rob_1_contact<-metaprop(events,total,data=data_rob_1_contact,sm = "PLOGIT", 
                          studlab=label, byvar=rob_select, prediction = TRUE, 
                          print.byvar = FALSE, comb.fixed = FALSE,
                          overall.hetstat = FALSE,
                          control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_1_contact

tiff(filename = "Q1_rob_select_contact.tiff",
     width = 2900, height = 3800,
     res = 300)
forest(asym_plot_rob_1_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       overall.hetstat = FALSE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_select_contact.pdf", height = 13, width = 10)
forest(asym_plot_rob_1_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       overall.hetstat = FALSE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#2. information bias - only analyse those with low ROB for symptom assessment
#and recording of symptoms
#prepare data
data_rob_2_screen <- data_rob %>%
  filter(setting == "Screening") %>%
  mutate(rob_info = ifelse(rob_3 == 3 & rob_4 == 3, "Low risk of information bias", "Unclear or high risk of information bias"))
data_rob_2_contact <- data_rob %>%
  filter(setting == "Contact and outbreak investigations") %>%
  mutate(rob_info = ifelse(rob_3 == 3 & rob_4 == 3, "Low risk of information bias", "Unclear or high risk of information bias"))
#conduct meta-analysis - screening

asym_plot_rob_2_screen <- metaprop(events,total,data=data_rob_2_screen,sm = "PLOGIT", studlab=label, 
                          byvar=rob_info,# tau.common =TRUE,
                          overall.hetstat = FALSE,
                          prediction = TRUE, print.subgroup.name = FALSE, fixed = FALSE,
                          control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_2_screen

tiff(filename = "Q1_rob_info_screen.tiff",
     width = 2800, height = 4500,
     res = 300)
forest(asym_plot_rob_2_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_info_screen.pdf", height = 15, width = 10)
forest(asym_plot_rob_2_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#conduct meta-analysis - contact

asym_plot_rob_2_contact <- metaprop(events,total,data=data_rob_2_contact,sm = "PLOGIT", studlab=label, 
                                   byvar=rob_info,# tau.common =TRUE,
                                   overall.hetstat = FALSE,
                                   prediction = TRUE, print.subgroup.name = FALSE, fixed = FALSE,
                                   control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_2_contact

tiff(filename = "Q1_rob_info_contact.tiff",
     width = 2800, height = 3700,
     res = 300)
forest(asym_plot_rob_2_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_info_contact.pdf", height = 13, width = 10)
forest(asym_plot_rob_2_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#3. misclassification bias - only analyse those with low ROB for 
#classification of asymp status
#prepare data
data_rob_3_screen <- data_rob %>%
  filter(setting == "Screening") %>%
  mutate(rob_misclas = ifelse(rob_5 == 3, "Low risk of misclassification bias", "Unclear or high risk of misclassification bias"))
data_rob_3_contact <- data_rob %>%
  filter(setting == "Contact and outbreak investigations") %>%
  mutate(rob_misclas = ifelse(rob_5 == 3, "Low risk of misclassification bias", "Unclear or high risk of misclassification bias"))

#conduct meta-analysis - screen

data_rob_3_screen <- metaprop(events,total,data=data_rob_3_screen,sm = "PLOGIT", studlab=label, 
                            byvar=rob_misclas,# tau.common =TRUE,
                            prediction = TRUE, comb.random = TRUE, comb.fixed = FALSE,
                            print.byvar = FALSE,
                            overall.hetstat = FALSE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
data_rob_3_screen

tiff(filename = "Q1_rob_misclass_screen.tiff",
     width = 3000, height = 4500,
     res = 300)
forest(data_rob_3_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_misclass_screen.pdf", height = 15, width = 10)
forest(data_rob_3_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#conduct meta-analysis - contact

data_rob_3_contact <- metaprop(events,total,data=data_rob_3_contact,sm = "PLOGIT", studlab=label, 
                              byvar=rob_misclas,# tau.common =TRUE,
                              prediction = TRUE, comb.random = TRUE, comb.fixed = FALSE,
                              print.byvar = FALSE,
                              overall.hetstat = FALSE,
                              control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
data_rob_3_contact

tiff(filename = "Q1_rob_misclass_contact.tiff",
     width = 3000, height = 3800,
     res = 300)
forest(data_rob_3_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_misclass_contact.pdf", height = 12, width = 10)
forest(data_rob_3_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 


#4. attrition bias - only analyse those with low ROB for selective reporting
#of symptom status
#prepare data
data_rob_4_screen <- data_rob %>%
  filter(setting == "Screening") %>%
  mutate(rob_att = ifelse(rob_6 == 3, "Low risk of attrition bias", "Unclear or high risk of attrition bias"))
data_rob_4_contact <- data_rob %>%
  filter(setting == "Contact and outbreak investigations") %>%
  mutate(rob_att = ifelse(rob_6 == 3, "Low risk of attrition bias", "Unclear or high risk of attrition bias"))

#conduct meta-analysis - screening

asym_plot_rob_4_screen <- metaprop(events, total, data = data_rob_4_screen, 
                            sm = "PLOGIT", studlab=label, 
                            byvar=rob_att,# tau.common =TRUE,
                            prediction = TRUE,
                            overall.hetstat = FALSE,
                            print.byvar = FALSE, comb.random = TRUE, comb.fixed = FALSE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_4_screen

tiff(filename = "Q1_rob_attrition_screen.tiff",
     width = 3000, height = 4500,
     res = 300)
forest(asym_plot_rob_4_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_attrition_screen.pdf", height = 15, width = 10)
forest(asym_plot_rob_4_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#conduct meta-analysis - contact

asym_plot_rob_4_contact <- metaprop(events, total, data = data_rob_4_contact, 
                                   sm = "PLOGIT", studlab=label, 
                                   byvar=rob_att,# tau.common =TRUE,
                                   prediction = TRUE,
                                   overall.hetstat = FALSE,
                                   print.byvar = FALSE, comb.random = TRUE, comb.fixed = FALSE,
                                   control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_4_contact

tiff(filename = "Q1_rob_attrition_contact.tiff",
     width = 3000, height = 3800,
     res = 300)
forest(asym_plot_rob_4_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_attrition_contact.pdf", height = 12, width = 10)
forest(asym_plot_rob_4_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#5. all low rob


#prepare data
data_rob_5_screen <- data_rob %>%
  filter(setting == "Screening") %>%
  mutate(rob_all_low = ifelse(rob_1 == 3 & rob_2 == 3 & rob_3 == 3 & rob_4 == 3 & rob_5 == 3 & rob_6 == 3, 
                          "Low risk of bias in all domains", "Unclear or high risk of bias in all domains"))
data_rob_5_contact <- data_rob %>%
  filter(setting == "Contact and outbreak investigations") %>%
  mutate(rob_all_low = ifelse(rob_1 == 3 & rob_2 == 3 & rob_3 == 3 & rob_4 == 3 & rob_5 == 3 & rob_6 == 3, 
                          "Low risk of bias in all domains", "Unclear or high risk of bias in all domains"))

#conduct meta-analysis - screening

asym_plot_rob_5_screen <- metaprop(events, total, data = data_rob_5_screen, 
                            sm = "PLOGIT", studlab=label, 
                            byvar=rob_all_low,# tau.common =TRUE,
                            prediction = TRUE,
                            overall.hetstat = FALSE,
                            print.byvar = FALSE,
                            comb.random = TRUE, comb.fixed = FALSE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_5_screen

tiff(filename = "Q1_rob_alllowROB_screen.tiff",
     width = 3000, height = 4500,
     res = 300)
forest(asym_plot_rob_5_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_alllowROB_screen.pdf", height = 15, width = 10)
forest(asym_plot_rob_5_screen, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#conduct meta-analysis - contact

asym_plot_rob_5_contact <- metaprop(events, total, data = data_rob_5_contact, 
                                   sm = "PLOGIT", studlab=label, 
                                   byvar=rob_all_low,# tau.common =TRUE,
                                   prediction = TRUE,
                                   overall.hetstat = FALSE,
                                   print.byvar = FALSE,
                                   comb.random = TRUE, comb.fixed = FALSE,
                                   control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_5_contact

tiff(filename = "Q1_rob_alllowROB_contact.tiff",
     width = 3000, height = 3700,
     res = 300)
forest(asym_plot_rob_5_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
pdf("Q1_rob_alllowROB_contact.pdf", height = 13, width = 10)
forest(asym_plot_rob_5_contact, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       overall.hetstat = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 

#6. without studies with sample size <10

#prepare data
data_sample_size_10 <- data %>%
  filter(total >= 10) %>%
  select(1:8)
#conduct meta-analysis

asym_plot_sample_size_10 <- metaprop(events, total, data = data_sample_size_10, 
                            sm = "PLOGIT", studlab=label, 
                            byvar=setting,# tau.common =TRUE,
                            prediction = TRUE,
                            overall.hetstat = FALSE,
                            print.byvar = FALSE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_sample_size_10

tiff(filename = "Q1_rob_sample_size_10.tiff",
     width = 3000, height = 6400,
     res = 300)
forest(asym_plot_sample_size_10, sortvar = total, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE, overall.hetstat = FALSE,
       test.subgroup.random=FALSE, test.subgroup.fixed=FALSE)
dev.off() 

##################################################################################################
#Q1 by publication date, age group and region
####

###
#Get data
Q1_data <- httr::content(response)
Q1_data_age <- Q1_data %>%
  select(record_id, starts_with("agerange"))
Q1_data_region <- Q1_data %>%
  select(record_id, country)



#download data
# get the data directly from redcap:
# report #155 is Q1:
url <- "https://redcap.ispm.unibe.ch/api/"
token <- "F2725F15FE84D2832E2793BB23B0A62B"
formData <- list("token"=token,
                 content='report',
                 format='csv',
                 report_id='155',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 returnFormat='csv'
)
response <- httr::POST(url, body = formData, encode = "form")

##########################################
# Q1 forest plot
##########################################

asymptomaticQ1 <- httr::content(response)


settings=c("Contact investigation",
           "Contact investigation, aggregated",
           "Outbreak investigation",
           "Statistical model",
           "Screening",
           "Hospitalised adults",
           "Hospitalised children",
           "Hospitalised adults & children",
           "Screening: institutional setting",
           "Screening: community setting",
           "Screening: occupational")


asymptomaticQ1$setting2<-factor(asymptomaticQ1$setting2, levels=1:11, labels=settings)
#combine contact investigation and contact investigation aggregated
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Contact investigation, aggregated"] <- "Contact investigation"

# minor cleaning
asymptomaticQ1[asymptomaticQ1==9999]=NA


#regroup settings
asymptomaticQ1$setting2 <- as.character(asymptomaticQ1$setting2)
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Contact investigation"] <- "Contact and outbreak investigations"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Outbreak investigation"] <- "Contact and outbreak investigations"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Screening: community setting"] <- "Screening"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Screening: institutional setting"] <- "Screening"
asymptomaticQ1$setting2[asymptomaticQ1$setting2 == "Screening: occupational"] <- "Screening"

asymptomaticQ1$setting = asymptomaticQ1$setting2




data_long1 <- gather(asymptomaticQ1, cluster, total, c(q1_c1_total,q1_c2_total,q1_c3_total), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(record_id, author_1, setting, total, id, cluster)
data_long2 <-gather(asymptomaticQ1, cluster, events, c(q1_c1_event,q1_c2_event,q1_c3_event), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(events, id)

data_Q1 = merge(data_long1, data_long2, by="id")
data_Q1 = data_Q1[!is.na(data_Q1$total),]
data_Q1$cluster=factor(data_Q1$cluster,labels=c("1","2","3"),levels=c("q1_c1_total","q1_c2_total","q1_c3_total"))

data_Q1[data_Q1$record_id %in% asymptomaticQ1[is.na(asymptomaticQ1$q1_c2_event),]$record_id,]$cluster=NA

asymptomaticQ1=data_Q1

asymptomaticQ1$label=paste0(asymptomaticQ1$author_1, " (2020)",ifelse(!is.na(asymptomaticQ1$cluster),paste0(" cluster:",asymptomaticQ1$cluster),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

asymptomaticQ1$label=paste0(asymptomaticQ1$author_1,ifelse(!is.na(asymptomaticQ1$cluster),paste0(" [cluster:",asymptomaticQ1$cluster,"]"),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

#change clusters to descriptions
asymptomaticQ1$label[asymptomaticQ1$label == "Harada S [cluster:1]"] <- "Harada S [Patients]"
asymptomaticQ1$label[asymptomaticQ1$label == "Harada S [cluster:2]"] <- "Harada S [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "Kennelly SP [cluster:1]"] <- "Kennelly SP [Nursing home residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Kennelly SP [cluster:2]"] <- "Kennelly SP [Nursing home staff]"
asymptomaticQ1$label[asymptomaticQ1$label == "van Buul LW [cluster:1]"] <- "van Buul LW [Nursing home residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "van Buul LW [cluster:2]"] <- "van Buul LW [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "Theuring S [cluster:1]"] <- "Theuring S [School students and staff]"
asymptomaticQ1$label[asymptomaticQ1$label == "Theuring S [cluster:2]"] <- "Theuring S [Household members]"
asymptomaticQ1$label[asymptomaticQ1$label == "van den Besselaar JH [cluster:1]"] <- "van den Besselaar JH [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "van den Besselaar JH [cluster:2]"] <- "van den Besselaar JH [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Taylor J [cluster:1]"] <- "Taylor J [Healthcare personnel]"
asymptomaticQ1$label[asymptomaticQ1$label == "Taylor J [cluster:2]"] <- "Taylor J [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Ladhani SN [cluster:1]"] <- "Ladhani SN [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "Ladhani SN [cluster:2]"] <- "Ladhani SN [Healthcare workers]"
asymptomaticQ1$label[asymptomaticQ1$label == "#5551 Vohra [cluster:1]"] <- "#5551 Vohra [Presurgical patients]"
asymptomaticQ1$label[asymptomaticQ1$label == "#5551 Vohra [cluster:2]"] <- "#5551 Vohra [Undergoing chemotherapy]"
asymptomaticQ1$label[asymptomaticQ1$label == "#6526 Garibaldi [cluster:1]"] <- "#6526 Garibaldi [Residents]"
asymptomaticQ1$label[asymptomaticQ1$label == "#6526 Garibaldi [cluster:2]"] <- "#6526 Garibaldi [Staff]"



data=asymptomaticQ1[order(asymptomaticQ1$setting,1/(1/asymptomaticQ1$events+1/(asymptomaticQ1$total-asymptomaticQ1$events))),]

data[is.na(data$setting),]$record_id
data=data[!is.na(data$setting),]

####
#Q1 by publication date
####

#calculate how many papers were published in each period
data$pub <- NA
data$pub[data$record_id < 1438] <- "Published Jan '20 - Jun '20"
data$pub[data$record_id == 294] <- "Published Jul '20 - Dec '20" #published v of preprint
data$pub[data$record_id >=4866] <- "Published Jan '21 - Jun '21"
data$pub[data$record_id >= 1438 & data$record_id < 4866] <- "Published Jul '20 - Dec '20" 


asym_plot_bydate <- metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    byvar=pub,
                    prediction = TRUE,
                    print.byvar = FALSE,
                    overall.hetstat = FALSE,
                    comb.fixed = FALSE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot_bydate

tiff(filename = "Q1_bypubdate.tiff",
     width = 4000, height = 9800,
     res = 400)
forest(asym_plot_bydate, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       overall.hetstat = FALSE,
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 

#contact and outbreak only

data_contact_outbreak <- data %>%
  filter(setting == "Contact and outbreak investigations")

asym_plot_bydate_con_out <- metaprop(events,total,data=data_contact_outbreak,sm = "PLOGIT", studlab=label, 
                             byvar=pub,
                             prediction = TRUE,
                             overall.hetstat = FALSE,
                             print.byvar = FALSE,
                             comb.fixed = FALSE,
                             control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot_bydate_con_out

asym_plot_bydate_con_out$bylevs <- c("Published Jan '20 - Jun '20", "Published Jul '20 - Dec '20", "Published Jan '21 - Jun '21")


tiff(filename = "Q1_bypubdate_contact_outbreak.tiff",
     width = 4000, height = 6000,
     res = 400)
forest(asym_plot_bydate_con_out, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       overall.hetstat = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 

#screening only

data_screening <- data %>%
  filter(setting == "Screening")


asym_plot_bydate_screen <- metaprop(events,total,data=data_screening,sm = "PLOGIT", studlab=label, 
                             byvar=pub,
                             prediction = TRUE,
                             print.byvar = FALSE,
                             overall.hetstat = FALSE,
                             comb.fixed = FALSE,
                             control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot_bydate_screen

asym_plot_bydate_screen$bylevs <- c("Published Jan '20 - Jun '20", "Published Jul '20 - Dec '20", "Published Jan '21 - Jun '21")


tiff(filename = "Q1_bypubdate_screen.tiff",
     width = 3000, height = 6500,
     res = 300)
forest(asym_plot_bydate_screen, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       overall.hetstat = FALSE,
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 



####
#Q1 by age group
####
Q1_data_age <- Q1_data_age %>%
  mutate(agerange = ifelse(agerange___1 == 1, "Children (<18 years)", NA)) %>%
  mutate(agerange = ifelse(agerange___2 == 1, "Adults (18 - 65 years)", agerange)) %>%
  mutate(agerange = ifelse(agerange___3 == 1, "Older adults (>65 years)", agerange)) %>%
  mutate(agerange = ifelse(agerange___4 == 1, "All ages", agerange)) %>%
  mutate(agerange = ifelse(agerange___5 == 1, "Not reported", agerange)) 
  

####
#Q1 by region
####

#############################
# meta-regression
#############################


asym_plot<-metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

#examine sample size
table(data$total)

asym_plot_mr <- metareg(asym_plot, total)

#############################
# Galbraith plot
#############################

### draw radial plot
radial(asym_plot, level = 0.95, 
       pch = 1, col = "darkred")

