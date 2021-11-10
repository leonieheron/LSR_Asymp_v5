
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
#asymptomaticQ1$label[asymptomaticQ1$label == "#5551 Vohra [cluster:1]"] <- "#5551 Vohra [Presurgical patients]"
#asymptomaticQ1$label[asymptomaticQ1$label == "#5551 Vohra [cluster:2]"] <- "#5551 Vohra [Undergoing chemotherapy]"
#asymptomaticQ1$label[asymptomaticQ1$label == "#6526 Garibaldi [cluster:1]"] <- "#6526 Garibaldi [Residents]"
#asymptomaticQ1$label[asymptomaticQ1$label == "#6526 Garibaldi [cluster:2]"] <- "#6526 Garibaldi [Staff]"



data=asymptomaticQ1[order(asymptomaticQ1$setting,1/(1/asymptomaticQ1$events+1/(asymptomaticQ1$total-asymptomaticQ1$events))),]

data[is.na(data$setting),]$record_id
data=data[!is.na(data$setting),]

#cut data at end of jan 31
published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
data <- data %>%
  filter(record_id <= 5296 | record_id %in% published_preprints)


asym_plot<-metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

tiff(filename = "Q1.tiff",
     width = 3000, height = 7100,
     res = 300)
forest(asym_plot, sortvar = 1/seTE, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=TRUE, test.subgroup.fixed=FALSE)
dev.off() 
 

#####
#subgroup analysis
#studies removed at high risk of selection bias
#studies 443, 597, 1960, 2802, 2907, 3921, 5068, 5238, 170, 2987, 5086, 1225, 4880
high_risk_select_bias <-c(443, 597, 1960, 2802, 2907, 3921, 5068, 5238, 170, 2987, 5086, 1225, 4880)
data_selectionbias <- data %>%
  filter(!record_id %in% high_risk_select_bias)

#run metaprop and forest plot


asym_plot_select_bias<-metaprop(events,total,data=data_selectionbias,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot_select_bias

tiff(filename = "Q1_select_bias.tiff",
     width = 3000, height = 6300,
     res = 300)
forest(asym_plot_select_bias, sortvar = 1/seTE, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
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

#merge with dataset
data_rob <- left_join(data, rob_records, by = "record_id")
names(data_rob)


#1. selection bias - only analyse those with low ROB for representativeness
#and characteristics of non-respondents
#prepare data
data_rob_1 <- data_rob %>%
  filter(rob_1 == 3 & rob_2 == 3) %>%
  select(1:8)
#conduct meta-analysis

asym_plot_rob_1<-metaprop(events,total,data=data_rob_1,sm = "PLOGIT", studlab=label, 
                                byvar=setting,# tau.common =TRUE,
                                prediction = TRUE,
                                control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_1

tiff(filename = "Q1_rob_1.tiff",
     width = 3000, height = 3100,
     res = 300)
forest(asym_plot_rob_1, sortvar = 1/seTE, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=FALSE, test.subgroup.fixed=FALSE)
dev.off() 

#2. information bias - only analyse those with low ROB for symptom assessment
#and recording of symptoms
#prepare data
data_rob_2 <- data_rob %>%
  filter(rob_3 == 3 & rob_4 == 3) %>%
  select(1:8)
#conduct meta-analysis

asym_plot_rob_2 <- metaprop(events,total,data=data_rob_2,sm = "PLOGIT", studlab=label, 
                          byvar=setting,# tau.common =TRUE,
                          prediction = TRUE,
                          control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_2

tiff(filename = "Q1_rob_2.tiff",
     width = 3000, height = 2200,
     res = 300)
forest(asym_plot_rob_2, sortvar = 1/seTE, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=FALSE, test.subgroup.fixed=FALSE)
dev.off() 

#3. misclassification bias - only analyse those with low ROB for 
#classification of asymp status
#prepare data
data_rob_3 <- data_rob %>%
  filter(rob_5 == 3) %>%
  select(1:8)
#conduct meta-analysis

asym_plot_rob_3 <- metaprop(events,total,data=data_rob_3,sm = "PLOGIT", studlab=label, 
                            byvar=setting,# tau.common =TRUE,
                            prediction = TRUE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_3

tiff(filename = "Q1_rob_3.tiff",
     width = 3000, height = 5500,
     res = 300)
forest(asym_plot_rob_3, sortvar = 1/seTE, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=FALSE, test.subgroup.fixed=FALSE)
dev.off() 


#4. attrition bias - only analyse those with low ROB for selective reporting
#of symptom status

#prepare data
data_rob_4 <- data_rob %>%
  filter(rob_6 == 3) %>%
  select(1:8)
#conduct meta-analysis

asym_plot_rob_4 <- metaprop(events, total, data = data_rob_4, 
                            sm = "PLOGIT", studlab=label, 
                            byvar=setting,# tau.common =TRUE,
                            prediction = TRUE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_4

tiff(filename = "Q1_rob_4.tiff",
     width = 3000, height = 6500,
     res = 300)
forest(asym_plot_rob_4, #sortvar = n, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=FALSE, test.subgroup.fixed=FALSE)
dev.off() 

#5. all low rob

#prepare data
data_rob_5 <- data_rob %>%
  filter(rob_1 == 3 & rob_2 == 3 & rob_3 == 3 & rob_4 == 3 & rob_5 == 3 & 
           rob_6 == 3) %>%
  select(1:8)
#conduct meta-analysis

asym_plot_rob_5 <- metaprop(events, total, data = data_rob_5, 
                            sm = "PLOGIT", studlab=label, 
                            byvar=setting,# tau.common =TRUE,
                            prediction = TRUE,
                            control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))
asym_plot_rob_5

tiff(filename = "Q1_rob_5.tiff",
     width = 3000, height = 1500,
     res = 300)
forest(asym_plot_rob_5, #sortvar = n, #sorted by study precision
       subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE,
       test.subgroup.random=FALSE, test.subgroup.fixed=FALSE)
dev.off() 


