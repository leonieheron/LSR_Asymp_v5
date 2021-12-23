
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
token <- "###############################"
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
     width = 4000, height = 11000,
     res = 400)
forest(asym_plot, sortvar = 1/seTE, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 


published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
#include additional study identified from ref list
additional <- 11099
data <- data %>%
  filter(record_id <= 5296 | 
           record_id %in% published_preprints | 
           record_id %in% additional)




asym_plot<-metaprop(events,total,data=data,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    print.byvar = FALSE,
                    overall.hetstat = FALSE,
                    comb.fixed = FALSE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

tiff(filename = "Q1_stratified_all_settings.tiff",
     width = 4000, height = 10500,
     res = 400)
forest(asym_plot, sortvar = total, #sorted by study precision
       #subgroup=TRUE,
       col.square = "darkblue",
       overall.hetstat = FALSE,
       just="left", colgap.studlab="1cm",
       predict=T, comb.random = TRUE, comb.fixed = FALSE,
       print.byvar = FALSE, overall = FALSE)
dev.off() 
