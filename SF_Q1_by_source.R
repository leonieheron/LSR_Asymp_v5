###############################################################
# Question 1: results output                         ##########
# Proportion of asymptomatic cases                   ##########
# Supplementary material: preprints removed          ##########
###############################################################

#load the libraries
library(meta)
library(readxl)
library(tidyverse)
library(httr) # use to retrieve data from REDCap
library(kableExtra)
library(flextable)
library(dplyr)
library(Rcpp)


# Prepare the data
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
asymptomaticQ1_pub <- httr::content(response)


settings=c("Contact investigation",
           "Contact investigation, aggregated",
           "Outbreak investigation",
           "Statistical model",
           "Screening",
           "Hospitalised adults",
           "Hospitalised children",
           "Hospitalised adults & children")


asymptomaticQ1_pub$setting2<-factor(asymptomaticQ1_pub$setting2, levels=1:8, labels=settings)
#combine contact investigation and contact investigation aggregated
asymptomaticQ1_pub$setting2[asymptomaticQ1_pub$setting2 == "Contact investigation, aggregated"] <- "Contact investigation"

# minor cleaning
asymptomaticQ1_pub[asymptomaticQ1_pub==9999]=NA

asymptomaticQ1_pub$setting=asymptomaticQ1_pub$setting2

#remove preprints at time of search
asymptomaticQ1_pub <- asymptomaticQ1_pub %>%
  filter(source != 1 & source != 2)

data_long1_pub <- gather(asymptomaticQ1_pub, cluster, total, c(q1_c1_total,q1_c2_total,q1_c3_total), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(record_id, author_1, setting, total, id, cluster)
data_long2_pub <-gather(asymptomaticQ1_pub, cluster, events, c(q1_c1_event,q1_c2_event,q1_c3_event), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(events, id)

data_Q1_pub = merge(data_long1_pub, data_long2_pub, by="id")
data_Q1_pub = data_Q1_pub[!is.na(data_Q1_pub$total),]
data_Q1_pub$cluster=factor(data_Q1_pub$cluster,labels=c("1","2","3"),levels=c("q1_c1_total","q1_c2_total","q1_c3_total"))

data_Q1_pub[data_Q1_pub$record_id %in% asymptomaticQ1_pub[is.na(asymptomaticQ1_pub$q1_c2_event),]$record_id,]$cluster=NA

asymptomaticQ1_pub=data_Q1_pub

asymptomaticQ1_pub$label=paste0("#", asymptomaticQ1_pub$record_id, " ", asymptomaticQ1_pub$author_1, " (2020)",ifelse(!is.na(asymptomaticQ1_pub$cluster),paste0(" cluster:",asymptomaticQ1_pub$cluster),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

asymptomaticQ1_pub$label=paste0("#", asymptomaticQ1_pub$record_id, " ", asymptomaticQ1_pub$author_1,ifelse(!is.na(asymptomaticQ1_pub$cluster),paste0(" [cluster:",asymptomaticQ1_pub$cluster,"]"),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

#change clusters to descriptions
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#4479 Harada [cluster:1]"] <- "#4479 Harada [Patients]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#4479 Harada [cluster:2]"] <- "#4479 Harada [Healthcare workers]"
#asymptomaticQ1$label[asymptomaticQ1$label == "#2826 Taylor [cluster:1]"] <- "#2826 Taylor []" #clarify
#asymptomaticQ1$label[asymptomaticQ1$label == "#2826 Taylor [cluster:2]"] <- "#2826 Taylor []"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#2892 Kennelly [cluster:1]"] <- "#2892 Kennelly [Nursing home residents]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#2892 Kennelly [cluster:2]"] <- "#2892 Kennelly [Nursing home staff]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#2802 Bender [cluster:1]"] <- "#2802 Bender [Hospital 1]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#2802 Bender [cluster:2]"] <- "#2802 Bender [Hospital 2]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#4968 van Buul [cluster:1]"] <- "#4968 van Buul [Nursing home residents]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#4968 van Buul [cluster:2]"] <- "#4968 van Buul [Healthcare workers]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#5273 Theuring [cluster:1]"] <- "#5273 Theuring [School students and staff]"
asymptomaticQ1_pub$label[asymptomaticQ1_pub$label == "#5273 Theuring [cluster:2]"] <- "#5273 Theuring [Household members]"


data_pub=asymptomaticQ1_pub[order(asymptomaticQ1_pub$setting,1/(1/asymptomaticQ1_pub$events+1/(asymptomaticQ1_pub$total-asymptomaticQ1_pub$events))),]

data_pub[is.na(data_pub$setting),]$record_id
data_pub=data_pub[!is.na(data_pub$setting),]

asym_plot<-metaprop(events,total,data=data_pub,sm = "PLOGIT", studlab=label, 
                    byvar=setting,# tau.common =TRUE,
                    prediction = TRUE,
                    control=list(stepadj=0.05, maxiter=10000))#, method ="INV") #, verbose=TRUE, digits=5, control=list(stepadj=0.5))

asym_plot

png(file = 'forest_meta_Q1_published.png',width = 30,height = 60, res=600, units="cm") 

forest(asym_plot,  col.square=data_pub$setting, sortvar = 1/seTE,
       #squaresize = 1/(1/data$events+1/(data$total-data$events)),
       #sortvar = 1/(1/events+1/(total-events)), 
       subgroup=TRUE, 
       just.studlab="left", colgap.studlab="2cm",
       #leftcols = c("label", "setting", "age", "design", "asym_denom"),
       #leftcols=c("studlab", "record_id", "event", "n"),
       xlab = "",just="left",comb.random=TRUE,test.subgroup.random=FALSE,
       comb.fixed=FALSE,test.subgroup.fixed=FALSE, predict=T)#, study.results=TRUE,test.subgroup=TRUE)
dev.off()






