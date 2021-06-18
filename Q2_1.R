###############################################################
# Question 2.1: results output ################################
# Secondary attack rate from asymp or pre-symp cases ##########
###############################################################


#load libraries
library(RCurl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(meta)
library(metafor)

# get the data directly from redcap: 
# report #172 is Q2.1 SAR:
url <- "https://redcap.ispm.unibe.ch/api/"
token <- "F2725F15FE84D2832E2793BB23B0A62B"
formData2_1 <- list("token"=token,
                    content='report',
                    format='csv',
                    report_id='172',
                    csvDelimiter='',
                    rawOrLabel='raw',
                    rawOrLabelHeaders='raw',
                    exportCheckboxLabel='false',
                    returnFormat='json'
)
response2_1 <- httr::POST(url, body = formData2_1, encode = "form")
asymptomaticQ2_1 <- httr::content(response2_1)

#clean data
asymptomaticQ2_1[asymptomaticQ2_1=="9999;9999"]=NA #indicate as missing
asymptomaticQ2_1[asymptomaticQ2_1=="9999"]=NA #indicate as missing
asymptomaticQ2_1=asymptomaticQ2_1%>% #separate symp SAR into 2 variables
  separate(q3_sar_s, c("Ec","Nc"), ";", remove=FALSE) 
asymptomaticQ2_1=asymptomaticQ2_1%>% #separate asymp SAR into 2 variables
  separate(q3_sar_a, c("Ee_a","Ne_a"), ";", remove=FALSE) 
asymptomaticQ2_1=asymptomaticQ2_1%>% #separate presymp SAR into 2 variables
  separate(q3_sar_p, c("Ee_p","Ne_p"), ";", remove=FALSE) 

asymptomaticQ2_1=asymptomaticQ2_1 %>% #change values to numeric
  mutate(Ec=as.numeric(Ec),
         Nc=as.numeric(Nc),
         Ee_a=as.numeric(Ee_a),
         Ne_a=as.numeric(Ne_a),
         Ee_p=as.numeric(Ee_p),
         Ne_p=as.numeric(Ne_p))

#metaanalysis for asymptomatic transmission

q2_1_asymp <- metabin(event.e = Ee_a,
                      n.e = Ne_a,
                      event.c = Ec,
                      n.c = Nc, 
                      data = asymptomaticQ2_1[!is.na(asymptomaticQ2_1$Ee_a),],
                      studlab = author_1,
                      prediction = TRUE,
                      sm = "RR",
                      method = "MH",
                      MH.exact = TRUE)
q2_1_asymp


png(file = 'forest_meta_Q2_1_asymp.png',width=750,height=250)

forest(q2_1_asymp)

dev.off()

#metaanalysis for presymptomatic transmission

q2_1_presymp <- metabin(event.e = Ee_p,
                        n.e = Ne_p,
                        event.c = Ec,
                        n.c = Nc, 
                        data = asymptomaticQ2_1[!is.na(asymptomaticQ2_1$Ee_p),],
                        studlab = author_1,
                        prediction = TRUE,
                        sm = "RR",
                        method = "MH",
                        MH.exact = TRUE)

q2_1_presymp


png(file = 'forest_meta_Q2_1_presymp.png',width=750,height=190) 

forest(q2_1_presymp)

dev.off()

##########################################
# table on characteristics of Q2.1 studies
##########################################


q2_1_author <- NA
q2_1_location <- NA
q2_1_infections_total <- NA

q2_1_infections_asymp <- NA

q2_1_n_infected_symp <- NA
q2_1_n_total_symp <- NA
q2_1_n_infected_presymp <- NA
q2_1_n_total_presymp <- NA
q2_1_n_infected_asymp <- NA
q2_1_n_total_asymp <- NA




