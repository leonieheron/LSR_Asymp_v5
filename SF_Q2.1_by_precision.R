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
library(flextable)

# get the data directly from redcap: 
# report #172 is Q2.1 SAR:
url <- "https://redcap.ispm.unibe.ch/api/"
#APIs token is not sharable
token <- "##################################"
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

#create new df with asymp/presymp as subgroups
q2_1_df_a <- asymptomaticQ2_1 %>%
  select("record_id", "author_1", "Ee_a", "Ne_a", "Ec", "Nc", "region", "year") %>%
  rename(Ee = Ee_a,
         Ne = Ne_a) %>%
  filter(!is.na(Ee)) %>%
  mutate(group = "Asymptomatic")
q2_1_df_p <- asymptomaticQ2_1 %>%
  select("record_id", "author_1", "Ee_p", "Ne_p", "Ec", "Nc", "region", "year") %>%
  rename(Ee = Ee_p,
         Ne = Ne_p) %>%
  filter(!is.na(Ee)) %>%
  mutate(group = "Presymptomatic")
#bind rows
q2_1_df <- bind_rows(q2_1_df_a, q2_1_df_p)


#metaanalysis for asymptomatic transmission

q2_1_asymp <- metabin(event.e = Ee,
                      n.e = Ne,
                      event.c = Ec,
                      n.c = Nc, 
                      data = q2_1_df,
                      studlab = author_1,
                      prediction = TRUE,
                      sm = "RR",
                      method = "MH",
                      MH.exact = TRUE, 
                      byvar = group)

##########################################
# supplementary information - by precision
##########################################

#metaanalysis for asymptomatic transmission

q2_1_asymp_precision <- metabin(event.e = Ee,
                                n.e = Ne,
                                event.c = Ec,
                                n.c = Nc, 
                                data = q2_1_df,
                                studlab = author_1,
                                sm = "RR",
                                byvar = group)

pdf(file = 'forest_meta_Q2_1_precision.pdf', width = 10, height = 27) 

forest(q2_1_asymp_precision, sortvar = (1/(sqrt(seTE))))


dev.off()

png(file = 'forest_meta_Q2_1_precision.png', res = 300, height = 14, width = 40, units = "cm")

forest(q2_1_asymp_precision, sortvar = (1/(sqrt(seTE))))

dev.off()



