
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

##########################################
# Q2.1 forest plot
##########################################

# get the data directly from redcap: 
# report #172 is Q2.1 SAR:

################################################################
#Below codes are to get data from REDCap. In order to reproduce #
#plots and analysis please use "Q2_ExtractedData.csv" file.     #
#################################################################


url <- "https://redcap.ispm.unibe.ch/api/"
token <- "####################################"
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


#read "Q2_ExtractedData.csv" file
asymptomaticQ2_1 <- read.csv("Q2_ExtractedData.csv")


#clean data
asymptomaticQ2_1[asymptomaticQ2_1=="9999;9999"]=NA #indicate as missing
asymptomaticQ2_1[asymptomaticQ2_1=="9999"]=NA #indicate as missing

#added for now
asymptomaticQ2_1 <- asymptomaticQ2_1[asymptomaticQ2_1$record_id < 5296, ]

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

#rename groups
q2_1_df$group[q2_1_df$group == "Presymptomatic"] <- "Presymptomatic vs. Symptomatic"
q2_1_df$group[q2_1_df$group == "Asymptomatic"] <- "Asymptomatic vs. Symptomatic"
q2_1_df$group[q2_1_df$record_id == 821] <- "Asymptomatic vs. Presymptomatic"



#add columns for forest plot
q2_1_df$col_1 <- paste0(q2_1_df$Ee, "/", q2_1_df$Ne, sep = "")

q2_1_df$col_2 <- paste0(q2_1_df$Ec, "/", q2_1_df$Nc, sep = "")


#run meta analysis for all available studies
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
                      hakn = TRUE,
                      MH.exact = TRUE, 
                      byvar = group,
                      comb.random = TRUE, #with pooled estimate set it to FALSE,
                      comb.fixed = FALSE,
                      print.byvar = FALSE,
                      label.e = "",
                      label.c = "Symp.  ")
q2_1_asymp


tiff(filename = "Q2_1_update5.tiff",
     width = 2450, height = 1400,
     res = 300)
forest(q2_1_asymp, sortvar = (1/(sqrt(seTE))), overall = FALSE,
       overall.hetstat = FALSE,
       print.byvar = FALSE,
       leftcols = c("studlab", "col_1", "col_2"),
       leftlabs = c("Author", "E/N", "E/N (REF)"))
dev.off()


#cut data at end of jan 31
published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
q2_1_df <- q2_1_df %>%
  filter(record_id <= 5296 | record_id %in% published_preprints)

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
                      hakn = TRUE,
                      MH.exact = TRUE, 
                      byvar = group,
                      comb.random = TRUE, #without pooled estimate set it to FALSE,
                      print.byvar = FALSE,
                      comb.fixed = FALSE, 
                      label.e = "",
                      label.c = "Symp.  ")
q2_1_asymp


tiff(filename = "Q2_1.tiff",
     width = 2450, height = 1800,
     res = 300)
forest(q2_1_asymp, sortvar = (1/(sqrt(seTE))), overall = FALSE,
       overall.hetstat = FALSE,
       print.byvar = FALSE,
       addrows.below.overall = 2,
       leftcols = c("studlab", "col_1", "col_2"),
       leftlabs = c("Author", "E/N", "E/N (Symp.)"))
dev.off()


pdf("Q2_1.pdf",
     width = 8, height = 6)
forest(q2_1_asymp, sortvar = (1/(sqrt(seTE))), overall = FALSE,
       overall.hetstat = FALSE,
       print.byvar = FALSE,
       addrows.below.overall = 2,
       leftcols = c("studlab", "col_1", "col_2"),
       leftlabs = c("Author", "E/N", "E/N (Symp.)"))
dev.off()

