
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
# table on characteristics of Q1 studies
##########################################

#prepare data for table 1: characteristics of studies included in q1

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
Q1_table <- httr::content(response)
#cut data at end of jan 31
published_preprints <-c(5565,6219, 6685, 7030, 7465, 8249, 9442, 9484)
Q1_table <- Q1_table %>%
  filter(record_id <= 5296 | record_id %in% published_preprints)
Q1_table <- Q1_table %>%
  select(-contains("rob")) %>%
  select(-contains("risk_of_bias")) %>%
  select(-journal)



#clean countries
Q1_table <- Q1_table %>%
  mutate(country = ifelse(country == 10, "Australia", country),
         country = ifelse(country == 11, "Austria", country),
         country = ifelse(country == 15, "Bahrain", country),
         country = ifelse(country == 19, "Belgium", country),
         country = ifelse(country == 27, "Brazil", country),
         country = ifelse(country == 28, "Brunei", country),
         country = ifelse(country == 34, "Canada", country),
         country = ifelse(country == 40, "China", country),
         country = ifelse(country == 41, "Colombia", country),
         country = ifelse(country == 51, "Denmark", country),
         country = ifelse(country == 62, "France", country),
         country = ifelse(country == 63, "French Guyana", country),
         country = ifelse(country == 67, "Germany", country),
         country = ifelse(country == 69, "Greece", country),
         country = ifelse(country == 78, "Iceland", country),
         country = ifelse(country == 79, "India", country),
         country = ifelse(country == 83, "Ireland", country),
         country = ifelse(country == 85, "Italy", country),
         country = ifelse(country == 87, "Japan", country),
         country = ifelse(country == 92, "Kuwait", country),
         country = ifelse(country == 105, "Malaysia", country),
         country = ifelse(country == 121, "Netherlands", country),
         country = ifelse(country == 130, "N", country),
         country = ifelse(country == 28, "Brunei", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 128, "Norway", country),
         country = ifelse(country == 138, "Portugal", country),
         country = ifelse(country == 146, "Saudi Arabia", country),
         country = ifelse(country == 157, "South Korea", country),
         country = ifelse(country == 160, "Spain", country),
         country = ifelse(country == 174, "Turkey", country),
         country = ifelse(country == 176, "Uganda", country),
         country = ifelse(country == 180, "United Kingdom", country),
         country = ifelse(country == 181, "United States of America", country),
         country = ifelse(country == 185, "Vietnam", country))
Q1_table$region <- NULL

#author column
Q1_table$study <- paste0(Q1_table$author_1, " (", Q1_table$year, ")")
Q1_table$author_1 <- NULL

Q1_table$year <- NULL

#clean settings
Q1_table <- Q1_table %>%
  mutate(setting = ifelse(setting2 == 1 | setting2 == 2, "Contact investigation", setting),
         setting = ifelse(setting2 == 3, "Outbreak investigation", setting),
         setting = ifelse(setting2 == 4, "Statistical model", setting),
         setting = ifelse(setting2 == 5, "Screening", setting),
         setting = ifelse(setting2 == 6, "Hospitalised adults", setting),
         setting = ifelse(setting2 == 7, "Hospitalised children", setting),
         setting = ifelse(setting2 == 8, "Hospitalised children and adults", setting),
         setting = ifelse(setting2 == 9, "Screening: institutional setting", setting),
         setting = ifelse(setting2 == 10, "Screening: community setting", setting),
         setting = ifelse(setting2 == 11, "Screening: occupational", setting))
Q1_table$setting2 <- NULL
record_ids <- Q1_table %>% select(record_id, setting)

#clean age category

Q1_table$q1_age_median[Q1_table$q1_age_median == 9999] <- NA
Q1_table$q1_age_iqr[Q1_table$q1_age_iqr == 9999] <- NA
Q1_table$age <- as.character(Q1_table$q1_age_median)
Q1_table$age[is.na(Q1_table$age)] <- "NR"
Q1_table <- Q1_table %>%
  mutate(age = ifelse(!is.na(q1_age_iqr), 
                      paste0(q1_age_median, " IQR ", q1_age_iqr),
                      age))
Q1_table$q1_age_iqr <- NULL
Q1_table$q1_age_median <- NULL
Q1_table$record_id <- NULL

Q1_table_studies <- Q1_table #table of studies, not individual clusters

#clean case numbers
#total SARS-CoV-2 n
Q1_table$total_SARS_n <- Q1_table$q1_c1_total
Q1_table$total_SARS_n_c2 <- Q1_table$q1_c2_total #numbers for cluster 2 - must be arranged later
#asymp n
Q1_table$asymp_SARS_n <- Q1_table$q1_c1_event
Q1_table$asymp_SARS_n_c2 <- Q1_table$q1_c2_event #numbers for cluster 2 - must be arranged later
Q1_table$q1_c1_event <- NULL
Q1_table$q1_c2_event <- NULL
Q1_table$q1_c1_total <- NULL
Q1_table$q1_c2_total <- NULL
Q1_table$q1_nclus <- NULL
Q1_table$q1_c3_event <- NULL
Q1_table$q1_c3_total <- NULL
Q1_table$comment_q1 <- NULL

#clean sex category
Q1_table$q1_female[Q1_table$q1_female == 9999] <- NA
Q1_table$q1_male[Q1_table$q1_male == 9999] <- NA


#clean up follow up
Q1_table_studies$follow_up <- NULL

Q1_table_studies$fup___2[Q1_table_studies$fup___2 == 1] <- 2
Q1_table_studies$fup___3[Q1_table_studies$fup___3 == 1] <- 3
Q1_table_studies$fup___4[Q1_table_studies$fup___4 == 1] <- 4
Q1_table_studies <- Q1_table_studies %>%
  mutate(follow_up = ifelse(fup___1 != 0 & fup___2 != 0 & fup___3 != 0 & fup___4 != 0,
                            paste0(fup___1, ", ", fup___2, ", ", fup___3, ", ", fup___4),
                            NA)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 0 & fup___3 == 0 & fup___4 == 0,
                            1,
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 2 & fup___3 == 0 & fup___4 == 0,
                            2,
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 0 & fup___3 == 3 & fup___4 == 0,
                            3,
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 0 & fup___3 == 0 & fup___4 == 4,
                            4,
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 2 & fup___3 == 0 & fup___4 == 0,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 0 & fup___3 == 3 & fup___4 == 0,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 2 & fup___3 == 3 & fup___4 == 0,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 0 & fup___3 == 0 & fup___4 == 4,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 2 & fup___3 == 0 & fup___4 == 4,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 0 & fup___3 == 3 & fup___4 == 4,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 2 & fup___3 == 0 & fup___4 == 4,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 2 & fup___3 == 3 & fup___4 == 0,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 1 & fup___2 == 0 & fup___3 == 3 & fup___4 == 4,
                            "2+",
                            follow_up)) %>%
  mutate(follow_up = ifelse(fup___1 == 0 & fup___2 == 2 & fup___3 == 3 & fup___4 == 4,
                            "2+",
                            follow_up))

Q1_table_studies <- Q1_table_studies %>%
  select(-contains("fup"))


names(Q1_table)
Q1_table$source <- NULL

#rearrange so that clusters and in one col
cluster2 <- Q1_table
cluster2 <- cluster2[!is.na(cluster2$total_SARS_n_c2), ]
cluster2$total_SARS_n <- cluster2$total_SARS_n_c2
cluster2$asymp_SARS_n <- cluster2$asymp_SARS_n_c2
cluster2$total_SARS_n_c2 <- NULL
cluster2$asymp_SARS_n_c2 <- NULL
cluster2$study <- paste0(cluster2$study, " [cluster 2]")
#add clusters to main table
Q1_table <- bind_rows(Q1_table, cluster2)
#drop extra cols
Q1_table$total_SARS_n_c2 <- NULL
Q1_table$asymp_SARS_n_c2 <- NULL

Q1_table <- Q1_table %>%
  arrange(setting)

names(Q1_table)
Q1_table$study <- NULL


##########################################
# summary table on characteristics of Q1 studies
# calculating summary numbers
##########################################


#calculate total studies by setting

total_n <- Q1_table_studies %>%
  count(setting)

#calculate sex, total infections, asymp infections 

sum_q1 <- Q1_table %>%
  select(-age, -follow_up, -country) %>%
  group_by(setting) %>%
  summarise(female = sum(q1_female, na.rm = TRUE), 
            male = sum(q1_male, na.rm = TRUE), 
            total_sars = sum(total_SARS_n, na.rm = TRUE), 
            total_asymp = sum(asymp_SARS_n, na.rm = TRUE))

#calculate n studies that did not report sex
sex_nr <- Q1_table_studies %>% 
  filter(q1_female == 9999 & q1_male == 9999) %>%
  count(setting)

#group ages and calculate how many per setting
age_groups <- Q1_table_studies %>%
  filter(age != "NR") %>%
  select(setting, age) %>%
  arrange(setting)

#calculate n studies that did not report age
age_nr <- Q1_table_studies %>% filter(age == "NR") %>%
  count(setting)

#calculate how many papers were published in each period
record_ids$period[record_ids$record_id < 1438] <- "Jan20"
record_ids$period[record_ids$record_id == 294] <- "Jul20" #published v of preprint
record_ids$period[record_ids$record_id >=4866] <- "Jan21"
record_ids$period[record_ids$record_id == 4968] <- "Jul20" #published dec 20
record_ids$period[record_ids$record_id >= 1438 & record_ids$record_id < 4866] <- "Jul20" 

by_period <- record_ids %>% group_by(period) %>% count(setting)

#calculate paers per region by setting
#first define who regions

africa <- c("Algeria","Angola","Benin","Botswana","Burkina Faso","Burundi","Cameroon","
            Cape Verde","Central African Republic","Chad","Comoros","Ivory Coast","
            Democratic Republic of the Congo","Equatorial Guinea","Eritrea","
            Ethiopia","Gabon","Gambia","Ghana","Guinea","Guinea-Bissau","Kenya","
            Lesotho","Liberia","Madagascar","Malawi","Mali","Mauritania","Mauritius","
            Mozambique","Namibia","Niger","Nigeria","Republic of the Congo","Rwanda","
            São Tomé and Príncipe","Senegal","Seychelles","Sierra Leone","
            South Africa","South Sudan","Eswatini","Togo","Uganda","Tanzania","
            Zambia","Zimbabwe")
americas <- c("Antigua and Barbuda", "Argentina", "Bahamas", "Barbados", 
              "Belize", "Bolivia", "Brazil", "Canada", "Chile", "Colombia", 
              "Costa Rica", "Cuba", "Dominica", "Dominican Republic", "Ecuador", 
              "El Salvador", "Grenada", "Guatemala", "Guyana", "Haiti", 
              "Honduras", "Jamaica", "Mexico", "Nicaragua", "Panama", 
              "Paraguay", "Peru", "Saint Kitts and Nevis", "Saint Lucia", 
              "Saint Vincent and the Grenadines", "Suriname", 
              "Trinidad and Tobago", "United States of America", "Uruguay", 
              "Venezuela", "French Guyana")
se_asia <- c("Bangladesh", "Bhutan", "North Korea", "India", "Indonesia", 
             "Maldives", "Myanmar", "Nepal", "Sri Lanka", "Thailand", 
             "Timor-Leste", "South Korea")
europe <- c("Albania", "Andorra", "Armenia", "Austria", "Azerbaijan", "Belarus", 
            "Belgium", "Bosnia and Herzegovina", "Bulgaria", "Croatia", 
            "Cyprus", "Czech Republic", "Denmark", "Estonia", "Finland", 
            "France", "Georgia", "Germany", "Greece", "Hungary", "Iceland", 
            "Ireland", "Israel", "Italy", "Kazakhstan", "Kyrgyzstan", "Latvia", 
            "Lithuania", "Luxembourg", "Malta", "Moldova", "Monaco", 
            "Montenegro", "Netherlands", "North Macedonia", "Norway", "Poland", 
            "Portugal", "Romania", "Russia", "San Marino", "Serbia", "Slovakia", 
            "Slovenia", "Spain", "Sweden", "Switzerland", "Tajikistan", 
            "Turkey", "Turkmenistan", "Ukraine", "United Kingdom", "Uzbekistan")
east_med <- c("Afghanistan", "Bahrain", "Djibouti", "Egypt", "Iran", "Iraq", 
             "Jordan", "Kuwait", "Lebanon", "Libya", "Morocco", "Oman", 
             "Pakistan", "Palestine", "Qatar", "Saudi Arabia", "Somalia", 
             "Sudan", "Syria", "Tunisia", "United Arab Emirates", "Yemen")
western_pacific <- c("Australia", "Brunei", "Cambodia", "China", "Cook Islands", 
                     "Fiji", "Japan", "Kiribati", "Laos", "Malaysia", 
                     "Marshall Islands", "Micronesia", "Mongolia", "Nauru", 
                     "New Zealand", "Niue", "Palau", "Papua New Guinea", 
                     "Philippines", "Republic of Korea", "Samoa", "Singapore", 
                     "Solomon Islands", "Tonga", "Tuvalu", "Vanuatu", "Vietnam",
                     "Other") #other is taiwan here
#categorise studies into a who region
Q1_table_studies$who_region[Q1_table_studies$country %in% africa] <- "Africa"
Q1_table_studies$who_region[Q1_table_studies$country %in% americas] <- "Americas"
Q1_table_studies$who_region[Q1_table_studies$country %in% se_asia] <- "SE Asia"
Q1_table_studies$who_region[Q1_table_studies$country %in% europe] <- "Europe"
Q1_table_studies$who_region[Q1_table_studies$country %in% east_med] <- "East Med"
Q1_table_studies$who_region[Q1_table_studies$country %in% western_pacific] <- "Western Pacific"
#studies by who region by setting
by_region <- Q1_table_studies %>% group_by(who_region) %>%
  count(setting)
#summarise follow-up methods
by_follow_up <- Q1_table_studies %>% group_by(follow_up) %>%
  count(setting)




