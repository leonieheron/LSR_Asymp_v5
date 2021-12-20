###############################################################
# SF: Prepare countries in Q1, Q2.1, Q2.2            ##########
###############################################################

#load the libraries
library(meta)
library(readxl)
library(tidyverse)
library(httr) # use to retrieve data from REDCap

# Prepare the data
# get the data directly from redcap:
# report #155 is Q1:
url <- "https://redcap.ispm.unibe.ch/api/"
#APIs token is not sharable
token <- "##################################"
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
asymptomaticQ1 <- httr::content(response)

#only keep required variables
countries_q1 <- asymptomaticQ1 %>%
  select(record_id, author_1, country, region) %>%
  mutate(country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 40, "China", country),
         country = ifelse(country == 87, "Japan", country),
         country = ifelse(country == 62, "France", country),
         country = ifelse(country == 85, "Italy", country),
         country = ifelse(country == 67, "Germany", country),
         country = ifelse(country == 157, "South Korea", country),
         country = ifelse(country == 181, "United States of America", country),
         country = ifelse(country == 28, "Brunei", country),
         country = ifelse(country == 180, "United Kingdom", country),
         country = ifelse(country == 185, "Vietnam", country),
         country = ifelse(country == 138, "Portugal", country),
         country = ifelse(country == 83, "Ireland", country),
         country = ifelse(country == 92, "Kuwait", country),
         country = ifelse(country == 160, "Other", country),
         country = ifelse(country == 69, "Other", country),
         country = ifelse(country == 170, "Other", country),
         country = ifelse(country == 10, "Other", country),
         country = ifelse(country == 34, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),
         country = ifelse(country == 888, "Other", country),)
