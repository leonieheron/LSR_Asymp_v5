###############################################################
# Supplementary material                        ##########
# Q1 - studies ordered by precision                   ##########
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

# minor cleaning
asymptomaticQ1[asymptomaticQ1 == 9999] = NA

asymptomaticQ1$setting = asymptomaticQ1$setting2


data_long1 <- gather(asymptomaticQ1, cluster, total, c(q1_c1_total,q1_c2_total,q1_c3_total), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(record_id, author_1, total, id, cluster)
data_long2 <-gather(asymptomaticQ1, cluster, events, c(q1_c1_event,q1_c2_event,q1_c3_event), factor_key=TRUE) %>% 
  mutate(id=1:nrow(.)) %>%
  select(events, id)

data_Q1 = merge(data_long1, data_long2, by="id")
data_Q1 = data_Q1[!is.na(data_Q1$total),]
data_Q1$cluster = factor(data_Q1$cluster, labels = c("1","2","3"), levels = c("q1_c1_total", "q1_c2_total", "q1_c3_total"))

data_Q1[data_Q1$record_id %in% asymptomaticQ1[is.na(asymptomaticQ1$q1_c2_event),]$record_id,]$cluster=NA

asymptomaticQ1 <- data_Q1

asymptomaticQ1$label=paste0(asymptomaticQ1$author_1, " (2020)",
                            ifelse(!is.na(asymptomaticQ1$cluster),
                                   paste0(" cluster:",asymptomaticQ1$cluster),
                                   "")) # [FU: ",asymptomaticQ1$fup_median,"]")

asymptomaticQ1$label=paste0(asymptomaticQ1$author_1,
                            ifelse(!is.na(asymptomaticQ1$cluster),
                                   paste0(" [cluster:",asymptomaticQ1$cluster,
                                          "]"),"")) # [FU: ",asymptomaticQ1$fup_median,"]")

asym_plot_by_precision <- metaprop(events, total, data = asymptomaticQ1, sm = "PLOGIT", 
                                   studlab = label)
pdf(file = 'forest_meta_Q1_precision.pdf', width = 10, height = 27) 

forest(asym_plot_by_precision, sortvar = (1/(sqrt(seTE))))


dev.off()

png(file = 'forest_meta_Q1_precision.png', res = 300, height = 60, width = 20, units = "cm")

forest(asym_plot_by_precision, sortvar = (1/(sqrt(seTE))))

dev.off()


