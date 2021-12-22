######Hira Imeri   ROB application: Reviewer 2####

library(shiny)
library(RCurl)
library(shinyjs)

source("config.R")
source("shared.R")




postnew <- function(id, user, studyaim,studydesign,otherstudydesign,diagnosis,settings,othersetting,settingsother,countries,regions,ibcheck1,ibcheck2,ibcheck3,ibcheck4,questionf,questions,fccomment,selectn,q1event,q1total,q2event,q2total,followup,q1female,q1male,q1median,q1iqr,q1comment,omitstudyq1,summaryres,asar,asarrisk,asarrisklc,asarriskuc,asarods,asarodslc,asarodsuc,psar,psarrisk,psarrisklc,psarriskuc,psarods,psarodslc,psarodsuc,mildsar,modsar,sevsar,anysar,q3region,prosympmed,prosympmedlc,prosympmeduc,prosarrriskmed,prosarrisklowc,prosarriskupc,prosarcomment,agecheck1,agecheck2,agecheck3,agecheck4,agecheck5){
  data_redcap <- paste0('record_id, ext_gen, aim, design, oth_spec, gen_diag, setting, oth_spec_3, setting2, country, region,fup___1,fup___2,fup___3,fup___4,quest___1,quest___5,comments_gen,q1_nclus,q1_c1_event,q1_c1_total,q1_c2_event,q1_c2_total,q1_fu_descrip,q1_female,q1_male,q1_age_median,q1_age_iqr,comment_q1,q3_sar,q3_sum,q3_sar_a,q3_sae_a_rr,q3_sae_a_rr_lci,q3_sae_a_rr_uci,q3_sae_a_or,q3_sae_a_or_lci,q3_sae_a_or_uci,q3_sar_p,q3_sar_rr,q3_sar_rr_lci,q3_sar_rr_uci,q3_sar_or,q3_sar_or_lci,q3_sar_or_uci,q3_sar_m1,q3_sar_m2,q3_sar_c,q3_sar_s,q3_setting,q3_pa_m,q3_pa_l,q3_pa_u,q3_pp_m,q3_pp_l,q3_pp_u,q3_comments,agerange___1,agerange___2,agerange___3,agerange___4,agerange___5\n',
                        id,',',user,',\"',studyaim,'\"',',',studydesign,',\"',otherstudydesign,'\"',',',diagnosis,',',settings,',\"',othersetting,'\"',',',settingsother,',',countries,',\"',regions,'\"',',',ibcheck1,',',ibcheck2,',',ibcheck3,',',ibcheck4,',',questionf,',',questions,',\"',fccomment,'\"',',',selectn,',\"',q1event,'\"',',\"',q1total,'\"',',\"',q2event,'\"',',\"',q2total,'\"',',\"',followup,'\"',',\"',q1female,'\"',',\"',q1male,'\"',',\"',q1median,'\"',',\"',q1iqr,'\"',',\"',q1comment,'\"',',',omitstudyq1,',\"',summaryres,'\"',',\"',asar,'\"',',\"',asarrisk,'\"',',\"',asarrisklc,'\"',',\"',asarriskuc,'\"',',\"',asarods,'\"',',\"',asarodslc,'\"',',\"',asarodsuc,'\"',',\"',psar,'\"',',\"',psarrisk,'\"',',\"',psarrisklc,'\"',',\"',psarriskuc,'\"',',\"',psarods,'\"',',\"',psarodslc,'\"',',\"',psarodsuc,'\"',',\"',mildsar,'\"',',\"',modsar,'\"',',\"',sevsar,'\"',',\"',anysar,'\"',',\"',q3region,'\"',',\"',prosympmed,'\"',',\"',prosympmedlc,'\"',',\"',prosympmeduc,'\"',',\"',prosarrriskmed,'\"',',\"',prosarrisklowc,'\"',',\"',prosarriskupc,'\"',',\"',prosarcomment,'\"',',',agecheck1,',',agecheck2,',',agecheck3,',',agecheck4,',',agecheck5)
  postRedcap(data_redcap)
  
}


server <- function(input, output, session){
  logname<-paste0(log_dir_rob,  "/", format(Sys.time(), "%Y%m%d_%H%M%S_"), ".csv")
  result <- postForm(
    uri='https://redcap.ispm.unibe.ch/api/',
    token=API, # API of project, MC
    content='report',
    format='csv',
    report_id=reportid, #  from config
    rawOrLabel='raw',
    rawOrLabelHeaders='raw',
    exportCheckboxLabel='false',
    returnFormat='csv'
   )
  #
  # write temp file with retrieved data, timestamped
  filename<-paste0(temp_dir_rob, "/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.csv")
  #
  #
  con <- file(filename, open = "wt", encoding = "UTF-8") # encoding does not properly translate ('portuguese' characters not correct)
  writeLines(result, con = con)
  close(con)
  #
  redcapdb<- read.csv(filename,header=TRUE, stringsAsFactors=FALSE)
  
  #add a new column to concatenate Author names and years
  redcapdb$authoryear <- paste0(redcapdb$author_1, " (", redcapdb$year, ")")
  
  redcapdb$id=redcapdb$record_id
  rvals = reactiveValues()
  
  observeEvent(rvals, {
    output$server_response = renderText(paste0("last response: ", rvals$last))  
  })
  
  observeEvent(input$add, {
    
    user = input$in1
    passw = input$passw
    
    if(user !=""){
      login=ifelse(passw==logins[logins$users==user,]$pw,TRUE,FALSE)
      login2=logins[logins$users==user,]$names
      output$login_response=renderText(paste0("User selected:", logins[logins$users==user,]$names, " login success:",login))
      if(login){
        shinyjs::disable("in1") #disable login
        shinyjs::disable("add")
      }
    }else{
      output$login_response=renderText("Invalid user...")
      login=FALSE
    }
    if(login){
      cat(user, nrow(redcapdb))
      
      #subsetting 
      redcapdb<-subset(redcapdb, redcapdb$ext_gen==user  & redcapdb$aim =="" & redcapdb$comments_gen =="")
      
      redcapdb<-head(redcapdb, row_limit)
      cat(user, nrow(redcapdb))
      
      if(nrow(redcapdb)==0){
        insertUI(
          selector = "#add",
          where = "afterEnd",
          ui = fluidRow(id= "nodata",
                        column(7, HTML("No data to show: You have either completed all your tasks, or no tasks have been assigned<br><br><b>Please check back tomorrow!</b>"))))
        
      }else{
        redcapdb<- redcapdb[seq(dim(redcapdb)[1],1),] #also reverse because shiny builds page bottom up
        for (i in 1:nrow(redcapdb)) {
          insertUI(
            selector = "#add",
            where = "afterEnd",
            ui = fluidRow(id= paste0("rcid_",redcapdb$id[i]),
                          column(6, br(),
                                 HTML(paste0("<font size=8><b>REDCap ID: ",redcapdb$id[i],"</b></font>")),
                                 br(), br(),
                                 HTML(paste0("<font size=4>Title: ",redcapdb$title[i],",",redcapdb$authoryear[i],"</b></font>")),
                                 br(),br(),
                                 HTML("<a target=\"_blank\" href=\"",redcapdb$url[i],"\" >Link to the full text</a>"),
                                 br(),br(),
                                
                                 HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=8>General Information</font></mark></b><br><br>"),
                                 #aim
                                 textAreaInput(paste0("studyaim",redcapdb$id[i]),HTML("<font size=4>Aim/objective of the study:</font><font color=\"#FF0000\"> *</font>"), "",
                                               width = 380, height = 150),
                                 br(),
                                 #design
                                 selectInput(paste0("studydesign", redcapdb$id[i]), HTML("<font size=4>Study design:</font><font color=\"#FF0000\"> *</font>"),
                                             choices = c("Case series" = 1, "Cross sectional study" = 2, "Cohort study" = 3, "Mathematical modelling study" = 4, "Case-control study" = 5, "Outbreak Investigation" = 6, "Other" = 88),
                                             selected = FALSE,
                                             multiple = FALSE,
                                             selectize = FALSE, size = 7, width = 380),
                                 br(),
                                 #oth_spec
                                 conditionalPanel(condition = paste0('input.studydesign', redcapdb$id[i], " =='88'"),
                                                  textInput(paste0("otherstudydesign", redcapdb$id[i]),HTML("<font size=4>Please specify 'other' study design:</font>"), "", width = 380)),
                                 #textInput(paste0("otherstudydesign", redcapdb$id[i]),HTML("Please specify other study design:"), "", width = 380),
                                 br(),
                                 #gen_diag
                                 selectInput(paste0("diagnosis", redcapdb$id[i]),HTML("<font size=4>SARS-CoV-2 infection diagnosed by:</font><font color=\"#FF0000\"> *</font>"),
                                             choices = c("PCR" = 1, "Serology" = 2, "Both PCR and Serology" = 3),
                                             selected = FALSE,
                                             multiple = FALSE,
                                             selectize = FALSE, size = 3, width = 380),
                                 br(),
                                 #setting
                                 selectInput(paste0("settings", redcapdb$id[i]), HTML("<font size=4>Setting of data collection:</font><font color=\"#FF0000\"> *</font>"),
                                             choices = c("Cruise ship" = 1, "Hospital" = 2,
                                                         "Traveler/evacuee from an affected area" = 3,
                                                         "Contact tracing" = 6, "Health care worker" = 7,
                                                         "Family cluster" = 9, "Other" = 88),
                                             selected = FALSE,
                                             multiple = FALSE,
                                             selectize = FALSE, size = 7, width = 380),
                                 br(),
                                 #oth_spec_3
                                 conditionalPanel(condition = paste0('input.settings', redcapdb$id[i], " =='88'"),
                                                  textInput(paste0("othersetting", redcapdb$id[i]),HTML("<font size=4>Please specify 'other' setting:</font>"), "", width = 380)),
                                 br(),
                                 
                                 #setting2
                                 selectInput(paste0("settingsother", redcapdb$id[i]), HTML("<font size=4>Setting 2 [used in Q1, Q2 analysis]</font><font color=\"#FF0000\"> *</font>"),
                                             choices = c("1. Contact investigation" = 1, "2. Contact investigation, aggregated" = 2,
                                                         "3. Outbreak investigation" = 3,
                                                         "4. Statistical model" = 4, "5. Screening" = 5,
                                                         "6. Hospitalised adults" = 6, "7. Hospitalised children" = 7,
                                                         "8. Hospitalised children and adults" = 8, "9. Screening: institutional setting" = 9,
                                                         "10. Screening: community setting" = 10, "11. Screening: occupational" = 11),
                                             selected = FALSE,
                                             multiple = FALSE,
                                             selectize = FALSE, size = 11, width = 380),
                                 br(),
                                 #country
                                 selectInput(paste0("countries", redcapdb$id[i]),HTML("<font size=4>Country:</font><font color=\"#FF0000\"> *</font>"),
                                             choices = c("Afghanistan" = 1, "Albania" = 2, "Algeria" = 3, "Andorra"= 4,"Angola" = 5, "Antigua and Barbuda" = 6,"Argentina" = 7, "Armenia" = 8, "Aruba" = 9, "Australia" = 10,
                                                         "Austria" = 11, "Azawad" = 12, "Azerbaijan" = 13, "Bahamas" = 14, "Bahrain" = 15, "Bangladesh" = 16, "Barbados" = 17, "Belarus" = 18, "Belgium" = 19, "Belize" = 20,
                                                         "Benin" = 21, "Bermuda" = 22, "Bhutan" = 23, "Bolivia" = 24, "Bosnia and Herzegovina" = 25, "Botswana" = 26, "Brazil" = 27, "Brunei" = 28, "Bulgaria" = 29, "Burkina Faso" = 30,
                                                         "Burundi" = 31, "Cambodia" = 32, "Cameroon" = 33, "Canada" = 34, "Cape Verde" = 35, "Central African Republic" = 36, "Chad" = 37, "Chechnya" = 38, "Chile" = 39, "China" = 40,
                                                         "Colombia" = 41, "Comoros" = 42, "Congo" = 43, "Costa Rica" = 44, "Cote d'Ivoire" = 45, "Croatia" = 46, "Cuba" = 47, "Curacao" = 48, "Cyprus" = 49, "Czechoslovakia" = 50,
                                                         "Denmark" = 51, "Dominican Republic" = 52, "Ecuador" = 53, "Egypt" = 54, "El Salvador" = 55, "Equatorial Guinea" = 56, "Eritrea" = 57, "Estonia" = 58, "Ethiopia" = 59, "Fiji" = 60,
                                                         "Finland" = 61, "France" = 62, "French Guyana" = 63, "Gabon" = 64, "Gambia" = 65, "Georgia" = 66, "Germany" = 67, "Ghana" = 68, "Greece" = 69, "Greenland" = 70,
                                                         "Guatemala" = 71, "Guinea" = 72, "Guyana" = 73, "Haiti" = 74, "Honduras" = 75, "Hong Kong" = 76, "Hungary" = 77, "Iceland" = 78, "India" = 79, "Indonesia" = 80,
                                                         "Iran" = 81, "Iraq" = 82, "Ireland" = 83, "Israel" = 84, "Italy" = 85, "Jamaica" = 86, "Japan" = 87, "Jordan" = 88, "Kazakhstan" = 89, "Kenya" = 90,
                                                         "Kosovo" = 91, "Kuwait" = 92, "Kyrgyzstan" = 93, "Laos" = 94, "Latvia" = 95, "Lebanon" = 96, "Liberia" = 97, "Libya" = 98, "Liechtenstein" = 99, "Lithuania" = 100,
                                                         "Luxembourg" = 101, "North Macedonia" = 102, "Madagascar" = 103, "Malawi" = 104, "Malaysia" = 105, "Maldives" = 106, "Mali" = 107, "Malta" = 108, "Mauritius" = 109, "Mexico" = 110,
                                                         "Moldova" = 111, "Monaco" = 112, "Mongolia" = 113, "Montenegro" = 114, "Morocco" = 115, "Mozambique" = 116, "Myanmar" = 117, "Multiple" = 118, "Namibia" = 119, "Nepal" = 120,
                                                         "Netherlands" = 121, "New Zealand" = 122, "Nicaragua" = 123, "Niger" = 124, "Nigeria" = 125, "North Korea" = 126, "North Yemen" = 127, "Norway" = 128, "Oman" = 129, "Pakistan" = 130,
                                                         "Palestine" = 131, "Panama" = 132, "Papua New Guinea" = 133, "Paraguay" = 134, "Peru" = 135, "Philippines" = 136, "Poland" = 137, "Portugal" = 138, "Puerto Rico" = 139, "Qatar" = 140,
                                                         "Romania" = 141, "Russia" = 142, "Rwanda" = 143, "Samoa" = 144, "San Marino" = 145, "Saudi Arabia" = 146, "Senegal" = 147, "Serbia" = 148, "Seychelles" = 149, "Sierra Leone" = 150,
                                                         "Singapore" = 151, "Slovakia" = 152, "Slovenia" = 153, "Solomon Islands" = 154, "Somalia" = 155, "South Africa" = 156, "South Korea" = 157, "South Sudan" = 158, "South Yemen" = 159, "Spain" = 160,
                                                         "Sri Lanka" = 161,"Sudan" = 162, "Suriname" = 163, "Swaziland" = 164, "Sweden" = 165, "Switzerland" = 166, "Syria" = 167, "Tajikistan" = 168, "Tanzania" = 169,"Thailand" = 170,
                                                         "Tibet" = 171, "Togo" = 172, "Tunisia" = 173, "Turkey" = 174, "Turkmenistan" = 175, "Uganda" = 176, "Ukraine" = 177, "United Arab Emirates" = 178,"United Arab Republic" = 179,"United Kingdom" = 180,
                                                         "United States of America" = 181, "Uruguay" = 182, "Uzbekistan" = 183, "Venezuela" = 184, "Vietnam" = 185, "Yemen" = 186, "Yugoslavia" = 187,"Zambia" = 188, "Zanzibar" = 189,"Zimbabwe" = 190,
                                                         "Sao Tome and Principe" = 191,"More than one country" = 200,"Other" = 888),
                                             selected = FALSE,
                                             multiple = FALSE,
                                             selectize = FALSE, width = 380),
                                 br(),
                                 #region
                                 textInput(paste0("regions", redcapdb$id[i]), HTML("<font size=4>Region:</font><font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\">If no comments, please write 'NA'</span>"), "", width = 380),
                                 br(),
                                 #fup
                                 HTML("<b><font size=4>Duration of follow-up time (choose all that apply):<font color=\"#FF0000\"> *</font></font></b>"), 
                                 checkboxInput(paste0('ibcheck1', redcapdb$id[i]), '14 days after the last possible exposure date', value = FALSE, width = NULL),
                                 checkboxInput(paste0('ibcheck2', redcapdb$id[i]), '7 days after diagnosis', value = FALSE, width = NULL),
                                 checkboxInput(paste0('ibcheck3', redcapdb$id[i]), 'Until negative PCR result', value = FALSE, width = NULL),
                                 checkboxInput(paste0('ibcheck4', redcapdb$id[i]), '>7 days after diagnosis', value = FALSE, width = NULL),
                                 br(),
                                 HTML("<b><font size=4>Which questions are addressed in this study?<font color=\"#FF0000\"> *</font></font></b>"), 
                                 checkboxInput(paste0('questionf', redcapdb$id[i]), 'Question 1: Amongst people who become infected with SARS-CoV-2, what proportion does not experience symptoms at all during their infection?', value = FALSE, width = NULL),
                                 checkboxInput(paste0('questions', redcapdb$id[i]), HTML('<b>Question 2.1:</b> What is the secondary attack rate (SAR) from asymptomatic or pre-symptomatic index cases?<br><b>OR</b><br><b>Question 2.2:</b> What proportion of SARS-CoV-2 infections is accounted for by people who are either asymptomatic throughout infection, or pre-symptomatic?'), value = FALSE, width = NULL),
                                 HTML("<b><font size=3><mark style=\"background-color:#FFFF00\"> -  Fields related to questions appear on the right side! (Scroll up!)</mark></font></b>"), 
                                 br(),
                                 br(),
                                 br(),
                                 #comments_gen
                                 textAreaInput(paste0("fccomment", redcapdb$id[i]), HTML("<font size=4>Comments</font><font color=\"#FF0000\"> *</font>"), "", width = 400, height = 150),
                                 br(),
                                 br(),
                                 br(),
                                 HTML("<font size=5><strong>----------------------------------------------------------------------------</font></strong><br>"),br(),br(),
                                 actionButton(paste0('c', redcapdb$id[i]), paste0("Submit:",redcapdb$id[i]),style='padding:30px; font-size:120%', width = 300),br(),br(),br(),
                                 HTML("<font size=4><strong>Please check all mandatory fields (<font color=\"#FF0000\">*</font>) are complete before submitting!</strong></font><br>"),br(),br(),
                                 HTML("<font size=5><strong>----------------------------------------------------------------------------</font></strong><br>")
                          ),
                          column(5, br(), br(), br(), br(), br(), br(), br(), br(), br(),br(),br(),
                                 #q1_nclus
                                 conditionalPanel(condition = paste0('input.questionf', redcapdb$id[i], " =='1'"),
                                                  HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Question 1</font></mark></b><br>"),
                                                  selectInput(paste0('selectn', redcapdb$id[i]),HTML("Number of clusters described:"),
                                                              choices = c("1" = 1, "2" = 2),
                                                              selected = FALSE,
                                                              multiple = FALSE,
                                                              selectize = FALSE, size = 2, width = 380),
                                                  #q1_c1_event
                                                  textInput(paste0("q1event", redcapdb$id[i]), HTML("Number of individuals that were asymptomatic throughout infection:"), "", width = 380),
                                                  #q1_c1_total
                                                  textInput(paste0("q1total", redcapdb$id[i]), HTML("Total number of individuals diagnosed with SARS-COV-2 (denominator)::"), "", width = 380),
                                                  conditionalPanel(condition = paste0('input.selectn', redcapdb$id[i], " =='2'"),
                                                                   #q1_c2_event
                                                                   textInput(paste0("q2event", redcapdb$id[i]), HTML("Number of individuals that were asymptomatic throughout infection [cluster 2]:"), "", width = 380),
                                                                   #q1_c2_total
                                                                   textInput(paste0("q2total", redcapdb$id[i]), HTML("Total number of individuals (denominator) [cluster 2]:"), "", width = 380)
                                                                   ),
                                                  HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=3>Follow-up time</font></mark></b>"),
                                                  #q1_fu_descrip
                                                  textAreaInput(paste0("followup", redcapdb$id[i]), HTML("Please describe the follow-up as reported in the study<font color=\"#FF0000\"> *</font>"), "", width = 400, height = 150),
                                                  
                                                  #SEX
                                                  HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=3>Sex</font></mark></b>"),
                                                  #q1_female
                                                  textInput(paste0("q1female", redcapdb$id[i]), HTML("Number of females who remained asymptomatic throughout infection:<font color=\"#FF0000\"> *</font> (If numbers are not reported, please write '9999')"), "", width = 380),
                                                  #q1_male
                                                  textInput(paste0("q1male", redcapdb$id[i]), HTML("Number of males who remained asymptomatic throughout infection:<font color=\"#FF0000\"> *</font> (If numbers are not reported, please write '9999')"), "", width = 380),
                                                  #AGE
                                                  HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=3>Age</font></mark></b>"),
                                                  #q1_age_median
                                                  textInput(paste0("q1median", redcapdb$id[i]), HTML("Enter median age of all people who remained asymptomatic throughout infection:<font color=\"#FF0000\"> *</font> (If numbers are not reported, please write '9999')"), "", width = 380),
                                                  #q1_age_iqr
                                                  textInput(paste0("q1iqr", redcapdb$id[i]), HTML("Enter IQR for age of all people who remained asymptomatic throughout infection:<font color=\"#FF0000\"> *</font> (If numbers are not reported, please write '9999')"), "", width = 380),
                                                  #fup
                                                  HTML("<b>Age range of asymptomatic <br> Select one or more if applicable<font color=\"#FF0000\"> *</font></b>"), 
                                                  checkboxInput(paste0('agecheck1', redcapdb$id[i]), 'Children (<18 years)', value = FALSE, width = NULL),
                                                  checkboxInput(paste0('agecheck2', redcapdb$id[i]), 'Adults (18-65 years)', value = FALSE, width = NULL),
                                                  checkboxInput(paste0('agecheck3', redcapdb$id[i]), 'Older adults (>65 years)', value = FALSE, width = NULL),
                                                  checkboxInput(paste0('agecheck4', redcapdb$id[i]), 'All ages', value = FALSE, width = NULL),
                                                  checkboxInput(paste0('agecheck5', redcapdb$id[i]), 'Not reported', value = FALSE, width = NULL),
                                                  #comment_q1
                                                  textAreaInput(paste0("q1comment", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 400, height = 150),
                                 ),
                                 conditionalPanel(condition = paste0('input.questions', redcapdb$id[i], " =='1'"),
                                                  HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Question 2.1 OR 2.2</font></mark></b><br>"),br(),
                                                  #q3_sar
                                                  radioButtons(paste0('omitstudyq1', redcapdb$id[i]),HTML("Omit from meta-analysis<font color=\"#FF0000\"> *</font>"),
                                                               choices = c("Q. 2.1 SAR" = 1, "Q. 2.2 Transmission proportion" = 0, "Both" = 2),
                                                               selected = FALSE, width = 380),
                                                  #q3_sum
                                                  textAreaInput(paste0("summaryres", redcapdb$id[i]), HTML("Summary of results:<font color=\"#FF0000\"> *</font>"), "", width = 400, height = 150),
                                                  conditionalPanel(condition = paste(paste0('input.omitstudyq1', redcapdb$id[i], " =='1'"), paste0('input.omitstudyq1', redcapdb$id[i], " =='2'"), sep = " || "),
                                                                   HTML("Question 2.1. What is the secondary attack rate from asymptomatic or pre-symptomatic index cases?<br>SAR (Secondary attack rate)<br>Number of infections (numerator) caused by certain group of people: Here asymptomatics vs symptomatics (denominator)"),
                                                                   #q3_sar_a
                                                                   textInput(paste0("asar", redcapdb$id[i]), HTML("Asymptomatic SAR"), "", width = 380),
                                                                   #q3_sae_a_rr
                                                                   textInput(paste0("asarrisk", redcapdb$id[i]), HTML("Asymptomatic SAR Risk Ratio (RR)"), "", width = 380),
                                                                   #q3_sae_a_rr_lci
                                                                   textInput(paste0("asarrisklc", redcapdb$id[i]), HTML("Asymptomatic  SAR Risk Ratio (RR) Lower Confidence Interval"), "", width = 380),
                                                                   #q3_sae_a_rr_uci
                                                                   textInput(paste0("asarriskuc", redcapdb$id[i]), HTML("Asymptomatic  SAR Risk Ratio (RR) Upper Confidence Interval"), "", width = 380),
                                                                   #q3_sae_a_or
                                                                   textInput(paste0("asarods", redcapdb$id[i]), HTML("Asymptomatic SAR Odds Ratio (OR)"), "", width = 380),
                                                                   #q3_sae_a_or_lci
                                                                   textInput(paste0("asarodslc", redcapdb$id[i]), HTML("Asymptomatic  SAR Odds Ratio (OR) Lower Confidence Interval"), "", width = 380),
                                                                   #q3_sae_a_or_uci
                                                                   textInput(paste0("asarodsuc", redcapdb$id[i]), HTML("Asymptomatic  SAR Odds Ratio (OR) Upper Confidence Interval"), "", width = 380),
                                                                   #q3_sar_p
                                                                   textInput(paste0("psar", redcapdb$id[i]), HTML("Pre-symptomatic SAR"), "", width = 380),
                                                                   #q3_sar_rr
                                                                   textInput(paste0("psarrisk", redcapdb$id[i]), HTML("Pre-symptomatic SAR Risk Ratio (RR)"), "", width = 380),
                                                                   #q3_sar_rr_lci
                                                                   textInput(paste0("psarrisklc", redcapdb$id[i]), HTML("Pre-symptomatic  SAR Risk Ratio (RR) Lower Confidence Interval"), "", width = 380),
                                                                   #q3_sar_rr_uci
                                                                   textInput(paste0("psarriskuc", redcapdb$id[i]), HTML("Pre-symptomatic  SAR Risk Ratio (RR) Upper Confidence Interval"), "", width = 380),
                                                                   #q3_sar_or
                                                                   textInput(paste0("psarods", redcapdb$id[i]), HTML("Pre-symptomatic SAR Odds Ratio (OR)"), "", width = 380),
                                                                   #q3_sar_or_lci
                                                                   textInput(paste0("psarodslc", redcapdb$id[i]), HTML("Pre-symptomatic  SAR Odds Ratio (OR) Lower Confidence Interval"), "", width = 380),
                                                                   #q3_sar_or_uci
                                                                   textInput(paste0("psarodsuc", redcapdb$id[i]), HTML("Pre-symptomatic  SAR Odds Ratio (OR) Upper Confidence Interval"), "", width = 380),
                                                                   #q3_sar_m1
                                                                   textInput(paste0("mildsar", redcapdb$id[i]), HTML("Mild symptomatic SAR "), "", width = 380),
                                                                   #q3_sar_m2
                                                                   textInput(paste0("modsar", redcapdb$id[i]), HTML("Moderate symptomatic SAR "), "", width = 380),
                                                                   #q3_sar_c
                                                                   textInput(paste0("sevsar", redcapdb$id[i]), HTML("Severe/critical symptomatic SAR "), "", width = 380),
                                                                   #q3_sar_s
                                                                   textInput(paste0("anysar", redcapdb$id[i]), HTML("ANY symptomatic SAR "), "", width = 380)
                                                                   #HTML("<b><font size=1> -  Please scroll up to submit the record!</font></b>"), 
                                                                   ),
                                                  conditionalPanel(condition = paste(paste0('input.omitstudyq1', redcapdb$id[i], " =='0'"), paste0('input.omitstudyq1', redcapdb$id[i], " =='2'"), sep = " || "),
                                                                   HTML("Question 2.2: What proportion of SARS-CoV-2 infections is accounted for by people who are either asymptomatic throughout infection, or pre-symptomatic?"),
                                                                   #q3_setting
                                                                   textInput(paste0("q3region", redcapdb$id[i]), HTML("Region/setting"), "", width = 380),
                                                                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=2>Asymptomatic transmission</font></mark></b><br>"),
                                                                   #q3_pa_m
                                                                   textInput(paste0("prosympmed", redcapdb$id[i]), HTML("Proportion asymptomatic transmission (median)"), "", width = 380),
                                                                   #q3_pa_l
                                                                   textInput(paste0("prosympmedlc", redcapdb$id[i]), HTML("Proportion asymptomatic transmission (lower CrI)"), "", width = 380),
                                                                   #q3_pa_u
                                                                   textInput(paste0("prosympmeduc", redcapdb$id[i]), HTML("Proportion asymptomatic transmission (upper CrI)"), "", width = 380),
                                                                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=2>Pre-symptomatic transmission</font></mark></b><br>"),
                                                                   #q3_pp_m
                                                                   textInput(paste0("prosarrriskmed", redcapdb$id[i]), HTML("Proportion pre-symptomatic transmission (median)"), "", width = 380),
                                                                   #q3_pp_l
                                                                   textInput(paste0("prosarrisklowc", redcapdb$id[i]), HTML("Proportion pre-symptomatic transmission (lower CrI)"), "", width = 380),
                                                                   #q3_pp_u
                                                                   textInput(paste0("prosarriskupc", redcapdb$id[i]), HTML("Proportion pre-symptomatic transmission (upper CrI)"), "", width = 380),
                                                                   #q3_comments
                                                                   textInput(paste0("prosarcomment", redcapdb$id[i]), HTML("Comment: "), "", width = 380)
                                                                   )
                                   
                                   
                                 )
                                 
                          )
                          
            )
          )
          
        }
      }
      
    }
  })
  
  
  observe({
    invalidateLater(100000000,session)
  })


  
  
  lapply(
    X = 1:nrow(redcapdb),
    FUN = function(j){
      observeEvent(input[[paste0("c", redcapdb$id[j])]], {
        cat("click\n")
        studyaim = input[[paste0('studyaim', redcapdb$id[j])]]
        studydesign = input[[paste0('studydesign', redcapdb$id[j])]]
        otherstudydesign = input[[paste0("otherstudydesign", redcapdb$id[j])]]
        diagnosis = input[[paste0('diagnosis', redcapdb$id[j])]]
        settings = input[[paste0('settings', redcapdb$id[j])]]
        othersetting = input[[paste0('othersetting', redcapdb$id[j])]]
        settingsother = input[[paste0('settingsother', redcapdb$id[j])]]
        countries = input[[paste0('countries', redcapdb$id[j])]]
        regions = input[[paste0('regions', redcapdb$id[j])]]
        ibcheck1 = input[[paste0('ibcheck1', redcapdb$id[j])]]
        ibcheck1 = ifelse(ibcheck1=="FALSE", 0,1)
        ibcheck2 = input[[paste0('ibcheck2', redcapdb$id[j])]]
        ibcheck2 = ifelse(ibcheck2=="FALSE", 0,1)
        ibcheck3 = input[[paste0('ibcheck3', redcapdb$id[j])]]
        ibcheck3 = ifelse(ibcheck3=="FALSE", 0,1)
        ibcheck4 = input[[paste0('ibcheck4', redcapdb$id[j])]]
        ibcheck4 = ifelse(ibcheck4=="FALSE", 0,1)
        questionf = input[[paste0('questionf', redcapdb$id[j])]]
        questionf = ifelse(questionf=="FALSE", 0,1)
        questions = input[[paste0('questions', redcapdb$id[j])]]
        questions = ifelse(questions=="FALSE", 0,1)
        fccomment = input[[paste0("fccomment", redcapdb$id[j])]]
        selectn = input[[paste0("selectn", redcapdb$id[j])]]
        q1event = input[[paste0("q1event", redcapdb$id[j])]]
        q1total = input[[paste0("q1total", redcapdb$id[j])]]
        q2event = input[[paste0("q2event", redcapdb$id[j])]]
        q2total = input[[paste0("q2total", redcapdb$id[j])]]
        followup = input[[paste0("followup", redcapdb$id[j])]]
        q1female = input[[paste0("q1female", redcapdb$id[j])]]
        q1male = input[[paste0("q1male", redcapdb$id[j])]]
        q1median = input[[paste0("q1median", redcapdb$id[j])]]
        q1iqr = input[[paste0("q1iqr", redcapdb$id[j])]]
        q1comment = input[[paste0("q1comment", redcapdb$id[j])]]
        omitstudyq1 = input[[paste0("omitstudyq1", redcapdb$id[j])]]
        summaryres = input[[paste0("summaryres", redcapdb$id[j])]]
        asar = input[[paste0("asar", redcapdb$id[j])]]
        asarrisk = input[[paste0("asarrisk", redcapdb$id[j])]]
        asarrisklc = input[[paste0("asarrisklc", redcapdb$id[j])]]
        asarriskuc = input[[paste0("asarriskuc", redcapdb$id[j])]]
        asarods = input[[paste0("asarods", redcapdb$id[j])]]
        asarodslc = input[[paste0("asarodslc", redcapdb$id[j])]]
        asarodsuc = input[[paste0("asarodsuc", redcapdb$id[j])]]
        psar = input[[paste0("psar", redcapdb$id[j])]]
        psarrisk = input[[paste0("psarrisk", redcapdb$id[j])]]
        psarrisklc = input[[paste0("psarrisklc", redcapdb$id[j])]]
        psarriskuc = input[[paste0("psarriskuc", redcapdb$id[j])]]
        psarods = input[[paste0("psarods", redcapdb$id[j])]]
        psarodslc = input[[paste0("psarodslc", redcapdb$id[j])]]
        psarodsuc = input[[paste0("psarodsuc", redcapdb$id[j])]]
        mildsar = input[[paste0("mildsar", redcapdb$id[j])]]
        modsar = input[[paste0("modsar", redcapdb$id[j])]]
        sevsar = input[[paste0("sevsar", redcapdb$id[j])]]
        anysar = input[[paste0("anysar", redcapdb$id[j])]]
        q3region = input[[paste0("q3region", redcapdb$id[j])]]
        prosympmed = input[[paste0("prosympmed", redcapdb$id[j])]]
        prosympmedlc = input[[paste0("prosympmedlc", redcapdb$id[j])]]
        prosympmeduc = input[[paste0("prosympmeduc", redcapdb$id[j])]]
        prosarrriskmed = input[[paste0("prosarrriskmed", redcapdb$id[j])]]
        prosarrisklowc = input[[paste0("prosarrisklowc", redcapdb$id[j])]]
        prosarriskupc = input[[paste0("prosarriskupc", redcapdb$id[j])]]
        prosarcomment = input[[paste0("prosarcomment", redcapdb$id[j])]]
        agecheck1 = input[[paste0('agecheck1', redcapdb$id[j])]]
        agecheck1 = ifelse(agecheck1=="FALSE", 0,1)
        agecheck2 = input[[paste0('agecheck2', redcapdb$id[j])]]
        agecheck2 = ifelse(agecheck2=="FALSE", 0,1)
        agecheck3 = input[[paste0('agecheck3', redcapdb$id[j])]]
        agecheck3 = ifelse(agecheck3=="FALSE", 0,1)
        agecheck4 = input[[paste0('agecheck4', redcapdb$id[j])]]
        agecheck4 = ifelse(agecheck4=="FALSE", 0,1)
        agecheck5 = input[[paste0('agecheck5', redcapdb$id[j])]]
        agecheck5 = ifelse(agecheck5=="FALSE", 0,1)
        
        id = redcapdb$id[j]
        user = input$in1
        req(studyaim, studydesign, diagnosis, settings, settingsother, countries, regions, fccomment)
        
        #post function
        response = postnew(id, user, studyaim, studydesign, otherstudydesign, diagnosis, settings, othersetting, settingsother, countries, regions, ibcheck1, ibcheck2, ibcheck3, ibcheck4, questionf, questions, fccomment,selectn,q1event,q1total,q2event,q2total,followup,q1female,q1male,q1median,q1iqr,q1comment,omitstudyq1,summaryres,asar,asarrisk,asarrisklc,asarriskuc,asarods,asarodslc,asarodsuc,psar,psarrisk,psarrisklc,psarriskuc,psarods,psarodslc,psarodsuc,mildsar,modsar,sevsar,anysar,q3region,prosympmed,prosympmedlc,prosympmeduc,prosarrriskmed,prosarrisklowc,prosarriskupc,prosarcomment,agecheck1,agecheck2,agecheck3,agecheck4,agecheck5)
        rvals$last = paste0('Updated: ', id, " response: ", response)
        removeUI(paste0("#rcid_",redcapdb$id[j]))
        
        
        
        
        
        
      })
      
    }
  )
  
}