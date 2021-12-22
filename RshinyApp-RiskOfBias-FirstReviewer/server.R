######ROB application: Reviewer 1####

library(shiny)
library(RCurl)
library(shinyjs)

source("config.R")
source("shared.R")

#1. sb - selection bias 2. rr - responnse rate 
#3. sbrr - selection bias & response rate 4. ib - information bias
#5. si - selective or incomplete 6. final comments - fc


postnew <- function(id, user, sbreport,sbselect,sbcomments,rrselect1,rrselect2,rrcomment1,rrcomment2,rrcomment3,rrcomment4,sbrrselect,sbrrcomment,ibcomment,ibselect,ibcomment2,ibselect2,ibcomment3,ibcheck1,ibcheck2,ibcheck3,ibcheck4,ibcheck5,ibcomment4,ibselect3,ibcomment5,siselect1,siselect2,sicomment,fcselect,fccomment,robcluster,sbreportc2,sbselectc2,rrselect1c2,rrselect2c2,rrcomment1c2,rrcomment2c2,rrcomment3c2,sbrrselectc2,ibselectc2,ibselect2c2,ibcheck1c2,ibcheck2c2,ibcheck3c2,ibcheck4c2,ibcheck5c2,ibcheck6c2,ibselect3c2,siselect1c2,siselect2c2,fcselectc2){
  data_redcap <- paste0('record_id, rob_reviewer_1, r1_target_pop, r1_rob_q1, r1_comment_1, r1_response_rate, r1_response_calc, r1_participants, r1_eligible, r1_response_prop,r1_response_comment, r1_rob_q2, r1_comment_2, r1_symp_def, r1_rob_q3,r1_comment_3,r1_rob_q4,r1_comment_4,r1_follow_up___1,r1_follow_up___2,r1_follow_up___3,r1_follow_up___4,r1_follow_up___5,r1_viral_load,r1_rob_q5,r1_comment_5,r1_symp_stat,r1_rob_q6,r1_comment_6,r1_risk,r1_comment_7,rob_cluster_desc,r1_target_pop_c2,r1_rob_q1_c2,r1_response_rate_c2,r1_response_calc_c2,r1_participants_c2,r1_eligible_c2,r1_response_prop_c2,r1_rob_q2_c2,r1_rob_q3_c2,r1_rob_q4_c2,r1_follow_up_c2___1,r1_follow_up_c2___2,r1_follow_up_c2___3,r1_follow_up_c2___4,r1_follow_up_c2___5,r1_follow_up_c2___6,r1_rob_q5_c2,r1_symp_stat_c2,r1_rob_q6_c2,r1_risk_c2\n',
                        id,',',user,',\"',sbreport,'\"',',',sbselect,',\"',sbcomments,'\"',',',rrselect1,',',rrselect2,',\"',rrcomment1,'\"',',\"',rrcomment2,'\"',',\"',rrcomment3,'\"',',\"',rrcomment4,'\"',',',sbrrselect,',\"',sbrrcomment,'\"',',\"',ibcomment,'\"',',',ibselect,',\"',ibcomment2,'\"',',',ibselect2,',\"',ibcomment3,'\"',',',ibcheck1,',',ibcheck2,',',ibcheck3,',',ibcheck4,',',ibcheck5,',\"',ibcomment4,'\"',',',ibselect3,',\"',ibcomment5,'\"',',',siselect1,',',siselect2,',\"',sicomment,'\"',',',fcselect,',\"',fccomment,'\"',',\"',robcluster,'\"',',\"',sbreportc2,'\"',',',sbselectc2,',',rrselect1c2,',',rrselect2c2,',\"',rrcomment1c2,'\"',',\"',rrcomment2c2,'\"',',\"',rrcomment3c2,'\"',',',sbrrselectc2,',',ibselectc2,',',ibselect2c2,',',ibcheck1c2,',',ibcheck2c2,',',ibcheck3c2,',',ibcheck4c2,',',ibcheck5c2,',',ibcheck6c2,',',ibselect3c2,',',siselect1c2,',',siselect2c2,',',fcselectc2)
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
      #subsetting of the data
      #redcapdb<-subset(redcapdb, redcapdb$rob_reviewer_1==user)
      redcapdb<-subset(redcapdb, redcapdb$rob_reviewer_1==user & redcapdb$r1_target_pop=="" & redcapdb$r1_comment_1=="")
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
          ui = fluidPage(id= paste0("rcid_",redcapdb$id[i]),
            column(9, br(),
                   HTML(paste0("<font size=6><b>REDCap ID: ",redcapdb$id[i],"</b></font>")),
                   br(),
                   HTML(redcapdb$authoryear[i]),
                   br(),
                   HTML("<a target=\"_blank\" href=\"",redcapdb$url[i],"\" >Link to the full text</a>"),
                   br(),
                   HTML(paste("Number of clusters: "), redcapdb$q1_nclus[i]),
                   br(),
                   HTML("NA = Not applicable"),
                   br(), br(),
                   
                   #Cluster check
                   HTML("<b><font size=2.5>Are there two clusters reported in this study? (please see above)<font color=\"#FF0000\"> *</font></font></b>"),
                   checkboxInput(paste0('helpme', redcapdb$id[i]), 'No', value = FALSE, width = NULL),
                   
                   #rob_cluster_desc
                   textAreaInput(paste0("robcluster",redcapdb$id[i]),HTML("<font color=\"#0066CC\">If the study reports more than one cluster, please describe the participants of cluster 1 and cluster 2:</font><font color=\"#FF0000\"> *</font>"), "",
                                 width = 380, height = 150),
                   
                   
                   #Selection Bias
                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Selection Bias</font></mark></b><br><br>"),
                   textAreaInput(paste0("sbreport", redcapdb$id[i]),HTML("Reporting: How was the target population described? (Copy and paste text from the study)<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150), #LH: text changed
                   #r1_target_pop_c2
                   textAreaInput(paste0("sbreportc2",redcapdb$id[i]),HTML("<font color=\"#0066CC\">Reporting: How was the target population described in <i>cluster 2</i>? (Extract it from the study)</font><font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   HTML("<h4><b>Guidance for Question 1</h4></b>"),
                   img(src = "Q1.png", width = 500),br(),
                   selectInput(paste0("sbselect", redcapdb$id[i]), HTML("Question 1 - Was the sample invited to participate a close or true representation of the target population?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   #r1_rob_q1_c2
                   selectInput(paste0("sbselectc2",redcapdb$id[i]),HTML("<font color=\"#0066CC\">Question 1 - Was the sample invited in <i>cluster 2</i> to participate a close or true representation of the target population?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</font></span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3, "NA" = 4),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   # HTML("<h4><b>Guidance for Question 1</h4></b>"),
                   # img(src = "Q1.png", width = 500),br(),
                   textAreaInput(paste0("sbcomments",redcapdb$id[i]),HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   
                   #Response Rate
                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Response Rate</font><br><br></mark><font size=2><span style=\"font-weight:normal\">Response rate: The number of completed or returned survey instruments (questionnaires, interviews, etc.) divided by the total number of persons who would have been surveyed if all had participated. Usually expressed as a percentage (Porta M. A Dictionary of Epidemiology, 2014)</font></span></b><br><br>"), #LH: Can you make this small text a bit bigger?
                   selectInput(paste0("rrselect1", redcapdb$id[i]), HTML("Reporting: Was the response rate from the eligible population provided?<font color=\"#FF0000\"> *</font>"),
                               choices = c("Yes" = 1, "No" = 2),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 2, width = 380),
                   #r1_response_rate_c2
                   selectInput(paste0("rrselect1c2",redcapdb$id[i]),HTML("<font color=\"#0066CC\">Reporting: Was the response rate from the eligible population in <i> cluster 2</i> provided?</font><font color=\"#FF0000\"> *</font>"),
                               choices = c("Yes" = 1, "No" = 2, "NA" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   selectInput(paste0("rrselect2",redcapdb$id[i]), HTML("If the response rate is NOT provided, can you calculate the response rate?<font color=\"#FF0000\"> *</font>"), #LH: Text changed
                               choices = c("Yes" = 1, "No" = 2, "NA" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 2, width = 380),
                   #r1_response_calc_c2
                   selectInput(paste0("rrselect2c2",redcapdb$id[i]),HTML("<font color=\"#0066CC\">If the response rate for <i>cluster 2</i> is NOT provided, can you calculate the response rate?</font><font color=\"#FF0000\"> *</font>"), #LH: Text changed
                               choices = c("Yes" = 1, "No" = 2, "NA" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   img(src = "responserate.png"),
                   br(),
                   br(),
                   textInput(paste0("rrcomment1", redcapdb$id[i]), HTML("Number of individuals who participated in the study<font color=\"#FF0000\"> *</font>. If numbers are not reported, please write 'NA'"), "", width = 380),
                   textInput(paste0("rrcomment1c2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Number of individuals in <i>cluster 2</i> who participated in the study<font color=\"#FF0000\"> *</font>. If numbers are not reported, please write 'NA'</font>"), "", width = 380),
                   textInput(paste0("rrcomment2", redcapdb$id[i]), HTML("Number of eligible participants<font color=\"#FF0000\"> *</font>"), "", width = 380),
                   textInput(paste0("rrcomment2c2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Number of eligible participants for <i>cluster 2</i></font><font color=\"#FF0000\"> *</font>"), "", width = 380),
                   textInput(paste0("rrcomment3", redcapdb$id[i]), HTML("Response rate (%)<font color=\"#FF0000\"> *</font>"), "", width = 380),
                   textInput(paste0("rrcomment3c2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Response rate (%) for <i>cluster 2</i></font><font color=\"#FF0000\"> *</font>"), "", width = 380),
                   textAreaInput(paste0("rrcomment4", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   
                   #Selection bias - Response rate
                   HTML("<h4><b>Guidance for Question 2</h4></b>"),
                   img(src = "Q2.png", width = 600),
                   br(),br(),
                   selectInput(paste0("sbrrselect", redcapdb$id[i]), HTML("Question 2 - Are the characteristics of the non-respondents (if any) similar to those who participated in the study?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</span>"), #LH: Text changed
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   selectInput(paste0("sbrrselectc2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Question 2 - Are the characteristics of the non-respondents for <i>cluster 2</i> (if any) similar to those who participated in the study?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</font></span>"), #LH: Text changed
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3, "NA" = 4),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   #br(),
                   textAreaInput(paste0("sbrrcomment", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   
                   #Infromation Bias
                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Information Bias</font></mark></b><br><br>"),
                   textAreaInput(paste0("ibcomment", redcapdb$id[i]), HTML("Reporting: How were symptomatic status and asymptomatic status defined?<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150), #LH: Text changed
                   HTML("<h4><b>Guidance for Question 3</h4></b>"),
                   img(src = "Q3.png", width = 500),
                   br(),br(),
                   selectInput(paste0("ibselect", redcapdb$id[i]), HTML("Question 3 - Was the assessment of symptoms status adequate?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   selectInput(paste0("ibselectc2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Question 3 - Was the assessment of symptoms status adequate for <i>cluster 2</i>?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</font></span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3, "NA" = 4),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   textAreaInput(paste0("ibcomment2", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   HTML("<h4><b>Guidance for Question 4</h4></b>"),
                   img(src = "Q4.png", width = 500),
                   br(),
                   br(),
                   selectInput(paste0("ibselect2", redcapdb$id[i]), HTML("Question 4 - Based on the method symptoms were collected, is there a risk of recall bias?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   selectInput(paste0("ibselect2c2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Question 4 - Based on the method symptoms were collected, is there a risk of recall bias for <i>cluster 2</i>?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</font></span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3, "NA" = 4),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   textAreaInput(paste0("ibcomment3", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   
                   HTML("<b><font size=2.5>Reporting: For how long were the participants followed to assess symptom status?<font color=\"#FF0000\"> *</font></font></b>"), 
                   checkboxInput(paste0('ibcheck1', redcapdb$id[i]), '14 days after the last possible exposure date', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck2', redcapdb$id[i]), '7 days after diagnosis', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck3', redcapdb$id[i]), 'Until negative PCR result', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck4', redcapdb$id[i]), 'Both 14 days after the last possible exposure date AND 7 days after diagnosis', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck5', redcapdb$id[i]), 'Longer follow-up', value = FALSE, width = NULL),
                   
                   HTML("<b><font size=2.5><font color=\"#0066CC\">Reporting: For how long were the participants followed to assess symptom status in <i> cluster 2 </i>?</font><font color=\"#FF0000\"> *</font></font></b>"), 
                   checkboxInput(paste0('ibcheck1c2', redcapdb$id[i]), '14 days after the last possible exposure date', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck2c2', redcapdb$id[i]), '7 days after diagnosis', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck3c2', redcapdb$id[i]), 'Until negative PCR result', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck4c2', redcapdb$id[i]), 'Both 14 days after the last possible exposure date AND 7 days after diagnosis', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck5c2', redcapdb$id[i]), 'Longer follow-up', value = FALSE, width = NULL),
                   checkboxInput(paste0('ibcheck6c2', redcapdb$id[i]), 'NA', value = FALSE, width = NULL),
                   
                   textAreaInput(paste0("ibcomment4", redcapdb$id[i]), HTML("Reporting: Was the viral load reported? Please provide information about it CT (cycle threshold) values.<font color=\"#FF0000\"> *</font> (Please write NR if not reported)"), "", width = 380, height = 150),
                   HTML("<h4><b>Guidance for Question 5</h4></b>"),
                   img(src = "Q5.png", width = 500),
                   br(),
                   br(),
                   selectInput(paste0("ibselect3", redcapdb$id[i]), HTML("Question 5 - Is there a risk that asymptomatic status was misclassified?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   selectInput(paste0("ibselect3c2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Question 5 - Is there a risk that asymptomatic status was misclassified in <i>cluster 2</i>?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</font></span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3, "NA" = 4),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   textAreaInput(paste0("ibcomment5",redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   
                   #Selective or Incomplete Reporting of Outcome
                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Selective or Incomplete Reporting of Outcome</font></mark></b><br><br>"),
                   selectInput(paste0("siselect1",redcapdb$id[i]), HTML("Reporting: Do the authors present the symptom status of all participants at the end of follow-up?<font color=\"#FF0000\"> *</font>"), #LH: Text changed
                               choices = c("Yes" = 1, "No" = 2),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 2, width = 380),
                   selectInput(paste0("siselect1c2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Reporting: Do the authors present the symptom status of all participants in <i> cluster 2 </i>at the end of follow-up?</font><font color=\"#FF0000\"> *</font>"), #LH: Text changed
                               choices = c("Yes" = 1, "No" = 2, "NA" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   HTML("<h4><b>Guidance for Question 6</h4></b>"),
                   img(src = "Q6.png", width = 500),
                   br(),
                   br(),
                   selectInput(paste0("siselect2", redcapdb$id[i]), HTML("Question 6 - Is there a risk of incomplete or selective reporting of symptoms status among those who were positive for SARS-CoV-2?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 3, width = 380),
                   selectInput(paste0("siselect2c2", redcapdb$id[i]), HTML("<font color=\"#0066CC\">Question 6 - Is there a risk of incomplete or selective reporting of symptoms status among those who were positive for SARS-CoV-2 in <i>cluster 2</i>?<font color=\"#FF0000\"> *</font><br><span style=\"font-weight:normal\"> - Indicate whether the risk of bias is high, unclear, or low, according to the guidance above:</font></span>"),
                               choices = c("High" = 1, "Unclear" = 2, "Low" = 3, "NA" = 4),
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   #HTML("<h4><b>Guidance for Question 6</h4></b>"),
                   #img(src = "Q6.png", width = 500),
                   textAreaInput(paste0("sicomment", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 380, height = 150),
                   
                   #Final Comments
                   HTML("<b><mark style=\"background-color:#FFFFE0\"><font size=6>Final Comments</font></mark></b><br><br>"),
                   selectInput(paste0("fcselect",redcapdb$id[i]), HTML("Is there a risk of over or under-estimating the proportion of the truly asymptomatic population?<font color=\"#FF0000\"> *</font>"),
                               choices = c("- Underestimation of the proportion of asymptomatics" = 1, #LH: Text changed
                                           "- Overestimation of the proportion of asymptomatics" = 2, #LH: Text changed
                                           "- The proportion provided by the authors is accurate" = 3, #LH: Text changed
                                           "- Unclear" = 4), #LH: Text changed
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 4, width = 380),
                   selectInput(paste0("fcselectc2",redcapdb$id[i]), HTML("<font color=\"#0066CC\">Is there a risk of over or under-estimating the proportion of the truly asymptomatic population in <i> cluster 2</i></font>?<font color=\"#FF0000\"> *</font>"),
                               choices = c("- Underestimation of the proportion of asymptomatics" = 1, #LH: Text changed
                                           "- Overestimation of the proportion of asymptomatics" = 2, #LH: Text changed
                                           "- The proportion provided by the authors is accurate" = 3, #LH: Text changed
                                           "- Unclear" = 4, "- NA" = 5), #LH: Text changed
                               selected = FALSE,
                               multiple = FALSE,
                               selectize = FALSE, size = 5, width = 380),
                   textAreaInput(paste0("fccomment", redcapdb$id[i]), HTML("Comments<font color=\"#FF0000\"> *</font>"), "", width = 400, height = 150),
                   actionButton(paste0('c', redcapdb$id[i]), paste0("Submit:",redcapdb$id[i])),br(),br(),
                   HTML("<font size=4><strong>Please check all mandatory fields (<font color=\"#FF0000\">*</font>) are complete before submitting!</strong></font><br>"),br(),
                   HTML("<font size=5><strong>-------------------------------------------------------------------------------</strong></font><br>")

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
    FUN=function(k){
      observeEvent(input[[paste0("helpme", redcapdb$id[k])]], {
        helpme = input[[paste0("helpme", redcapdb$id[k])]]
        if (helpme=="TRUE") {
          updateTextAreaInput(session, paste0("robcluster", redcapdb$id[k]), value= "NA")
          updateTextAreaInput(session, paste0("sbreportc2", redcapdb$id[k]), value= "NA")
          updateSelectInput(session,paste0("sbselectc2",redcapdb$id[k]), selected = 4)
          updateSelectInput(session,paste0("rrselect1c2",redcapdb$id[k]), selected = 3)
          updateSelectInput(session,paste0("rrselect2c2",redcapdb$id[k]), selected = 3)
          updateTextInput(session, paste0("rrcomment1c2", redcapdb$id[k]), value= "NA")
          updateTextInput(session, paste0("rrcomment2c2",redcapdb$id[k]), value= "NA")
          updateTextInput(session, paste0("rrcomment3c2",redcapdb$id[k]), value= "NA")
          updateSelectInput(session,paste0("sbrrselectc2",redcapdb$id[k]), selected = 4)
          updateSelectInput(session,paste0("ibselectc2",redcapdb$id[k]), selected = 4)
          updateSelectInput(session,paste0("ibselect2c2",redcapdb$id[k]), selected = 4)
          updateCheckboxInput(session,paste0("ibcheck6c2",redcapdb$id[k]), value = TRUE)
          updateSelectInput(session,paste0("ibselect3c2",redcapdb$id[k]), selected = 4)
          updateSelectInput(session,paste0("siselect1c2",redcapdb$id[k]), selected = 3)
          updateSelectInput(session,paste0("siselect2c2",redcapdb$id[k]), selected = 4)
          updateSelectInput(session,paste0("fcselectc2",redcapdb$id[k]), selected = 5)
          
          
        }
        
        
      })
    })
  
  
  
  lapply(
    X = 1:nrow(redcapdb),
    FUN = function(j){
      observeEvent(input[[paste0("c", redcapdb$id[j])]], {
        cat("click\n")
        cluster2 = input[[paste0("helpme", redcapdb$id[j])]]
        #cluster2 = ifelse(cluster2=="TRUE", 1,0)
        robcluster = input[[paste0('robcluster', redcapdb$id[j])]]
        sbreport = input[[paste0('sbreport', redcapdb$id[j])]]
        sbreportc2 = input[[paste0('sbreportc2', redcapdb$id[j])]]
        sbselect = input[[paste0('sbselect', redcapdb$id[j])]]
        sbselectc2 = input[[paste0('sbselectc2', redcapdb$id[j])]]
        sbcomments = input[[paste0("sbcomments", redcapdb$id[j])]]
        rrselect1 = input[[paste0('rrselect1', redcapdb$id[j])]]
        rrselect1c2 = input[[paste0('rrselect1c2', redcapdb$id[j])]]
        rrselect2 = input[[paste0('rrselect2', redcapdb$id[j])]]
        rrselect2c2 = input[[paste0('rrselect2c2', redcapdb$id[j])]]
        rrcomment1 = input[[paste0('rrcomment1', redcapdb$id[j])]]
        rrcomment1c2 = input[[paste0('rrcomment1c2', redcapdb$id[j])]]
        rrcomment2 = input[[paste0('rrcomment2', redcapdb$id[j])]]
        rrcomment2c2 = input[[paste0('rrcomment2c2', redcapdb$id[j])]]
        rrcomment3 = input[[paste0('rrcomment3', redcapdb$id[j])]]
        rrcomment3c2 = input[[paste0('rrcomment3c2', redcapdb$id[j])]]
        rrcomment4 = input[[paste0('rrcomment4', redcapdb$id[j])]]
        sbrrselect = input[[paste0('sbrrselect', redcapdb$id[j])]]
        sbrrselectc2 = input[[paste0('sbrrselectc2', redcapdb$id[j])]]
        sbrrcomment = input[[paste0("sbrrcomment", redcapdb$id[j])]]
        ibcomment = input[[paste0('ibcomment', redcapdb$id[j])]]
        ibselect = input[[paste0('ibselect', redcapdb$id[j])]]
        ibselectc2 = input[[paste0('ibselectc2', redcapdb$id[j])]]
        ibcomment2 = input[[paste0("ibcomment2", redcapdb$id[j])]]
        ibselect2 = input[[paste0('ibselect2', redcapdb$id[j])]]
        ibselect2c2 = input[[paste0('ibselect2c2', redcapdb$id[j])]]
        ibcomment3 = input[[paste0("ibcomment3", redcapdb$id[j])]]
        ibcheck1 = input[[paste0('ibcheck1', redcapdb$id[j])]]
        ibcheck1 = ifelse(ibcheck1=="FALSE", 0,1)
        ibcheck2 = input[[paste0('ibcheck2', redcapdb$id[j])]]
        ibcheck2 = ifelse(ibcheck2=="FALSE", 0,1)
        ibcheck3 = input[[paste0('ibcheck3', redcapdb$id[j])]]
        ibcheck3 = ifelse(ibcheck3=="FALSE", 0,1)
        ibcheck4 = input[[paste0('ibcheck4', redcapdb$id[j])]]
        ibcheck4 = ifelse(ibcheck4=="FALSE", 0,1)
        ibcheck5 = input[[paste0('ibcheck5', redcapdb$id[j])]]
        ibcheck5 = ifelse(ibcheck5=="FALSE", 0,1)
        ibcheck1c2 = input[[paste0('ibcheck1c2', redcapdb$id[j])]]
        ibcheck1c2 = ifelse(ibcheck1c2=="FALSE", 0,1)
        ibcheck2c2 = input[[paste0('ibcheck2c2', redcapdb$id[j])]]
        ibcheck2c2 = ifelse(ibcheck2c2=="FALSE", 0,1)
        ibcheck3c2 = input[[paste0('ibcheck3c2', redcapdb$id[j])]]
        ibcheck3c2 = ifelse(ibcheck3c2=="FALSE", 0,1)
        ibcheck4c2 = input[[paste0('ibcheck4c2', redcapdb$id[j])]]
        ibcheck4c2 = ifelse(ibcheck4c2=="FALSE", 0,1)
        ibcheck5c2 = input[[paste0('ibcheck5c2', redcapdb$id[j])]]
        ibcheck5c2 = ifelse(ibcheck5c2=="FALSE", 0,1)
        ibcheck6c2 = input[[paste0('ibcheck6c2', redcapdb$id[j])]]
        ibcheck6c2 = ifelse(ibcheck6c2=="FALSE", 0,1)
        ibcomment4 = input[[paste0('ibcomment4', redcapdb$id[j])]]
        ibselect3 = input[[paste0('ibselect3', redcapdb$id[j])]]
        ibselect3c2 = input[[paste0('ibselect3c2', redcapdb$id[j])]]
        ibcomment5 = input[[paste0("ibcomment5", redcapdb$id[j])]]
        siselect1 = input[[paste0('siselect1', redcapdb$id[j])]]
        siselect1c2 = input[[paste0('siselect1c2', redcapdb$id[j])]]
        siselect2 = input[[paste0('siselect2', redcapdb$id[j])]]
        siselect2c2 = input[[paste0('siselect2c2', redcapdb$id[j])]]
        sicomment = input[[paste0("sicomment", redcapdb$id[j])]]
        fcselect = input[[paste0('fcselect', redcapdb$id[j])]]
        fcselectc2 = input[[paste0('fcselectc2', redcapdb$id[j])]]
        fccomment = input[[paste0("fccomment", redcapdb$id[j])]]
        id = redcapdb$id[j]
        user = input$in1
        req(sbreport, sbselect, sbcomments, rrselect1, rrselect2, rrcomment1, rrcomment2, rrcomment3, rrcomment4, sbrrselect, sbrrcomment, ibcomment, ibselect,ibcomment2,ibselect2,ibcomment3,ibcomment4,ibselect3,ibcomment5,siselect1,siselect2,sicomment,fcselect,fccomment)
        response = postnew(id, user, sbreport, sbselect, sbcomments, rrselect1, rrselect2, rrcomment1, rrcomment2, rrcomment3, rrcomment4, sbrrselect, sbrrcomment, ibcomment, ibselect,ibcomment2,ibselect2,ibcomment3,ibcheck1,ibcheck2,ibcheck3,ibcheck4,ibcheck5,ibcomment4,ibselect3,ibcomment5,siselect1,siselect2,sicomment,fcselect,fccomment,robcluster,sbreportc2,sbselectc2,rrselect1c2,rrselect2c2,rrcomment1c2,rrcomment2c2,rrcomment3c2,sbrrselectc2,ibselectc2,ibselect2c2,ibcheck1c2,ibcheck2c2,ibcheck3c2,ibcheck4c2,ibcheck5c2,ibcheck6c2,ibselect3c2,siselect1c2,siselect2c2,fcselectc2)
        
        rvals$last = paste0('Updated: ', id, " response: ", response)
        showModal(modalDialog(
          title = "",
          HTML(paste0("<font color=\"#FF0000\">Record #",id,' successfully submitted.</font>')),
          easyClose = TRUE,
          footer = NULL
        ))
        removeUI(paste0("#rcid_",redcapdb$id[j]))
        
      
        
        
        
        
      })
      
    }
  )
  
}