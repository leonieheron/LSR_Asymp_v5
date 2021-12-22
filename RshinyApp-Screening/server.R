
source("config.R")
source("shared.R")

decisions<-c("No: No reason needed" = 1,
             "No: Publication not on COVID-19" = 11,
             "No: No original data" = 12,
             "No: Data included in other publication" = 13,
             "No: Duplicate" = 14,
             "No: Insufficient extractable data" = 15,
             "No: Study design inappropriate" = 16,
             "No: Infections not diagnosed with a molecular test" = 17,
             "No: Aim of mathematical model not in review scope"=19,
             "No: Inadequate or undocumented follow-up time"=20,
             "No: Other" = 18)

postdata<-function(id, decision,helpme,user,design_groupscreen,comment){ # this function handles the reason (decision) and posts this to the appropriate id, with the appropriate logic.
  if(helpme==1){
    data_redcap<-paste0('record_id,help,screen1_assigned,comment,screening_api_complete\n',id,',',helpme,',',user,',\"',comment,'\",1')
  }else{
    if(decision==99){  # include decision:
      data_redcap<-paste0('record_id,review,help,screen1_assigned,design_groupscreen,comment,screening_api_complete\n', id,',1,',helpme,',',design_groupscreen,',',user,',\"',comment,'\",1')
    } else{ # exclude decisions:
      data_redcap<-paste0('record_id,review,ex_reas,help,screen1_assigned,design_groupscreen,comment,screening_api_complete\n', id,',2,',decision,',',helpme,',',design_groupscreen,',',user,',\"',comment,'\",1')
    }
    
  }
  postRedcap(data_redcap)
}


server <- function(input, output, session) {
  logname<-paste0(log_dir_screen,  "/", format(Sys.time(), "%Y%m%d_%H%M%S_"), ".csv")
  
  # retrieve screening data
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
  
  # write temp file with retrieved data, timestamped
  filename<-paste0(temp_dir_screen, "/", format(Sys.time(), "%Y%m%d_%H%M%S_"), "data_set.csv")
  
  
  con <- file(filename, open = "wt", encoding = "UTF-8") # encoding does not properly translate ('portuguese' characters not correct)
  writeLines(result, con = con)
  close(con)
  
  redcapdb<- read.csv(filename,header=TRUE, stringsAsFactors=FALSE)
  #redcapdb<-head(redcapdb, row_limit) #limit number of displayed rows
  
  redcapdb$id=redcapdb$record_id
  rvals = reactiveValues()
  
  observeEvent(rvals, {
    output$server_response = renderText(paste0("last response: ", rvals$last))  
  })
  
  observeEvent(input$add, {

    user=input$in1
    passw=input$passw
    
    if(user!=""){
    
      login=ifelse(passw==logins[logins$users==user,]$pw,TRUE,FALSE)
      login2=logins[logins$users==user,]$names
      output$login_response=renderText(paste0("User selected:", logins[logins$users==user,]$names, " login success:",login))
      if(login){
	      shinyjs::disable("in1") #disable login
        shinyjs::disable("add")
	#output$login_response=renderText(paste("number of lines:",10))
      }
    }else{
      output$login_response=renderText("Invalid user...")
      login=FALSE
    }
	  if(login){
	    cat(user, nrow(redcapdb))
    redcapdb<-subset(redcapdb, redcapdb$screen1_assigned==user & redcapdb$screening_api_complete==0 & redcapdb$screen1_veri=="") 
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
      for(i in 1:nrow(redcapdb)) {
      abstract = highlightWords(redcapdb$abstract[i])
      insertUI(
      selector = "#add",
      where = "afterEnd",
      ui = fluidRow(id= paste0("rcid_",redcapdb$id[i]),
          column(7,
            h3(redcapdb$title[i]),
                        tags$div(HTML(paste0("<a target=\"_blank\" href=\"http://dx.doi.org/",redcapdb$doi[i],"\" >DOI</a> | <a target=\"_blank\" href=\"",redcapdb$url[i],"\" >url</a> | 
                                             <a target=\"_blank\" href=\"https://redcap.ispm.unibe.ch/redcap_v10.6.9/DataEntry/index.php?pid=139&page=screening_api&id=",redcapdb$id[i],"&event_id=408\" >REDCap</a>"))),
                        h4(HTML(abstract))),
          column(5,tags$div(id= paste0("sel_", redcapdb$id[i]),
                            
                            selectInput(paste0('a', redcapdb$id[i]), paste0(redcapdb$id[i], " decision: \n"),
                                        choices =list("Included based on Title/Abstract" = 
                                                        c("No: No reason needed" = 1),
                                                      "Included based on Full text" = 
                                                        c("YES!" = 99, 
                                                          "No: Publication not on COVID-19" = 11,
                                                          "No: No original data" = 12,
                                                          "No: Data included in other publication" = 13,
                                                          "No: Duplicate" = 14,
                                                          "No: Insufficient extractable data" = 15,
                                                          "No: Study design inappropriate" = 16,
                                                          "No: Infections not diagnosed with a molecular test" = 17,
                                                          "No: Aim of mathematical model not in review scope"=19,
             			          "No: Inadequate or undocumented follow-up time"=20,
                                                          "No: Other" = 18)), size = 12, width = '500px', selectize = F)),
            checkboxInput(paste0('helpme', redcapdb$id[i]), 'IDONTKNOW!', value = FALSE, width = NULL),
            textInput(paste0('comment', redcapdb$id[i]), "comment:"),
            uiOutput(paste0('b', redcapdb$id[i])),
            selectInput(paste0('design_groupscreen', redcapdb$id[i]), paste0("Study design group: \n"),
                        choices = c("I don't know"=99,
                                    "EPI"=1,
                                    "BASIC"=2,
                                    "Non-original"=3), size = 4, width = '500px', selectize = F),
            actionButton(paste0('c', redcapdb$id[i]), paste0("Submit:",redcapdb$id[i])),HTML("<br><br>")
            )
          
      ))
      }
    }
    }
  })
  
  lapply(
    X = 1:nrow(redcapdb),
    FUN = function(a){
      observeEvent(input[[paste0("c", redcapdb$id[a])]], {
        cat("click\n")
        decision = input[[paste0("a", redcapdb$id[a])]]
        helpme = input[[paste0("helpme", redcapdb$id[a])]]        
        helpme = ifelse(helpme=="TRUE", 1,0)
        design_groupscreen = input[[paste0('design_groupscreen', redcapdb$id[a])]]
        id = redcapdb$id[a]
        user = input$in1
        comment = paste0(user,": ", input[[paste0("comment", redcapdb$id[a])]])
        cat(helpme, " user=",user,"\n")
        response = postdata(id, decision, helpme,design_groupscreen,user, comment)
        cat("response:", gsub("[\r\n]", "", response, "\n"), "\n")
        rvals$last = paste0('Updated: ', id, ' with value: decision=', " design group=",design_groupscreen," response: ", response)
        removeUI(paste0("#rcid_",redcapdb$id[a]))
        removeUI(paste0('#sel_', redcapdb$id[a]), immediate = T) 
      })
    }
  )
}
