
source("config.R")
source("shared.R")


postdata<-function(id,agreed,design_group,commentText,user){ # this function handles the reason (decision) and posts this to the appropriate id, with the appropriate logic.
  if(agreed==1){
    data_redcap<-paste0('record_id,design_group,screening_api_complete\n',id,',',design_group,',2')
  }else{
    data_redcap<-paste0('record_id,design_group,veri_comment,screening_api_complete\n', id,',',design_group,',',paste0("\"",user,": ",commentText,"\""),',0')
  }
  postRedcap(data_redcap)
}


server <- function(input, output, session) {
  logname<-paste0(log_dir_screen,  "/", format(Sys.time(), "%Y%m%d_%H%M%S_"), ".csv")
  
  # put inside server so its unique per user
  result <- postForm(
    uri='https://redcap.ispm.unibe.ch/api/',
    token=API, # API of copy project, MC
    content='report',
    format='csv',
    report_id=reportid, #  3395 = report number verification
    rawOrLabel='label',
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
      }
    }else{
      output$login_response=renderText("Invalid user...")
      login=FALSE
    }
	  if(login){
    redcapdb<-subset(redcapdb, redcapdb$screen1_veri==user & redcapdb$screening_api_complete=="Unverified") 
    redcapdb<-head(redcapdb, row_limit)
    if(nrow(redcapdb)==0){
      insertUI(
        selector = "#add",
        where = "afterEnd",
        ui = fluidRow(id= "nodata",
                      column(7, HTML("No data to show: You have either completed all your tasks, or no tasks have been assigned<br><br><b>Please check back tomorrow!</b>"))))
      
    }else{
      redcapdb<- redcapdb[seq(dim(redcapdb)[1],1),] #also reverse because shiny builds page bottom up
      for(i in 1:nrow(redcapdb)) {
      
      abstract = redcapdb$abstract[i]
      insertUI(
      selector = "#add",
      where = "afterEnd",
      ui = fluidRow(id= paste0("rcid_",redcapdb$id[i]),
          column(7,
            h3(redcapdb$title[i]),
            # build links !!!redcap link is instabile and dependent on redcap updates!!!
                         tags$div(HTML(paste0("<a target=\"_blank\" href=\"http://dx.doi.org/",redcapdb$doi[i],"\" >DOI</a> |
                                              <a target=\"_blank\" href=\"",redcapdb$url[i],"\" >url</a> | <a target=\"_blank\" href=\"https://redcap.ispm.unibe.ch/redcap_v10.6.9/DataEntry/index.php?pid=139&page=screening_api&id=",redcapdb$id[i],"&event_id=408\" >REDCap</a>"))),

                          h5(HTML(abstract))
                        
          #
              ),
          column(5,tags$div(id= paste0("sel_", redcapdb$id[i])),
                 h4(HTML(paste0("included: <b>",redcapdb$review[i],"</b><br>reason: <b>", redcapdb$ex_reas[i] ,"</b>","<br>comment from screening: <b>", substring(redcapdb$comment[i],4)))),
                 textInput(paste0('in_comment', redcapdb$id[i]), 'Comment:'),
                 radioButtons(paste0('agree', redcapdb$id[i]), "Agreed",
                              list("Yes","No"), inline = TRUE, selected=character(0)),
                 selectInput(paste0('design_group', redcapdb$id[i]), paste0("Study design group: \n"),
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
        commentText = input[[paste0("in_comment", redcapdb$id[a])]]
        agreed = input[[paste0("agree", redcapdb$id[a])]]
        id = redcapdb$id[a]
	if(is.null(agreed)){
	  rvals$last=paste("Make a valid decision for record",id)
	  
	}else{
        agreed=ifelse(agreed=="Yes", 1,0)

        user = input$in1
        design_group=   input[[paste0('design_group', redcapdb$id[a])]]
        cat(agreed, " user=",user," id=",id,"\n")
        response = postdata(id,agreed,design_group,commentText,user)
        cat("response:", gsub("[\r\n]", "", response, "\n"), "\n")
        
        rvals$last = paste0('Updated: ', id, ' with value: agreed=', agreed, " comment=", commentText, " design group=",design_group," response: ", response)
        
        removeUI(paste0("#rcid_",redcapdb$id[a]))
        removeUI(paste0('#sel_', redcapdb$id[a]), immediate = T) 
}
      })
    }
  )
}
