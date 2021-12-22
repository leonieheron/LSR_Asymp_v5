
library(RCurl)
library(shiny)

source("config.R")
source("shared.R")

ui <- fluidPage(
  tags$head(tags$style(HTML(" body { margin:0.4%; "))),
  titlePanel(HTML("COVID ROB: Asymptomatics (Reviewer 2)"), windowTitle="COVID ROB APP"),
  fluidRow(shinyjs::useShinyjs(),
           #h4(paste("user selection:",user)),
           textOutput("server_response", inline = T),
           textOutput("login_response", inline = T), br(),
           selectInput('in1', 'Select user:', c(Choose='', logins$users), selectize=FALSE),
           passwordInput('passw', 'Code', value = ""),
           actionButton("add", "Show data!"),
           br()
  )
)