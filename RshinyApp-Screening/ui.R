
library(RCurl)
library(shiny)


source("config.R")
source("shared.R")

ui <- fluidPage(
  tags$head(tags$style(HTML(" body { margin:0.4%; "))),
  titlePanel(HTML("COVID screening: Asymptomatics"), windowTitle="COVID SCREENING APP"),
  fluidRow(shinyjs::useShinyjs(),
    textOutput("server_response", inline = T),
    textOutput("login_response", inline = T), br(),
    selectInput('in1', 'Select user:', c(Choose='', logins$users), selectize=FALSE),
    passwordInput('passw', 'Code', value = ""),
    actionButton("add", "Show data!"),
    br()
  )
)