library(shiny)
library(shinydashboard)
# library(mmir)
library(tidyverse)
library(rlang)

ui <- dashboardPage(
  dashboardHeader(title = "Beta"),
  dashboardSidebar(
      selectInput("data",
                  "Data",
                  c("Import CSV", "Example Data"),
                  selected = "Import CSV"),
      conditionalPanel("input.data == 'Import CSV'",
                       fileInput('input.csv', 'Import CSV File',
                                 accept=c('text/csv', 
                                          'text/comma-separated-values,text/plain', 
                                          '.csv'))),
    uiOutput("feature.select"),
    uiOutput("feature.values.select"),
    uiOutput("site.select"),
    uiOutput("taxa.select"),
    uiOutput("count.select"),
    #sliderInput("diff.thresh", "Difference Threshold", 0, 10, 1),
    uiOutput("slider.thresh"),
    radioButtons("orientation",
                 "Orientation",
                 c("Horizontal", "Vertical"))
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .shiny-output-error-validation {
                      color: gray; text-align: center; font-size:30px; padding: 70px;
                      }
                      "))),
    #fillPage(
      #DT::dataTableOutput('table1')
      tags$style(type = "text/css", "#plot1 {height: calc(100vh - 80px) !important;}"),
      plotOutput("plot1")
    #)
  )
)