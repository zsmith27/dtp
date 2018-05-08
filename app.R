#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# library(mmir)
library(tidyverse)
library(rlang)

# Define UI for application that draws a histogram
ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),
  dashboardSidebar(
    selectInput("feature", "Features:",
                unique(input.df$basin))
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1", height = 250)),
      
      box(
        title = "Controls",
        sliderInput("slider", "Number of observations:", 1, 100, 50)
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot1 <- renderPlot({
    dichotomous_summary(input.df, "basin", feature.vec,
                        feature.col, taxa.col, count.col)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

