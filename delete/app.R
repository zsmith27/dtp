library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    sidebarMenu(id = "sidebarmenu",
                menuItem("A", tabName = "a",  icon = icon("group", lib="font-awesome")),
                menuItem("B", tabName = "b", icon = icon("check-circle", lib = "font-awesome")),
                conditionalPanel("input.sidebarmenu === 'b'",
                                 sliderInput("b", "Under sidebarMenu", 1, 100, 50)
                )
    ),
    sliderInput("x", "Outside of menu", 1, 100, 50)
  ),
  dashboardBody()
)

server <- function(input, output) {}

shinyApp(ui, server)