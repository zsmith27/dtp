#------------------------------------------------------------------------------
output$feature.select <- renderUI({
  req(data.df)
  selectInput("feature.col", "Feature Column:",
              c("", names(data.df())),
              selected = "basin")
})
#------------------------------------------------------------------------------
output$feature.values.select <- renderUI({
  req(data.df)
  req(input$feature.col)
  selectInput("feature.vec", "Features:",
              unique(subset(data.df(), select = input$feature.col)),
              multiple = TRUE)
})
#------------------------------------------------------------------------------
output$site.select <- renderUI({
  req(data.df)
  selectInput("site.col", "Site Column:",
              c("", names(data.df())),
              selected = "site")
})
#------------------------------------------------------------------------------
output$taxa.select <- renderUI({
  req(data.df)
  selectInput("taxa.col", "Taxa Column:",
              c("", names(data.df())),
              selected = "vernacular_name")
})
#------------------------------------------------------------------------------
output$count.select <- renderUI({
  req(data.df)
  selectInput("count.col", "Count Column:",
              c("", names(data.df())),
              selected = "count")
})
#------------------------------------------------------------------------------
slider.max <- reactive({
  req(summary.df())
  slider.max <- floor(max(abs(summary.df()$diff)))
})

output$slider.thresh <- renderUI({
  req(slider.max())
  sliderInput("diff.thresh", "Difference Threshold", 0, slider.max(), 1)
})
#------------------------------------------------------------------------------
