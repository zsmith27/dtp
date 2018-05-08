output$plot1 <- renderPlot({
  #invalidateLater(5000, session)
  if (input$data == 'Import CSV') {
    validate(
      need(input$input.csv$datapath, "Select a CSV File"))
  }
  validate(
    need(input$feature.col != "", 'Select the Feature Column'),
    need(length(input$feature.vec) >= 2, 'Select two features'),
    need(input$site.col != "", 'Select the Site Column'),
    need(input$taxa.col != "", 'Select the Taxa Column'),
    need(input$count.col != "", 'Select the Count Column')
  )
  req(filter.df())
  
  dichotomous_taxa_plot(long.df = filter.df(),
                        dichotomous.col = input$feature.col,
                        dichotomous.vec = input$feature.vec,
                        taxa.col = input$taxa.col,
                        value.col = "median",
                        coord.flip = if_else(input$orientation == "Vertical", TRUE, FALSE)
  )
})