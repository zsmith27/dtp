summary.df <- reactive({
  if (input$data == 'Import CSV') {
    req(input$input.csv$datapath)
  }
  req(data.df())
  req(input$feature.col != "")
  req(length(input$feature.vec) >= 2)
  req(input$site.col != "")
  req(input$taxa.col != "")
  req(input$count.col != "")
  
  dichotomous_summary(long.df = data.df(),
                      dichotomous.col = input$feature.col,
                      dichotomous.vec = input$feature.vec,
                      site.col = input$site.col,
                      taxa.col = input$taxa.col,
                      count.col = input$count.col)
  
})


filter.df <- reactive({
  req(summary.df())
  req(input$diff.thresh)
  
  summary.df() %>% 
    filter(abs(diff) >= input$diff.thresh)
})