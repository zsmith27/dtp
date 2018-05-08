data.df <- reactive({
  if (input$data == 'Import CSV') {
    req(input$input.csv$datapath)
    read.csv(input$input.csv$datapath, stringsAsFactors = FALSE)
  } else {
    example.df
  }
})