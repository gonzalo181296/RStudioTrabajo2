library(shiny)

function(input, output) {
  output$a_out <- renderPrint({
    res <- lapply(1:9, function(i) input[[paste0('a', i)]])
    str(setNames(res, paste0('respuesta ', 1:9)))
  })
  
  ##lapply(1:10, function(i) {
  ##output[[paste0('b', i)]] <- renderUI({
  ##    strong(paste0('Hi, this is output B#', i))
  ##  })
  ##})
}