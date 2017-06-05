library(shiny)

Preguntas <- read.table(file='Questions.txt',sep=",")

fluidPage(
  headerPanel('Cual es tu posibilidad de tener un ataque cardiaco?'),
  sidebarLayout(
    sidebarPanel(
      # create some select inputs
      lapply(1:9, function(i) {
        selectInput(paste0('a', i), Preguntas$V1[i],
                    choices <- c("Choose one" = "",list(Preguntas$V2[i],Preguntas$V3[i],Preguntas$V4[i],Preguntas$V5[i],Preguntas$V6[i],Preguntas$V7[i])))
      }),width = 20
    ),
    
    mainPanel(
      #verbatimTextOutput('a_out')
      
      # UI output
      #lapply(1:10, function(i) {
      #  uiOutput(paste0('b', i))
      #})
    )
  )
)