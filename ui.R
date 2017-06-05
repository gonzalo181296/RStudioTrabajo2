library(shiny)

Preguntas <- read.table(file='Questions.txt',sep=",")

fluidPage(
  headerPanel('Cual es tu posibilidad de tener un ataque cardiaco?'),
  sidebarLayout(
    sidebarPanel(
      # create some select inputs
      lapply(1:9, function(i) {
        selectInput(Preguntas$V1[i], Preguntas$V2[i],
                    choices <- c("Elija uno" = "",list(Preguntas$V3[i],Preguntas$V4[i],
                      Preguntas$V5[i],Preguntas$V6[i])))
      }),width = 20,
      submitButton("Calcular!")
    ),
    
    mainPanel(
      verbatimTextOutput('probabilidad')
      
      # UI output
      #lapply(1:9, function(i) {
      #  uiOutput(paste0('b', i))
      #})
    )
  )
)