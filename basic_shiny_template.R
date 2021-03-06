########################################################
# The shortest viable shiny app
#######################################################
install.packages("rsconnect")
library(rsconnect)

library(shiny)




ui <- fluidPage(
  
  titlePanel("Ol� Shiny2!"), 
  
  sidebarLayout(
    
    sidebarPanel(
      sliderInput(inputId = "num",label = "Quantidade de N�meros a gerar", value = 20, min = 1,max = 100),
      numericInput(inputId = "mean", label = "M�dia", value = 0, min = 0, max=100)), # sidebarpanel
    
    mainPanel (plotOutput(outputId = "hist"),
               titlePanel("Sum�rio"), 
               verbatimTextOutput("sumario")), # mainpanel
    position = c("left")

  ) # sidebarlayout

 )# fluidpage

server <- function(input, output){
  # guarda a distribuic�o
  distribuicao <- reactive({rnorm(input$num, mean = input$mean)})
  
  output$hist <- renderPlot({hist(distribuicao())})
  output$sumario <- renderPrint({summary(distribuicao())})
                            
  
  
}


shinyApp(ui=ui, server=server)

