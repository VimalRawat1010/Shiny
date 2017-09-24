library(shiny)
library(jsonlite)
library(mongolite)

loadData <- function(qry){
  mong <- mongo(collection = "test", db = "test", url = "mongodb://localhost",
                verbose = TRUE)
  
  df <- mong$find(qry)
  return(df)
}


shinyServer(function(input, output) {
  
  qryResults <- reactive({
    
    ## This bit responds to the user selection 
    ## which makes it 'reactive'
    region <- list(region = input$si_region)
    
    qry <- paste0('{ "region" : "',region , '"}')
    df <- loadData(qry)
    return(df)
  })
  
  output$qry_results <- renderDataTable({
    qryResults()
  })
  
  output$text1 <- renderText(nrow(qryResults()))
  
})



library(shiny)

shinyUI(navbarPage("mongo query",
                   sidebarLayout(
                     sidebarPanel(
                       selectInput(inputId = "si_region", label = "Select region", choices = c("r1", "r2"))
                     ),
                     mainPanel(
                       verbatimTextOutput(outputId = "text1"),
                       dataTableOutput(outputId = "qry_results")
                     )
                   )))