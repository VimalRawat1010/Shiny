#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#.libPaths("/home/vimal/R_Library/")

library(shiny)
library(ggfortify)
library(gplots)



  ui <- fluidPage(
    sidebarLayout(
      sidebarPanel(
        fileInput("file", "Upload the file with ATG ids and Gene as header",
                  accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv")
        ),
        helpText("Deafault file size 5MB"),
        tags$hr(),
        checkboxInput("header", "Header", TRUE),
        radioButtons(inputId = "sep",label =  "Seprator", choices = c(Comma=',', "NewLine" = '\n', Tab ='\t'), selected ='\n'),
        sliderInput(inputId = "num", label = "K-mean clustering : K=", value = 5, min=2, max=10)
      ),
      mainPanel(tableOutput("tb"), plotOutput("distPlot"))
    )
)
  
# Define server logic required to draw a histogram
server <- function(input, output) {
   

  
  
       #### Reactive function to return content of uploaded file
        data <-reactive({
        file1 <- input$file
        if (is.null(file1)) {return()}
        read.table(file= file1$datapath, sep=input$sep, header = input$header)
        })
        
       #### Function to get path of file 
       output$filedf <- renderTable({
        if(is.null(data())) {return ()} 
        input$file
      }) 
       
      
       output$heatmap <- renderPlot({
         if(is.null(data())) {return ()} 
         
         x <- read.table("../data/TissueSpecific_FPKM.txt", header = T) 
         geneid <- data()
         WorkingSet <-subset(x, x$Gene %in% geneid$Gene)
         row.names(WorkingSet) <- WorkingSet[,1]
         WorkingSet$Gene <- NULL
         heatmap.2(data.matrix(WorkingSet), col=redgreen(100), scale="row", trace="none")
         }, width = 880, height = 880, units = "px", pointsize = 12)
      
      
      output$table <- renderTable({
      if(is.null(data())) {return ()} 
      data()
      })
      
      output$pca <- renderPlot({
        if(is.null(data())) {return ()} 
        
        x <- read.table("../data/TissueSpecific_FPKM.txt", header = T) 
        geneid <- data()
        WorkingSet <-subset(x, x$Gene %in% geneid$Gene)
        row.names(WorkingSet) <- WorkingSet[,1]
        WorkingSet$Gene <- NULL
        autoplot(prcomp(WorkingSet),label = TRUE,loadings = TRUE, loadings.colour = 'blue',
                   loadings.label = TRUE, loadings.label.size = 3)
        }, width = 880, height = 880, units = "px", pointsize = 12
        )
      
      
      output$clust <- renderPlot({
        if(is.null(data())) {return ()} 
        x <- read.table("../data/TissueSpecific_FPKM.txt", header = T) 
        geneid <- data()
        WorkingSet <-subset(x, x$Gene %in% geneid$Gene)
        row.names(WorkingSet) <- WorkingSet[,1]
        WorkingSet$Gene <- NULL
        autoplot(kmeans(WorkingSet, input$num),data=WorkingSet,label = TRUE)
      }, width = 880, height = 880, units = "px", pointsize = 12
      )
      
      

      
      output$tb <-renderUI({
        if(is.null(data()))
        {}
        else
          tabsetPanel(tabPanel("About file", tableOutput("filedf")),tabPanel("Data",tableOutput("table")), tabPanel("Heatmap",plotOutput("heatmap")), tabPanel("PCA",plotOutput("pca")), tabPanel("K-mean Clustering",plotOutput("clust")))
        })

}

# Run the application 
shinyApp(ui = ui, server = server)
