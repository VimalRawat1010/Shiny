#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(VennDiagram)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("Venn Diagrams"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
                    fileInput(inputId = "f1",label = "Upload file with unique items",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    fileInput(inputId = "f2",label = "Upload file with unique items",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    fileInput(inputId = "f3",label = "Upload file with unique items",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    fileInput(inputId = "f4",label = "Upload file with unique items",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    fileInput(inputId = "f5",label = "Upload file with unique items",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                    downloadButton("downloadPlot", "Download Plot")
      ),  
      # Show a plot of the generated distribution
      mainPanel(
        plotOutput("vennDiag"),uiOutput("tb") 
      )
   )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
   
  data1 <- reactive({
    file1 <-input$f1
    if(is.null(file1)){return()}
    read.table(file=file1$datapath,header = FALSE)
  })
  
  data2 <- reactive({
    file2 <-input$f2
    if(is.null(file2)){return()}
    read.table(file=file2$datapath,header = FALSE)
  })
  
  data3 <- reactive({
    file3 <-input$f3
    if(is.null(file3)){return()}
    read.table(file=file3$datapath,header = FALSE)
  })
  
  data4 <- reactive({
    file4 <-input$f4
    if(is.null(file4)){return()}
    read.table(file=file4$datapath,header = FALSE)
  })
  data5 <- reactive({
    file5 <-input$f5
    if(is.null(file5)){return()}
    read.table(file=file5$datapath,header = FALSE)
  })
  
  output$table1 <-renderTable({
    if(is.null(data1())){return()}
    data1()
  })
  output$table2 <-renderTable({
    if(is.null(data2())){return()}
    data2()
  })
  output$table3 <-renderTable({
    if(is.null(data3())){return()}
    data3()
  })
  output$table4 <-renderTable({
    if(is.null(data4())){return()}
    data4()
  })
  output$table5 <-renderTable({
    if(is.null(data5())){return()}
    data5()
  })
  
  
  
  output$downloadPlot <- downloadHandler(filename =function(){ paste(input$filename, '.svg', sep='')},
                                         content=function(filename) {ggsave(filename, plotInput(), device = svg)}
  )
  
  output$vennDiag <-renderPlot({
       print(plotInput())
  })
  
  
  plotInput <- reactive({
      l1 <- data1()
      l2 <- data2()
      l3 <- data3()
      l4 <- data4()
      l5 <- data5()
      a1 <- nrow(l1)
      a2 <- nrow(l2)
      a3 <- nrow(l3)
      a4 <- nrow(l4)
      a5 <- nrow(l5)
      a12 = length(intersect(l1$V1,l2$V1))
      a13 = length(intersect(l1$V1,l3$V1))
      a14 = length(intersect(l1$V1,l4$V1))
      a15 = length(intersect(l1$V1,l5$V1))
      a23 = length(intersect(l2$V1,l3$V1))
      a24 = length(intersect(l2$V1,l4$V1))
      a25 = length(intersect(l2$V1,l5$V1))
      a34 = length(intersect(l3$V1,l4$V1))
      a35 = length(intersect(l3$V1,l5$V1))
      a45 = length(intersect(l4$V1,l5$V1))
      a123 = length(intersect(intersect(l1$V1,l2$V1), l3$V1))
      a124 = length(intersect(intersect(l1$V1,l2$V1), l4$V1))
      a125 = length(intersect(intersect(l1$V1,l2$V1), l5$V1))
      a134 = length(intersect(intersect(l1$V1,l3$V1), l4$V1))
      a135 = length(intersect(intersect(l1$V1,l3$V1), l5$V1))
      a145 = length(intersect(intersect(l1$V1,l4$V1), l5$V1))
      a234 = length(intersect(intersect(l2$V1,l3$V1), l5$V1))
      a235 = length(intersect(intersect(l2$V1,l3$V1), l5$V1))
      a245 = length(intersect(intersect(l2$V1,l4$V1), l5$V1))
      a345 = length(intersect(intersect(l3$V1,l4$V1), l5$V1))
      a1234 = length(intersect(intersect(intersect(l1$V1,l2$V1),l3$V1),l4$V1))
      a1235 = length(intersect(intersect(intersect(l1$V1,l2$V1),l3$V1),l5$V1))
      a1245 = length(intersect(intersect(intersect(l1$V1,l2$V1),l4$V1),l5$V1))
      a1345 = length(intersect(intersect(intersect(l1$V1,l3$V1),l4$V1),l5$V1))
      a2345 = length(intersect(intersect(intersect(l2$V1,l3$V1),l4$V1),l5$V1))
      a12345 = length(intersect(intersect(intersect(intersect(l1$V1,l2$V1),l3$V1),l4$V1),l5$V1))
    
      if(is.null(data3())){
        
        venn.plot <- draw.pairwise.venn(
          area1 = a1,
          area2 = a2,
          cross.area = a12,
          category = c("First","Second"),
          fill = c("dodgerblue", "goldenrod1")
        )
        
      }
      
      
      if((is.null(data5())  && (!is.null(data3())))){
        
        venn.plot <- draw.triple.venn(
          area1 = a1,
          area2 = a2,
          area3 = a3,
          n12 = a12,
          n13 = a13,
          n23 = a23,
          n123 = a123,
          category = c("A","B","C"),
          fill = c("dodgerblue", "goldenrod1", "darkorange1")
        )
        
      }
      
      
      if(!is.null(data5())){
        
      venn.plot <- draw.quintuple.venn(
      area1 = a1,
      area2 = a2,
      area3 = a3,
      area4 = a4,
      area5 = a5,
      n12 = a12,
      n13 = a13,
      n14 = a14,
      n15 = a15,
      n23 = a23,
      n24 = a24,
      n25 = a25,
      n34 = a34,
      n35 = a35,
      n45 = a45,
      n123 = a123,
      n124 = a124,
      n125 = a125,
      n134 = a134,
      n135 = a135,
      n145 = a145,
      n234 = a234,
      n235 = a235,
      n245 = a245,
      n345 = a345,
      n1234 = a1234,
      n1235 = a1235,
      n1245 = a1245,
      n1345 = a1345,
      n2345 = a2345,
      n12345 = a12345,
      category = c("A", "B", "C", "D", "E"),
      fill = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
      cat.col = c("dodgerblue", "goldenrod1", "darkorange1", "seagreen3", "orchid3"),
      cat.cex = 2,
      margin = 0.05,
      cex = c(1.5, 1.5, 1.5, 1.5, 1.5, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 1, 0.8, 
              1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 0.55, 1, 1, 1, 1, 1, 1.5),
      ind = TRUE
    )
  } 
  })
  
  output$tb <-renderUI({

      tabsetPanel(tabPanel("File1", tableOutput("table1")),tabPanel("File2", tableOutput("table2")),tabPanel("File3", tableOutput("table3")),tabPanel("File4", tableOutput("table4")),tabPanel("File5", tableOutput("table5")),tabPanel("VennDiag", tableOutput("vennDiag")))
    
    
  })
  
  })
  


# Run the application 
shinyApp(ui = ui, server = server)

