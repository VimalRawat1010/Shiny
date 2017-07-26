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
library(ggplot2)
library(gplots)
#library(beeswarm)
# Define UI for application that draws a histogram
ui <- navbarPage(
                    HTML(paste0(textOutput("Vimal Rawat")),paste0("<b> <a href=", shQuote("https://scholar.google.ch/citations?user=NNotcvEAAAAJ&hl=en"), ">", "About Me", "</a> </b>")),
                    windowTitle = "BasicStat",
                    inverse = TRUE,
                    tabPanel("FAQ"),
                    titlePanel("Basic Stats Operations"),
                    sidebarPanel(h3("Input data here"),
                                textAreaInput(inputId="TxA1", "Set1: Paste unique items here",rows=3, width='90%', resize = "both", value=""),
                                fileInput(inputId = "f1",label = "or Upload file1",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                textAreaInput(inputId="TxA2", "Set2: Paste unique items here",rows=3, width='90%', resize = "both", value=""),
                                fileInput(inputId = "f2",label = "or Upload file2",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                textAreaInput(inputId="TxA3", "Set3: Paste unique items here",rows=3, width='90%', resize = "both", value=""),
                                fileInput(inputId = "f3",label = "or Upload file3",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                textAreaInput(inputId="TxA4", "Set4: Paste unique items here",rows=3, width='90%', resize = "both", value=""),
                                fileInput(inputId = "f4",label = "or Upload file4",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                textAreaInput(inputId="TxA5", "Set5: Paste unique items here",rows=3, width='90%', resize = "both", value=""),
                                fileInput(inputId = "f5",label = "or Upload file5",accept = c("text/csv","text/comma-separated-values,text/plain",".csv")),
                                downloadButton("downloadPlot", "Download Plot")
                                ),  
                    # Show a plot of the generated distribution
                    mainPanel(
                                h2(textOutput("Main Area")),
                               verbatimTextOutput("TxA1"),verbatimTextOutput("TxA2"),verbatimTextOutput("TxA3"),verbatimTextOutput("TxA4"),verbatimTextOutput("TxA5")
                                , tabsetPanel(tabPanel("Venneuler diag", 
                                                       sliderInput("alpha","Transparency", min=0, max=1, value=0.5),
                                                       sliderInput("ltype","Line type", min=1, max=5, value=1),
                                                       sliderInput("lwidth","Line width", min=0.01, max=30, value=1),
                                                       plotOutput("vennDiag"))
                                             ,tabPanel("Box plot", 
                                                       sliderInput("lwd1","Transparency", min=0, max=1, value=0.5),
                                                       plotOutput("boxPlot"))
                                             ,tabPanel("beeswarm plot", 
                                                       sliderInput("pch2","Style", min=1, max=20, value=16),
                                                       sliderInput("cex2","Size", min=0.1, max=5, value=2.5),
                                                        plotOutput("beeswarmPlot")
                                                      )
                                             ,tabPanel("stripchart", 
                                                       sliderInput("lwd3","Transparency", min=0, max=1, value=0.5),
                                                       plotOutput("stripchart")
                                             )
                                             )
        
                                )
                )


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {
   
   
   
     
    fileText1 <- eventReactive(input$f1, {
    filePath  <- input$f1$datapath
    fileText  <- paste(readLines(filePath), collapse = "\n")
    list1 <- fileText
    # update text area with file content
    updateTextAreaInput(session, "TxA1", value = fileText)
  })
  
      fileText2  <- eventReactive(input$f2, {
      filePath <- input$f2$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      list2 <- fileText
      updateTextAreaInput(session, "TxA2", value = fileText)
    })
      fileText3 <- eventReactive(input$f3, {
      filePath <- input$f3$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      list3 <- fileText
      updateTextAreaInput(session, "TxA3", value = fileText)
    })
      fileText4 <- eventReactive(input$f4, {
      filePath <- input$f4$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      list4 <- fileText
      # update text area with file content
      updateTextAreaInput(session, "TxA4", value = fileText)
    })
      fileText5 <- eventReactive(input$f5, {
      filePath <- input$f5$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      list5 <- fileText
      # update text area with file content
      updateTextAreaInput(session, "TxA5", value = fileText)
    })
    

    output$TxA1 <- renderPrint({ fileText1() })    
    output$TxA2 <- renderPrint({ fileText2() }) 
    output$TxA3 <- renderPrint({ fileText3() }) 
    output$TxA4 <- renderPrint({ fileText4() }) 
    output$TxA5 <- renderPrint({ fileText5() }) 
  


  #output$downloadPlot <- downloadHandler(filename =function(){ paste(input$filename, '.png', sep='')},content=function(filename) {ggsave(filename, plotInput2(),device = "png")})

  output$vennDiag <-renderPlot({
       print(plotInput2())
  })
  
  output$boxPlot <-renderPlot({
    print(boxPlotInput())
  })
  
  output$beeswarmPlot <-renderPlot({
    print(beeswarmInput())
  })
  
  
  beeswarmInput <- reactive({
    list1=strtoi(unlist(strsplit(input$TxA1,split = '\n' )))
    list2=strtoi(unlist(strsplit(input$TxA2,split = '\n' )))
    list3=strtoi(unlist(strsplit(input$TxA3,split = '\n' )))
    list4=strtoi(unlist(strsplit(input$TxA4,split = '\n' )))
    list5=strtoi(unlist(strsplit(input$TxA5,split = '\n' )))

    if((input$TxA2 != "") & (input$TxA3 == "")){
      beeswarm(list(list1,list2),col = 2:3, pch = input$pch2, cex=input$cex2)
    }
    if((input$TxA3 != "") & (input$TxA4 == "")){
      beeswarm(list(list1,list2,list3),col = 2:4, pch = input$pch2, cex=input$cex2)
    }
    if((input$TxA4 != "") & (input$TxA5 == "")){
      beeswarm(list(list1,list2,list3,list4),col = 2:5, pch = input$pch2, cex=input$cex2)
    }
    if(input$TxA5 != "" & input$TxA4 != "" & input$TxA3 != "" & input$TxA2 != "" & input$TxA1 != ""){
      beeswarm(list(list1,list2,list3,list4,list5),col = 2:6, pch = input$pch2, cex=input$cex2)
    }
  })
  
  
  
  boxPlotInput <- reactive({
    list1=strtoi(unlist(strsplit(input$TxA1,split = '\n' )))
    list2=strtoi(unlist(strsplit(input$TxA2,split = '\n' )))
    list3=strtoi(unlist(strsplit(input$TxA3,split = '\n' )))
    list4=strtoi(unlist(strsplit(input$TxA4,split = '\n' )))
    list5=strtoi(unlist(strsplit(input$TxA5,split = '\n' )))

    if((input$TxA2 != "") & (input$TxA3 == "")){
      boxplot(list1,list2)
    }
    if((input$TxA3 != "") & (input$TxA4 == "")){
      boxplot(list1,list2,list3)
    }
    if((input$TxA4 != "") & (input$TxA5 == "")){
      boxplot(list1,list2,list3,list4)
    }
    if(input$TxA5 != "" & input$TxA4 != "" & input$TxA3 != "" & input$TxA2 != "" & input$TxA1 != ""){
      boxplot(list1,list2,list3,list4,list5)
    }
  })
  
  
  plotInput2 <- reactive({
    list1=unlist(strsplit(input$TxA1,split = '\n' ))
    list2=unlist(strsplit(input$TxA2,split = '\n' ))
    list3=unlist(strsplit(input$TxA3,split = '\n' ))
    list4=unlist(strsplit(input$TxA4,split = '\n' ))
    list5=unlist(strsplit(input$TxA5,split = '\n' ))
     if((input$TxA2 != "") & (input$TxA3 == "")){
       grid.draw(venn.diagram(list(GrpA=list1,GrpB=list2),fill= 2:3,filename=NULL,alpha=input$alpha, lty=input$ltype, lwd=input$lwidth ))
     }
    if((input$TxA3 != "") & (input$TxA4 == "")){
      grid.draw(venn.diagram(list(GrpA=list1,GrpB=list2,GrpC=list3),fill= 2:4,filename=NULL,alpha=input$alpha, lty=input$ltype, lwd=input$lwidth))
    }
    if((input$TxA4 != "") & (input$TxA5 == "")){
      grid.draw(venn.diagram(list(GrpA=list1,GrpB=list2,GrpC=list3,GrpD=list4),fill= 2:5,filename=NULL,alpha=input$alpha, lty=input$ltype, lwd=input$lwidth))
    }
    if(input$TxA5 != "" & input$TxA4 != "" & input$TxA3 != "" & input$TxA2 != "" & input$TxA1 != ""){
      grid.draw(venn.diagram(list(GrpA=list1,GrpB=list2,GrpC=list3,GrpD=list4,GrpE=list5),fill= 2:6,filename=NULL,alpha=input$alpha, lty=input$ltype, lwd=input$lwidth))
    }
  })
  
  
  output$tb <-renderUI({

      tabsetPanel(tabPanel("File1", tableOutput("table1")),tabPanel("BoxPlot", plotOutput("boxPlot")),tabPanel("File3", tableOutput("table3")),tabPanel("File4", tableOutput("table4")),tabPanel("File5", tableOutput("table5")),tabPanel("VennDiag", tableOutput("vennDiag")))
    
    
  })
  
  })
  


# Run the application 
shinyApp(ui = ui, server = server)

