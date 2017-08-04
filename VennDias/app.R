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
library(beeswarm)
# Define UI for application that draws a histogram
ui <- navbarPage(
                    HTML(paste0(textOutput("Vimal Rawat")),paste0("<b> <h3> <a href=", shQuote("https://scholar.google.ch/citations?user=NNotcvEAAAAJ&hl=en"), ">", "About Me", "</a> </h3></b>")),
                    windowTitle = "BasicStat",
                    inverse = TRUE,
                    tabPanel(
                      HTML(paste0("<b> <h3> <a href=", shQuote("https://github.com/VimalRawat1010"), ">", "&nbsp;Github", "</a> </h3></b>"))
                    ),
                    tabPanel(
                      HTML(paste0("<b> <h3> <a href=", shQuote("https://github.com/VimalRawat1010"), ">", "&nbsp;Tab2", "</a> </h3></b>"))
                    ),
                    tabPanel(
                      HTML(paste0("<b> <h3> <a href=", shQuote("https://github.com/VimalRawat1010"), ">", "&nbsp;Tab3", "</a> </h3></b>"))
                    ),
                    tabPanel(
                      HTML(paste0("<h1><b>&nbsp;&nbsp;Shiny Web-tool for basic statistical plotting of data</b><h1>"))
                    ),

                    titlePanel("Basic Stats Plotting"),
                    sidebarPanel(h3("Define plot setting here"),
                                 sliderInput("alpha","Transparency", min=0, max=1, value=0.5),
                                 sliderInput("lty","Line type", min=1, max=5, value=1),
                                 sliderInput("lwd","Line width", min=0.01, max=30, value=5),
                                 sliderInput("cex","Label size", min=0.1, max=20, value=2),
                                 width = 2
                                ),
                    # Show a plot of the generated distribution
                    mainPanel(
                               h3(textOutput("Main Area")),
                               verbatimTextOutput("TxA1"),verbatimTextOutput("TxA2"),verbatimTextOutput("TxA3"),verbatimTextOutput("TxA4"),verbatimTextOutput("TxA5"),
                               verbatimTextOutput("BoxTxA1"),verbatimTextOutput("BoxTxA2"),verbatimTextOutput("BoxTxA3"),verbatimTextOutput("BoxTxA4"),verbatimTextOutput("BoxTxA5"),
                               verbatimTextOutput("HeatTxA1"),verbatimTextOutput("HeatTxA2"),verbatimTextOutput("HeatTxA3"),verbatimTextOutput("HeatTxA4"),verbatimTextOutput("HeatTxA5"),
                               verbatimTextOutput("BSTxA1"),verbatimTextOutput("BSTxA2"),verbatimTextOutput("BSTxA3"),verbatimTextOutput("BSTxA4"),verbatimTextOutput("BSTxA5")

                                , h3("This app/webserver provide some basic statistical plotting feature using user provided data.\n"),

                                   tabsetPanel( type="tabs",

                                     tabPanel(h4("About web-tool"),
                                              HTML("<br></br>"),
                                              HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                              HTML("This section describe how to use this web tool to plot your data.")
                                              ),

                                     tabPanel(h4("FAQs"), HTML("<br></br>"), h5("This webtool can be used to to some basic operation on a dataset such as : Venn digram, Boxplots, Beeswarm plots, and Heatmap.\n "),
                                              HTML("<br></br>"),
                                              HTML("<h5> <b>What is a venn diagram?</h5></b>"),
                                              HTML(paste0("<b> <h5> <a href=", shQuote("https://en.wikipedia.org/wiki/Venn_diagram"), ">", "&bull;&nbsp;Venn diagram:", "</a> </h5></b>")),
                                              HTML("<h5>A Venn diagram show all possible logical relationships between a collection of sets</h5>"),

                                              HTML("<br></br>"),
                                              HTML("<h5><b>What is a Euler diagram and how it is different from Venn diagram?</h5></b>"),
                                              HTML(paste0("<b> <h5> <a href=", shQuote("https://en.wikipedia.org/wiki/Euler_diagram"), ">", "&bull;&nbsp;Euler diagram:", "</a> </h5></b>")),
                                              HTML("<h5>While a Venn diagram show all possible logical relationships between a collection of sets, Euler diagram only shows relationships that exist in real world.</h5>"),

                                              HTML("<br></br>"),
                                              HTML("<h5><b>What is a boxplot?</h5></b>"),
                                              HTML(paste0("<b> <h5> <a href=", shQuote("https://en.wikipedia.org/wiki/Box_plot"), ">", "&bull;&nbsp;Boxplot Diagram", "</a> </h5></b>")),
                                              HTML("<h5>Box plots are graphical tools to visualize key statistical measures, such as median, mean and quartile.</h5>"),


                                              HTML("<br></br>"),
                                              HTML("<h5><b>What is a Beeswarm Plot?</h5></b>"),
                                              HTML(paste0("<b> <h5> <a href=", shQuote("https://infobib.de/2016/01/07/was-ist-ein-beeswarm-plot/"), ">", "&bull;&nbsp;Beeswarm Plot", "</a> </h5></b>")),
                                              HTML("<h5>The bee swarm plot is a one-dimensional scatter plot like \"stripchart\", but with closely-packed, non-overlapping points..</h5>"),

                                              HTML("<br></br>"),
                                              HTML("<h5><b>What is a heatmap?</h5></b>"),
                                              HTML(paste0("<b> <h5> <a href=", shQuote("https://en.wikipedia.org/wiki/Heat_map"), ">", "&bull;&nbsp;Heatmap", "</a> </h5></b>")),
                                              HTML("<h5>A heat map is a two-dimensional representation of data in which values are represented by colors.</h5>")
                                                ),

                                    tabPanel(h4("Venneuler diag"),
                                    fluidRow(
                                              HTML("<br></br>"),
                                              HTML("<h4><b>In this section, you can plot your data in form of a venn/euler diagram</b>.<br></br>This tool makes a euler diagram for 3 or less datasets and a venn diagram for more than 3 datasets.</h4>"),
                                              HTML("<h4><b>Step1:</b> Enter your data in each text area by typing it or upload text file </h4>"),
                                              HTML("<h5>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;data may contain Numbers or Text string but should be separated by a newline </h5>"),
                                              HTML("<h4><b>Step2:</b> After submitting data to atleast 2 textareas or uploading 2 files a venn diagram should be visible.</h4>"),
                                              HTML("<h4><b>Step3:</b> Click on download.</h4>"),
                                              HTML("<br></br>"),
                                              column(2,
                                                       textAreaInput(inputId="TxA1", "Set A: ",rows=3, width='90%', resize = "both", value=""),
                                                       fileInput(inputId = "f1",label = "or Upload file1",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                       ),
                                              column(2,
                                                      textAreaInput(inputId="TxA2", "Set B: ",rows=3, width='90%', resize = "both", value=""),
                                                       fileInput(inputId = "f2",label = "or Upload file2",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                     ),
                                              column(2,
                                                       textAreaInput(inputId="TxA3", "Set C:",rows=3, width='90%', resize = "both", value=""),
                                                       fileInput(inputId = "f3",label = "or Upload file3",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                     ),
                                              column(2,
                                                       textAreaInput(inputId="TxA4", "Set D: ",rows=3, width='90%', resize = "both", value=""),
                                                       fileInput(inputId = "f4",label = "or Upload file4",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                    ),
                                              column(2,
                                                       textAreaInput(inputId="TxA5", "Set E: ",rows=3, width='90%', resize = "both", value=""),
                                                       fileInput(inputId = "f5",label = "or Upload file5",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                     ),
                                              downloadButton("downloadVennplot", "Download Plot"),

                                              plotOutput("vennDiag")
                                         )),

                                              tabPanel(h4("Box plot"),
                                                      fluidRow(
                                                        HTML("<br></br>"),
                                                        HTML("<h4><b>In this section, you can plot your data in form of a Boxplot</b>.<br></br>This tool makes a euler diagram for 3 or less datasets and a venn diagram for more than 3 datasets.</h4>"),
                                                        HTML("<h4><b>Step1:</b> Enter your data in each text area by typing it or upload text file </h4>"),
                                                        HTML("<h5>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><b>data must contain only Numbers and should be separated by a newline </u></b></h5>"),
                                                        HTML("<h4><b>Step2:</b> After submitting data to atleast 2 textareas or uploading 2 files a venn diagram should be visible.</h4>"),
                                                        HTML("<h4><b>Step3:</b> Click on download.</h4>"),
                                                        HTML("<br></br>"),
                                                          column(2,
                                                               textAreaInput(inputId="BoxTxA1", "Set A",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "Boxf1",label = "or Upload file1",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BoxTxA2", "Set B",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "Boxf2",label = "or Upload file2",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BoxTxA3", "Set C",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "Boxf3",label = "or Upload file3",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BoxTxA4", "Set D",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "Boxf4",label = "or Upload file4",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BoxTxA5", "Set E",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "Boxf5",label = "or Upload file5",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        downloadButton("downloadBoxplot", "Download Plot"),


                                              plotOutput("boxPlot")
                                          )),

                                             tabPanel(h4("beeswarm plot"),
                                                      fluidRow(
                                                        HTML("<br></br>"),
                                                        HTML("<h4><b>In this section, you can plot your data in form of a heatmap diagram</b>.<br></br>This tool makes a euler diagram for 3 or less datasets and a venn diagram for more than 3 datasets.</h4>"),
                                                        HTML("<h4><b>Step1:</b> Enter your data in each text area by typing it or upload text file </h4>"),
                                                        HTML("<h5>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><b>data must contain only Numbers and should be separated by a newline </u></b></h5>"),
                                                        HTML("<h4><b>Step2:</b> After submitting data to atleast 2 textareas or uploading 2 files a venn diagram should be visible.</h4>"),
                                                        HTML("<h4><b>Step3:</b> Click on download.</h4>"),
                                                        HTML("<br></br>"),


                                                        column(2,
                                                               textAreaInput(inputId="BSTxA1", "Set A",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "BSf1",label = "or Upload file1",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BSTxA2", "Set B",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "BSf2",label = "or Upload file2",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BSTxA3", "Set C",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "BSf3",label = "or Upload file3",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BSTxA4", "Set D",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "BSf4",label = "or Upload file4",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        column(2,
                                                               textAreaInput(inputId="BSTxA5", "Set E",rows=3, width='90%', resize = "both", value=""),
                                                               fileInput(inputId = "BSf5",label = "or Upload file5",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                        ),
                                                        downloadButton("downloadBS", "Download Plot"),
                                                        plotOutput("beeswarmPlot")
                                                      )),
                                              tabPanel(h4("Heatmap"),
                                                        fluidRow(
                                                          HTML("<br></br>"),
                                                          HTML("<h4><b>In this section, you can plot your data in form of a heatmap diagram</b>.<br></br>This tool makes a euler diagram for 3 or less datasets and a venn diagram for more than 3 datasets.</h4>"),
                                                          HTML("<h4><b>Step1:</b> Enter your data in each text area by typing it or upload text file </h4>"),
                                                          HTML("<h5>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;<u><b>data must contain only Numbers and should be separated by a newline </u></b></h5>"),
                                                          HTML("<h4><b>Step2:</b> After submitting data to atleast 2 textareas or uploading 2 files a venn diagram should be visible.</h4>"),
                                                          HTML("<h4><b>Step3:</b> Click on download.</h4>"),
                                                          HTML("<br></br>"),
                                                          radioButtons("Bx_r1", "Colors scale by:",
                                                                       c("Row" = "row",
                                                                         "Column" = "col",
                                                                         "None" = "none"
                                                                       ), inline= TRUE),
                                                          checkboxInput("Bx_cb_row", "Show dendogram for row", value = TRUE, width = NULL),
                                                          checkboxInput("Bx_cb_col", "Show dendogram for column", value = TRUE, width = NULL),

                                                          column(2,
                                                                 textAreaInput(inputId="HeatTxA1", "Set A",rows=3, width='90%', resize = "both", value=""),
                                                                 fileInput(inputId = "Heatf1",label = "or Upload file1",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                          ),
                                                          column(2,
                                                                 textAreaInput(inputId="HeatTxA2", "Set B",rows=3, width='90%', resize = "both", value=""),
                                                                 fileInput(inputId = "Heatf2",label = "or Upload file2",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                          ),
                                                          column(2,
                                                                 textAreaInput(inputId="HeatTxA3", "Set C",rows=3, width='90%', resize = "both", value=""),
                                                                 fileInput(inputId = "Heatf3",label = "or Upload file3",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                          ),
                                                          column(2,
                                                                 textAreaInput(inputId="HeatTxA4", "Set D",rows=3, width='90%', resize = "both", value=""),
                                                                 fileInput(inputId = "Heatf4",label = "or Upload file4",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                          ),
                                                          column(2,
                                                                 textAreaInput(inputId="HeatTxA5", "Set E",rows=3, width='90%', resize = "both", value=""),
                                                                 fileInput(inputId = "Heatf5",label = "or Upload file5",accept = c("text/csv","text/comma-separated-values,text/plain",".csv"))
                                                          ),
                                                          downloadButton("downloadHeatmap", "Download Plot"),
                                                         plotOutput("Heatmap")
                                                         ))
                                             ), width = 9

                                )
                )


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output,session) {



    fileText1 <- eventReactive(input$f1, {
    filePath  <- input$f1$datapath
    fileText  <- paste(readLines(filePath), collapse = "\n")
    # update text area with file content
    updateTextAreaInput(session, "TxA1", value = fileText)
  })
      fileText2  <- eventReactive(input$f2, {
      filePath <- input$f2$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "TxA2", value = fileText)
    })
      fileText3 <- eventReactive(input$f3, {
      filePath <- input$f3$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "TxA3", value = fileText)
    })
      fileText4 <- eventReactive(input$f4, {
      filePath <- input$f4$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "TxA4", value = fileText)
    })
      fileText5 <- eventReactive(input$f5, {
      filePath <- input$f5$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "TxA5", value = fileText)
    })


    output$TxA1 <- renderPrint({ fileText1() })
    output$TxA2 <- renderPrint({ fileText2() })
    output$TxA3 <- renderPrint({ fileText3() })
    output$TxA4 <- renderPrint({ fileText4() })
    output$TxA5 <- renderPrint({ fileText5() })



    fileBoxText1 <- eventReactive(input$Boxf1, {
      filePath  <- input$Boxf1$datapath
      fileText  <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BoxTxA1", value = fileText)
    })
    fileBoxText2  <- eventReactive(input$Boxf2, {
      filePath <- input$Boxf2$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BoxTxA2", value = fileText)
    })
    fileBoxText3 <- eventReactive(input$Boxf3, {
      filePath <- input$Boxf3$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BoxTxA3", value = fileText)
    })
    fileBoxText4 <- eventReactive(input$Boxf4, {
      filePath <- input$Boxf4$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BoxTxA4", value = fileText)
    })
    fileBoxText5 <- eventReactive(input$Boxf5, {
      filePath <- input$Boxf5$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BoxTxA5", value = fileText)
    })


    output$BoxTxA1 <- renderPrint({ fileBoxText1() })
    output$BoxTxA2 <- renderPrint({ fileBoxText2() })
    output$BoxTxA3 <- renderPrint({ fileBoxText3() })
    output$BoxTxA4 <- renderPrint({ fileBoxText4() })
    output$BoxTxA5 <- renderPrint({ fileBoxText5() })




    fileHeatText1 <- eventReactive(input$Heatf1, {
      filePath  <- input$Heatf1$datapath
      fileText  <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "HeatTxA1", value = fileText)
    })
    fileHeatText2  <- eventReactive(input$Heatf2, {
      filePath <- input$Heatf2$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "HeatTxA2", value = fileText)
    })
    fileHeatText3 <- eventReactive(input$Heatf3, {
      filePath <- input$Heatf3$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "HeatTxA3", value = fileText)
    })
    fileHeatText4 <- eventReactive(input$Heatf4, {
      filePath <- input$Heatf4$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "HeatTxA4", value = fileText)
    })
    fileHeatText5 <- eventReactive(input$Heatf5, {
      filePath <- input$Heatf5$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "HeatTxA5", value = fileText)
    })


    output$HeatTxA1 <- renderPrint({ fileHeatText1() })
    output$HeatTxA2 <- renderPrint({ fileHeatText2() })
    output$HeatTxA3 <- renderPrint({ fileHeatText3() })
    output$HeatTxA4 <- renderPrint({ fileHeatText4() })
    output$HeatTxA5 <- renderPrint({ fileHeatText5() })


    fileBSText1 <- eventReactive(input$BSf1, {
      filePath  <- input$BSf1$datapath
      fileText  <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BSTxA1", value = fileText)
    })
    fileBSText2  <- eventReactive(input$BSf2, {
      filePath <- input$BSf2$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BSTxA2", value = fileText)
    })
    fileBSText3 <- eventReactive(input$BSf3, {
      filePath <- input$BSf3$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BSTxA3", value = fileText)
    })
    fileBSText4 <- eventReactive(input$BSf4, {
      filePath <- input$BSf4$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BSTxA4", value = fileText)
    })
    fileBSText5 <- eventReactive(input$BSf5, {
      filePath <- input$BSf5$datapath
      fileText <- paste(readLines(filePath), collapse = "\n")
      # update text area with file content
      updateTextAreaInput(session, "BSTxA5", value = fileText)
    })

    output$BSTxA1 <- renderPrint({ fileBSText1() })
    output$BSTxA2 <- renderPrint({ fileBSText2() })
    output$BSTxA3 <- renderPrint({ fileBSText3() })
    output$BSTxA4 <- renderPrint({ fileBSText4() })
    output$BSTxA5 <- renderPrint({ fileBSText5() })






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
  output$Heatmap <-renderPlot({
    print(HeatmapInput())
  })


  beeswarmInput <- reactive({
    list1=strtoi(unlist(strsplit(input$BSTxA1,split = '\n' )))
    list2=strtoi(unlist(strsplit(input$BSTxA2,split = '\n' )))
    list3=strtoi(unlist(strsplit(input$BSTxA3,split = '\n' )))
    list4=strtoi(unlist(strsplit(input$BSTxA4,split = '\n' )))
    list5=strtoi(unlist(strsplit(input$BSTxA5,split = '\n' )))

    if((input$BSTxA2 != "") & (input$BSTxA3 == "")){
      beeswarm(list(setA=list1,setB=list2),col = 2:3)
    }
    if((input$BSTxA3 != "") & (input$BSTxA4 == "")){
      beeswarm(list(setA=list1,setB=list2,setC=list3),col = 2:4)
    }
    if((input$BSTxA4 != "") & (input$BSTxA5 == "")){
      beeswarm(list(setA=list1,setB=list2,setC=list3,setD=list4),col = 2:5)
    }
    if(input$BSTxA5 != "" & input$BSTxA4 != "" & input$BSTxA3 != "" & input$BSTxA2 != "" & input$BSTxA1 != ""){
      beeswarm(list(setA=list1,setB=list2,setC=list3,setD=list4,setE=list5),col = 2:6)
    }
  })



  HeatmapInput <- reactive({
    setA=strtoi(unlist(strsplit(input$HeatTxA1,split = '\n' )))
    setB=strtoi(unlist(strsplit(input$HeatTxA2,split = '\n' )))
    setC=strtoi(unlist(strsplit(input$HeatTxA3,split = '\n' )))
    setD=strtoi(unlist(strsplit(input$HeatTxA4,split = '\n' )))
    setE=strtoi(unlist(strsplit(input$HeatTxA5,split = '\n' )))
    par(mar=c(0,0,0,0))
    if((input$HeatTxA2 != "") & (input$HeatTxA3 == "")){
      plot.data <- rbind(setA, setB)
      heatmap.2(plot.data,cexCol=input$cex,cexRow = input$cex, scale = input$Bx_r1, Rowv = input$Bx_cb_row, Colv = input$Bx_cb_col)
    }
    if((input$HeatTxA3 != "") & (input$HeatTxA4 == "")){
      plot.data <- rbind(setA, setB , setC)
      heatmap.2(plot.data,cexCol=input$cex,cexRow = input$cex, Rowv = input$Bx_cb_row, Colv = input$Bx_cb_col)
    }
    if((input$HeatTxA4 != "") & (input$HeatTxA5 == "")){
      plot.data <- rbind(setA, setB , setC, setD)
      heatmap.2(plot.data,cexCol=input$cex,cexRow = input$cex, Rowv = input$Bx_cb_row, Colv = input$Bx_cb_col)
    }
    if(input$HeatTxA5 != "" & input$HeatTxA4 != "" & input$HeatTxA3 != "" & input$HeatTxA2 != "" & input$HeatTxA1 != ""){
      plot.data <- rbind(setA, setB , setC, setD,setE)
      heatmap.2(plot.data,cexCol=input$cex,cexRow = input$cex, Rowv = input$Bx_cb_row, Colv = input$Bx_cb_col)
    }
  })


  boxPlotInput <- reactive({
    list1=strtoi(unlist(strsplit(input$BoxTxA1,split = '\n' )))
    list2=strtoi(unlist(strsplit(input$BoxTxA2,split = '\n' )))
    list3=strtoi(unlist(strsplit(input$BoxTxA3,split = '\n' )))
    list4=strtoi(unlist(strsplit(input$BoxTxA4,split = '\n' )))
    list5=strtoi(unlist(strsplit(input$BoxTxA5,split = '\n' )))

    if((input$BoxTxA2 != "") & (input$BoxTxA3 == "")){
      boxplot(list(list1,list2),lwd=input$lwd, pch=input$pch,col=rainbow(5))
    }
    if((input$BoxTxA3 != "") & (input$BoxTxA4 == "")){
      boxplot(list1,list2,list3,lwd=input$lwd, pch=input$pch,col=rainbow(5))
    }
    if((input$BoxTxA4 != "") & (input$BoxTxA5 == "")){
      boxplot(list1,list2,list3,list4,lwd=input$lwd, pch=input$pch,col=rainbow(5))
    }
    if(input$BoxTxA5 != "" & input$BoxTxA4 != "" & input$BoxTxA3 != "" & input$BoxTxA2 != "" & input$BoxTxA1 != ""){
      boxplot(list1,list2,list3,list4,list5,lwd=input$lwd, pch=input$pch,col=rainbow(5))
    }
  })

  plotInput2 <- reactive({
    list1=unlist(strsplit(input$TxA1,split = '\n' ))
    list2=unlist(strsplit(input$TxA2,split = '\n' ))
    list3=unlist(strsplit(input$TxA3,split = '\n' ))
    list4=unlist(strsplit(input$TxA4,split = '\n' ))
    list5=unlist(strsplit(input$TxA5,split = '\n' ))
     if((input$TxA2 != "") & (input$TxA3 == "")){
       grid.draw(venn.diagram(list(A=list1,B=list2),cex=input$cex,fill= 2:3,filename=NULL,alpha=input$alpha, lty=input$lty, lwd=input$lwd))
     }
    if((input$TxA3 != "") & (input$TxA4 == "")){
      grid.draw(venn.diagram(list(A=list1,B=list2,C=list3),,cex=input$cex,fill= 2:4,filename=NULL,alpha=input$alpha, lty=input$lty, lwd=input$lwd))
    }
    if((input$TxA4 != "") & (input$TxA5 == "")){
      grid.draw(venn.diagram(list(A=list1,B=list2,C=list3,D=list4),,cex=input$cex,fill= 2:5,filename=NULL,alpha=input$alpha, lty=input$lty, lwd=input$lwd))
    }
    if(input$TxA5 != "" & input$TxA4 != "" & input$TxA3 != "" & input$TxA2 != "" & input$TxA1 != ""){
      grid.draw(venn.diagram(list(A=list1,B=list2,C=list3,D=list4,E=list5),cex=input$cex,fill= 2:6,filename=NULL,alpha=input$alpha, lty=input$lty, lwd=input$lwd))
    }
  })


  output$tb <-renderUI({

      tabsetPanel(tabPanel("File1", tableOutput("table1")),tabPanel("BoxPlot", plotOutput("boxPlot")),tabPanel("File3", tableOutput("table3")),tabPanel("File4", tableOutput("table4")),tabPanel("File5", tableOutput("table5")),tabPanel("VennDiag", tableOutput("vennDiag")))


  })

  })



# Run the application
shinyApp(ui = ui, server = server)

