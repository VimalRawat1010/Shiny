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



ui <- navbarPage(
                  HTML(paste0(textOutput("Vimal Rawat")),paste0("<b> <h4> <a href=", shQuote("https://scholar.google.ch/citations?user=NNotcvEAAAAJ&hl=en"), ">", "About Me", "</a> </h3></b>")),
                  windowTitle = "Expression",
                  inverse = TRUE,
                  tabPanel(HTML(paste0("<b> <h4> <a href=", shQuote("https://github.com/VimalRawat1010"), ">", "&nbsp;Github", "</a> </h3></b>"))),
                  tabPanel(HTML(paste0("<h3><b>&nbsp;&nbsp;&nbsp;&nbsp;Arabidopsis Gene Expression Landscape</b><h1>"))),
                  fluidPage(theme ="darkly.css",
                            # Application title
                            titlePanel(h2("Paste/Type-in ATG ids here"),windowTitle ="AthExp"),
                            inverse = FALSE,
                        sidebarLayout(
                           sidebarPanel(
                               textAreaInput("TxA1", "Type ATG ids here: ",rows=5, width='100%', resize = "both", value=""),
                               fileInput("file", "or Upload the file with ATG ids"),
                               h3(actionButton("enterdata", "Enter", style='font-size:100%'))

                                        ),

                           mainPanel(


                                    tabsetPanel( type="tabs",
                                                  tabPanel(h4("About web-tool"),
                                                            HTML("<h4>This section describe how to use this web tool to plot your data.</h4>"),
                                                           br(),
                                                            HTML("To get the tissue-specific expression information of desired list of genes\n paste ATG ids of Arabidopsis genes in section on the left.")
                                                  ),
                                                  tabPanel(h4("Gene list"),
                                                           HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                            h4(textOutput("text1")),
                                                            h4(tableOutput("data_sets"))
                                                            ),
                                                  tabPanel(h4("Expression profile"),
                                                            HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                           checkboxInput("Bx_cb_row", "Show dendogram for row", value = TRUE, width = NULL),
                                                           checkboxInput("Bx_cb_col", "Show dendogram for column", value = TRUE, width = NULL),

                                                           sliderInput("cex1","Row Label size", min=0.1, max=2, value=1),
                                                           sliderInput("cex2","Col Label size", min=0.1, max=2, value=1),
                                                           h5(plotOutput("heatmap"))
                                                           ),
                                                  tabPanel(h4("Gene structure"),
                                                            HTML("<h4><b>How to use this webtool to plot your data.</b></h4>")
                                                           )
                                                    ),
                                    verbatimTextOutput("TxA1")
                                              )
                                        )
                              )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  fileText1 <- eventReactive(input$file,{
                              filePath  <- input$file$datapath
                              fileText  <- paste(readLines(filePath), collapse = "\n")
                              # update text area with file content
                              updateTextAreaInput(session, "TxA1", value = fileText)
                              })

  output$TxA1 <- renderPrint({ fileText1() })

  getGeneExp <- eventReactive(input$enterdata,{
                              output$data_sets  <- renderTable({
                              conn1 <- dbConnect(
                              drv = RMySQL::MySQL(),
                              dbname = "Athal_Expression",
                              host = "localhost",
                              username = "root",
                              password = "abc123")
                              on.exit(dbDisconnect(conn1), add = TRUE)
                              geneList  <- gsub("\n", "','", input$TxA1)

                      return (dbGetQuery(conn1,paste0("SELECT  *  FROM RNAseq WHERE Gene IN ('" , geneList , "');")))

                              })
                          })

  getHeatmap <- function(){

                conn <- dbConnect(
                drv = RMySQL::MySQL(),
                dbname = "Athal_Expression",
                host = "localhost",
                username = "root",
                password = "abc123")
                on.exit(dbDisconnect(conn), add = TRUE)
                geneList  <- gsub("\n", "','", input$TxA1)
                rs= dbSendQuery(conn,paste0("SELECT  *  FROM RNAseq WHERE Gene IN ('" , geneList , "');"))
                heatmap.df = fetch(rs, n=-1)
       return (heatmap.df)

  }




  output$text1 <- renderPrint({ getGeneExp() })

  output$heatmap <- renderPlot({
                    data <- getHeatmap()
                    row.names(data) <- data$Gene
                    data <- data[,2:22]
                    heatmap.2(data.matrix(data), trace="none" , cexRow = input$cex1, cexCol = input$cex2, Rowv = input$Bx_cb_row, Colv = input$Bx_cb_col)
    })



}

# Run the application
shinyApp(ui = ui, server = server)
