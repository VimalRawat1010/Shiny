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
                  fluidPage(theme ="bootstrap.css",
                            # Application title
                            titlePanel(h2("Make selection"),windowTitle ="Pfam"),
                            inverse = FALSE,
                        sidebarLayout(
                           sidebarPanel(
                               textAreaInput("TxA1", "Type ATG ids here: ",rows=5, width='100%', resize = "both", value=""),
                               fileInput("file", "or Upload the file with ATG ids and Gene as header"),
                               h3(actionButton("enterdata", "Enter", style='font-size:100%'))
                           
                                        ),
                                                   
                           mainPanel( 
                                      
                                    
                                    tabsetPanel( type="tabs",
                                                  tabPanel(h4("About web-tool"),
                                                            HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                            HTML("This section describe how to use this web tool to plot your data.")
                                                          ),
                                                  tabPanel(h4("Gene list"),
                                                           HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                            h5(textOutput("text1")), 
                                                            h5(tableOutput("data_sets")), 
                                                            h5(tableOutput("tbl1"))
                                                            ),
                                                  tabPanel(h4("Expression profile"),
                                                            HTML("<h4><b>How to use this webtool to plot your data.</b></h4>")
                                                            ),
                                                  tabPanel(h4("Gene structure"),
                                                            HTML("<h4><b>How to use this webtool to plot your data.</b></h4>")
                                                           )
                                                    ),
                                    
                                    verbatimTextOutput("TxA1"), tableOutput("tb"), plotOutput("distPlot")
                                              )
                                        )
                              )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {

  
  fileText1 <- eventReactive(input$file, {
    filePath  <- input$file$datapath
    fileText  <- paste(readLines(filePath), collapse = "\n")
    # update text area with file content
    updateTextAreaInput(session, "TxA1", value = fileText)
  })
  
  output$TxA1 <- renderPrint({ fileText1() })
  
  getTopGenes <- eventReactive(input$enterdata,{
  output$data_sets  <- renderTable({
    conn1 <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "Athal_Expression",
      host = "localhost",
      username = "root",
      password = "abc123")
    on.exit(dbDisconnect(conn1), add = TRUE)
    return (dbGetQuery(conn1,"SELECT  *  FROM RNAseq LIMIT 10;"))
    
  })
  
  })

  
  
  output$text1 <- renderText({
    getTopGenes()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
