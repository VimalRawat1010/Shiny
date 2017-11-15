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
library(DBI)
library(RMySQL)
library(RCurl)

library(d3Network)
library(networkD3)

library(OmicCircos);


library(shinydashboard)



ui <- navbarPage(
                  HTML(paste0(textOutput("Vimal Rawat")),paste0("<b> <h4> <a href=", shQuote("https://scholar.google.ch/citations?user=NNotcvEAAAAJ&hl=en"), ">", "About Me", "</a> </h3></b>")),
                  windowTitle = "Expression",
                  inverse = TRUE,
                  tabPanel(HTML(paste0("<b> <h4> <a href=", shQuote("https://github.com/VimalRawat1010"), ">", "&nbsp;Github", "</a> </h3></b>"))),
                  tabPanel(HTML(paste0("<h3><b>&nbsp;&nbsp;&nbsp;&nbsp;Arabidopsis Gene Expression Landscape</b><h1>"))),
                  fluidPage(theme ="darkly.css",
                            # Load d3.js
                            tags$head(
                              tags$script(src = 'http://d3js.org/d3.v3.min.js')
                            ),
                            
                            # Application title
                            titlePanel(h2("Paste/Type-in ATG ids here"),windowTitle ="AthExp"),
                            inverse = FALSE,
                        sidebarLayout(
                           sidebarPanel(
                               textAreaInput("TxA1", "Type ATG ids here: ",rows=5, width='100%', resize = "both", value="AT1G10220\nAT1G27280\nAT1G01400\nAT1G01120\nAT1G01110\nAT1G01130\n"),
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
                                                           #HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                           selectInput("tableGene", "Choose a Source:", c("MA"= "MA", "RNAseq" = "RNAseq"), selected=NULL),
                                                            h4(textOutput("geneExp")),
                                                            h4(tableOutput("data_sets"))
                                                            ),
                                                  tabPanel(h4("Expression profile"),
                                                            #HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                           selectInput("tableExp", "Choose a Source:", c("MA"= "MA", "RNAseq" = "RNAseq"), selected=NULL),
                                                           checkboxInput("Bx_cb_row", "Show dendogram for row", value = TRUE, width = NULL),
                                                           checkboxInput("Bx_cb_col", "Show dendogram for column", value = TRUE, width = NULL),
                                                           radioButtons("Rb_col", "Select Color pallete", c("Red-Green" = "redgreen","Heat" = "heat.colors","Terrain" = "terrain.colors","Topo" = "topo.colors","CM" = "cm.colors"), inline = TRUE),
                                                           #tags$style(HTML(".radio-inline {margin-right: 80px;}")),
                                                           h5(uiOutput("tissue")),
                                                           sliderInput("cex1","Row Label size", min=0.1, max=2, value=1),
                                                           sliderInput("cex2","Col Label size", min=0.1, max=2, value=1),
                                                           h5(plotOutput("heatmap"))
                                                           ),
                                                  tabPanel(h4("Network structure"),
                                                            HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                           sliderInput('slider', label = 'Choose node opacity',min = 0, max = 1, step = 0.01, value = 0.5),
                                                           htmlOutput('networkPlot')
                                                            
                                                            ),
                                                 tabPanel(h4("Genomic Location"),
                                                          HTML("<h4><b>How to use this webtool to plot your data.</b></h4>"),
                                                          h5(plotOutput("locationCircos"))
                                                          #htmlOutput('networkPlot')
                                                          
                                                 )
                                                 
                                                 
                                                 
                                                    ),
                                    verbatimTextOutput("TxA1")
                                              )
                                        )
                              )
)


######################################################3

MisLinks <- read.table("/home/vimal/Software/Github/Latest/Shiny/heatmap/MisLinks", header=T)
MisNodes <- read.table("/home/vimal/Software/Github/Latest/Shiny/heatmap/MisNodes", header=T)



######################################################3



# Define server logic required to draw a histogram
server <- function(input, output,session) {


  
      # Getting data from uploaded file
      fileText1 <- eventReactive(input$file,{
                  filePath  <- input$file$datapath
                  fileText  <- paste(readLines(filePath), collapse = "\n")
                  # update text area with file content
                  updateTextAreaInput(session, "TxA1", value = fileText)
              })
      
      # overwritting data of uploaded file to TextArea
      output$TxA1 <- renderPrint({ fileText1() })     
      
      # Getting Gene Exp in Table
      output$geneExp <- renderText({ 
        return(getGeneExp())
      })
      
      
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
          
          rs= dbGetQuery(conn1,paste0("SELECT  *  FROM ", input$tableGene , " WHERE Gene IN ('" , geneList , "');"))
          return (rs)
        })
      })
      

  output$tissue <- renderUI({
    
                  checkboxGroupInput("Tissue", "Tissue", as.list(data_sets()), inline = TRUE, selected = "Gene")
                    })
  
  data_sets  <- reactive({
    return (colnames(getHeatmap()))
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
                #tissueList  <- gsub("\n", "','", as.symbol(input$tissue))
                #print(geneList)
                rs= dbSendQuery(conn,paste0("SELECT  *  FROM ", input$tableExp , " WHERE Gene IN ('" , geneList , "');"))
                heatmap.df = fetch(rs, n=-1)
       return (heatmap.df)

  }

  
  output$heatmap <- renderPlot({
    data <- getData()
    #print(data)
    row.names(data) <- data$Gene
    data <- data[-c(1)]
    #print(data)
    heatmap.2(as.matrix(data), trace="none", scale="row",col= input$Rb_col, cexRow = input$cex1, cexCol = input$cex2, Rowv = input$Bx_cb_row, Colv = input$Bx_cb_col)
  })
  
  
#### This function gives sexpression values for a subset of tissue   
  getData<-reactive({
    data <- getHeatmap()
    data <- data[,input$Tissue]
    return(data)
  })
  
  
  
  getLocation <- function(){
    
      conn <- dbConnect(
      drv = RMySQL::MySQL(),
      dbname = "Athal_Expression",
      host = "localhost",
      username = "root",
      password = "abc123")
    on.exit(dbDisconnect(conn), add = TRUE)
    geneList  <- gsub("\n", "','", input$TxA1)
    #tissueList  <- gsub("\n", "','", as.symbol(input$tissue))
    #print(geneList)
    rs= dbSendQuery(conn,paste0("SELECT  *  FROM GeneCoord WHERE value IN ('" , geneList , "');"))
    location.df = fetch(rs, n=-1)
    return (location.df)
    
  }
  
  
  
  getCircosData<-reactive({
    data <- getLocation()
    return(data)
  })
  
  
  output$locationCircos <- renderPlot({

    #print(data)
    chr.n  <- 7;
    chrN     <- paste0("Chr", 1:5);
    chrEN     <- c("ChrC", "ChrM");
    chr = c(chrN, chrEN)
    val     <- c(10, 11,20,15,20,0.05,0.1) ;
    val2     <- c(15,15,15,15,14) ;
    seg.dat <- data.frame(chr=chr, start=rep(1, chr.n), end=c(34.964571,22.037565,25.499034,20.862711,31.270811,0.154478,0.366924), name=chr, value=val);
    #seg.dat2 <- data.frame(chr=chrN, start=rep(10, 5), end=c(11,11,11,11,11), name=chrN, value=val2);

    seg.dat2 <- getCircosData()
    
    seg.c   <- segAnglePo(seg.dat, seg=chr);
    
    cols    <- "black";
    
    par(mar=c(2, 2, 2, 2))
    plot(c(1,800), c(1,800), type="n", axes=FALSE, xlab="", ylab="");
    circos(R=300,type="chr", cir=seg.c,  W=80 , print.chr.lab=TRUE, cex = 10)
    circos(R=250, cir=seg.c, W=40, mapping=seg.dat2, type="b", col.v=5, col="black", B=T, cex=abs(seg.dat[,5])*1.5);
    circos(R=220, cir=seg.c, W=40, mapping=seg.dat2, type="label2", col.v=5, col=cols, B=T, cex=0.8);

  })




 ######################################################################
  
  getMisNodes<-reactive({
          #geneList  <- gsub("\n", ",", input$TxA1)
          geneList <- as.list(strsplit(input$TxA1, "\n")[[1]])
          MisNodes <-  MisNodes[MisNodes$name %in% geneList,]
          #print(class(MisNodes))
          MisNodes <- MisNodes[order(MisNodes$name),]
          MisNodes$ID <- 1:length(MisNodes$ID)
          return(MisNodes)
  })
  
  
  ########### Function to get get Link Dataframe
  f <- function(dataFrame, link.df) {
          #print(link.df$target)
          names <- as.vector(dataFrame[,1])
          n.links = length(names) * 5 
          links <- data.frame(source = numeric(n.links), target = numeric(n.links), value = numeric(n.links))
          #colnames(links) <- c("source", "target", "value")
          count = 0
          for (i in 1:length(names))
          {
            for (j in 1:(length(names)))
              {
                index1 = which(link.df$source %in%  names[i])
                index2 = which(link.df$target %in%  names[j] )
                machElement = intersect(index1,index2)#index1 <- unlist(index1)
                index2 <- unlist(index2)
                
             if(length(machElement) != 0)
             {
               count = count+1
               #print(machElement)
               #print(i)
               #print(j)
               #print("MATCH")
               links$source[count] <- i-1
               links$target[count] <- j-1
               links$value[count] <- 1
               index1 <- -1
               index2 <- -2
               #print(links)
             }
            
            }
   
          }

          return(links)
        }
  
  
  
  getMisLinks<-reactive({
    nodes <- getMisNodes()
    B <- f(nodes, MisLinks)
    return(B)
  })
  
  
  output$networkPlot <- renderPrint({
    A <- getMisNodes()
    B <- getMisLinks()
    #print(A)
    #print(B)
    #print(MisLinks)
    
    d3ForceNetwork(Nodes = A,
                   Links = B,  
                   Source = "source", Target = "target", 
                   Value = "value", NodeID = "name", 
                   Group = "group", width = 700, height = 700, 
                   opacity = input$slider, standAlone = FALSE,
                   
                   parentElement = '#networkPlot')
    
  })
  

  
  
  
  
}

# Run the application
shinyApp(ui = ui, server = server)
