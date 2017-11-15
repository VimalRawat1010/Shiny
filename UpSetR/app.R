#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
options(shiny.maxRequestSize=30*1024^2)

library(shiny)
library(ggplot2)
library(gridExtra)
library(plyr)
library(UpSetR)
library(shinythemes)
library(RSVGTipsDevice)
source("converters.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  title = "", id="main_panel",
  theme = shinythemes::shinytheme("flatly"),
  tabPanel(value="main",
           title = p("UpSetR", style = "font-size: 20px; padding-bottom: -0.5cm"),
           fluidRow(column(
             6,
             sidebarLayout(
               sidebarPanel(
                 h2("Welcome to the UpSetR Shiny App!"),
                 br(),
                 tags$p(HTML(
                   "UpSetR generates static <a href=\"http://vcg.github.io/upset/?dataset=0&duration=1000&orderBy=subsetSize&grouping=groupByIntersectionSize&selection=\">UpSet plots</a>.
                   The UpSet technique visualizes set intersections in a matrix layout
                   and introduces aggregates based on groupings and queries. The matrix layout enables the effective
                   representation of associated data, such as the number of elements in the aggregates and intersections,
                   as well as additional summary statistics derived from subset or element attributes."
                 )),
                 br(),
                 h4(
                   HTML("To begin, input your data using one of the three input styles.")
                 ),
                 
                 tags$p(
                   HTML(
                     "<ol start='1'> <li>\"File\" takes a correctly formatted.csv file.</li>
                     <li>\"List\" takes up to 6 different lists that contain unique elements, similar to that used in
                     the web applications BioVenn <a href='http://www.biomedcentral.com/content/pdf/1471-2164-9-488.pdf'> (Hulsen et al., 2008)</a> and jvenn <a href=http://www.biomedcentral.com/content/pdf/1471-2105-15-293.pdf> (Bardou et al., 2014) </a></li>
                     <li>\"Expression\" takes the input used by the venneuler R package <a href ='https://cran.r-project.org/web/packages/venneuler/venneuler.pdf'> (Wilkinson, 2015) </a></li>
                     </ol>"
                   )
                   ),
                 h4('To view and explore your data click on the "Plot!" button.'),
                 br(),
                 tags$p(HTML(
                   "For further details about the original technique see the <a href=\"http://www.caleydo.org/tools/upset/\">UpSet website</a>.
                   You can also check out the <a href=\"https://github.com/hms-dbmi/UpSetR\"> UpSetR R package </a> and its source code."
                 )),
                 tags$p(HTML(
                   "If you use UpSetR in a paper, please cite: "
                 )),
                 tags$p(HTML(
                   "<blockquote style='font-size:14px'>Alexander Lex, Nils Gehlenborg, Hendrik Strobelt, Romain Vuillemot, Hanspeter Pfister,
                   UpSet: Visualization of Intersecting Sets,
                   IEEE Transactions on Visualization and Computer Graphics (InfoVis '14), vol. 20, no. 12, pp. 1983–1992, 2014.
                   doi:10.1109/TVCG.2014.2346248 </blockquote>"
                   )),
                 width = 12
                   ),
               mainPanel(img(src = 'Rplot.png', align = "center", width="675px", height="457px"))
                 )
                 ), column(
                   6,
                   tabsetPanel(
                     tabPanel("Option 1: File",
                              column(12,
                                     sidebarLayout(
                                       fluidRow(
                                         mainPanel(
                                           tags$h3(HTML("<u>Instructions</u>")),
                                           p(
                                             "To use this method, begin by uploading a correctly formatted .csv file.
                                             A correctly formatted .csv file is encoded in binary and set up so that each
                                             column represents a set, and each row represents an element. If an element is
                                             in the set it is represented as a 1 in that position. If an element is not in
                                             the set it is represented as a 0."
                                           ),
                                           br(),
                                           p(
                                             "After uploading the file, choose the correct separator. If the elements in each
                                             column are seperated by a ' , ' choose comma, by a ' ; ' choose semicolon, or by tabs choose tab."
                                           ),
                                           #          h3("Begin by uploading your correctly formatted .csv file, and selecting the correct separator."),
                                           #          h4("A correctly formatted data set will denote the sets in binary. (e.g. The movie genres in the table below.)"),
                                           #          h5(br()),
                                           #          h5("For example, in this data set Toy Story is considered only a comedy, whereas Grumpier Old Men is considered both a comedy and a romance."),
                                           #          br(),
                                           #          h5("Additional attributes may be present in the data. (e.g. ReleaseDate, AvgRating in the table below.)"),
                                           br(),
                                           # img(src='Data_setup.png', align = "center"),
                                           # h6("Want some data to get a feel for UpSetR?"),
                                           p(
                                             "To see what a correctly formatted data set with a comma (' , ') seperator
                                             looks like download the movies file below."
                                           ),
                                           tags$a(href = "movies.csv", "Download the movies data set here!"),
                                           br(),
                                           br(),
                                           width = 12
                                           )
                                           ),
                                       fluidRow(
                                         sidebarPanel(
                                           fluidRow(fileInput(
                                             'file1',
                                             label = h5("Upload file"),
                                             accept = c(
                                               'text/csv',
                                               'text/comma-separated-values',
                                               'text/tab-separated-values',
                                               '.csv',
                                               '.tsv'
                                             )
                                           )),
                                           fluidRow(checkboxInput('header', label = 'Header', TRUE)),
                                           fluidRow(radioButtons(
                                             'sep',
                                             label = h6('Separator'),
                                             choices = c(
                                               Comma = ',',
                                               Semicolon = ';',
                                               Tab = '\t'
                                             ),
                                             selected = ','
                                           )),
                                           fluidRow(br(),
                                                    tags$button(id="confirm1", 
                                                                type="button", 
                                                                class="btn action-button btn-large btn-primary", 
                                                                HTML('<i class="icon-star"></i>Plot!'))
                                           ),
                                           width = 12
                                         ))))),
                     tabPanel("Option 2: List",
                              column(12,
                                     sidebarLayout(
                                       fluidRow(
                                         mainPanel(
                                           tags$h3(HTML("<u>Instructions</u>")),
                                           p(
                                             "The input style of lists is useful when wanting to compare sets by supplying , say
                                             a list of gene IDs or SNPs. To use this format enter a list of elements seperated by a
                                             comma to each input box. These elements can be entered as numbers, letters, IDs, words, etc.
                                             The only limitation to entering the lists is having spaces in the element names. As an
                                             alternative an underscore (' _ ') character can be used to to substitute for the spaces.
                                             To give each set a name, enter the names into the bars where the word 'List' followed by
                                             a number is grayed out."
                                           ),
                                           br(),
                                           br(),
                                           p(
                                             "To see how the list format works copy and paste each list of letters into their respective input boxes."
                                           ),
                                           br(),
                                           p("List 1: A, B, C, D, E, F, G, H"),
                                           p("List 2: A, B, D, F, I, J, K, L"),
                                           p("List 3: A, H, J, M, N, O, P, Q"),
                                           p("List 4: B, L, O, P, R, S, T, U")
                                           #          h3("The set visualization web apps BioVenn and jvenn utilize the input style of lists containing unique elements."),
                                           #          h3("This type of input is useful when wanting to compare sets by supplying, say, a list of gene IDs or SNPs"),
                                           #          h3("Here is an example of the input. Copy and paste them into the respective lists to see how it works."),
                                           #          br(),
                                           #          h4("List 1: A, B, C, D, E, F, G, H"),
                                           #          h4("List 2: A, B, D, F, I, J, K, L"),
                                           #          h4("List 3: A, H, J, M, N, O, P, Q"),
                                           #          h4("List 4: B, L, O, P, R, S, T, U"),
                                           #          br(),br(),
                                           #          h4("Example of jvenn with cancer SNPs:"),
                                           #          img(src='jvenn.png', align = "right", width="103%")
                                           ,
                                           width = 12
                                           )
                                         ),
                                       fluidRow(
                                         sidebarPanel(tags$style(type = "text/css", "textarea {width:100%}"),
                                                      fluidRow(
                                                        column(6,
                                                               tags$textarea(
                                                                 id = 'name1',
                                                                 placeholder = "List 1",
                                                                 rows = 1
                                                               ),
                                                               tags$style(type = "text/css", "textarea {width:100%}"),
                                                               tags$textarea(
                                                                 id = 'list1',
                                                                 placeholder = '',
                                                                 rows = 3
                                                               ),
                                                               tags$textarea(
                                                                 id = 'name2',
                                                                 placeholder = "List 2",
                                                                 rows = 1
                                                               ),
                                                               tags$style(type = "text/css", "textarea {width:100%}"),
                                                               tags$textarea(
                                                                 id = 'list2',
                                                                 placeholder = '',
                                                                 rows = 3
                                                               ),
                                                               tags$textarea(
                                                                 id = 'name3',
                                                                 placeholder = "List 3",
                                                                 rows = 1
                                                               ),
                                                               tags$style(type = "text/css", "textarea {width:100%}"),
                                                               tags$textarea(
                                                                 id = 'list3',
                                                                 placeholder = '',
                                                                 rows = 3
                                                               )),
                                                        column(6,
                                                               tags$textarea(
                                                                 id = 'name4',
                                                                 placeholder = "List 4",
                                                                 rows = 1
                                                               ),
                                                               tags$style(type = "text/css", "textarea {width:100%}"),
                                                               tags$textarea(
                                                                 id = 'list4',
                                                                 placeholder = '',
                                                                 rows = 3
                                                               ),
                                                               tags$textarea(
                                                                 id = 'name5',
                                                                 placeholder = "List 5",
                                                                 rows = 1
                                                               ),
                                                               tags$style(type = "text/css", "textarea {width:100%}"),
                                                               tags$textarea(
                                                                 id = 'list5',
                                                                 placeholder = '',
                                                                 rows = 3
                                                               ),
                                                               tags$textarea(
                                                                 id = 'name6',
                                                                 placeholder = "List 6",
                                                                 rows = 1
                                                               ),
                                                               tags$style(type = "text/css", "textarea {width:100%}"),
                                                               tags$textarea(
                                                                 id = 'list6',
                                                                 placeholder = '',
                                                                 rows = 3
                                                               )
                                                        )),
                                                      fluidRow(br(),
                                                               tags$button(id="confirm2", 
                                                                           type="button", 
                                                                           class="btn action-button btn-large btn-primary", 
                                                                           HTML('<i class="icon-star"></i>Plot!'))
                                                      ),
                                                      width= 12)
                                       )))),
                     tabPanel(
                       "Option 3: Expression",
                       column(12,
                              sidebarLayout(
                                mainPanel(
                                  tags$h3(HTML("<u>Instructions</u>")),
                                  p(
                                    "The expression input style allows the user to name each intersection and assign
                                    a size to it. When including an intersection of degree 2 or more, the names of the sets
                                    that make up the intersections are seperated by an ampersand (' & '). The names of the sets
                                    can be any string. The only limitations of the set names is that they cant contain any spaces.
                                    As an alternative an underscore (' _ ') character can be used to to substitute for the spaces."
                                  ),
                                  br(),
                                  br(),
                                  p(
                                    "Here is an example of an expression input. Copy and paste it into the input box to see how it works."
                                  ),
                                  br(),
                                  p("A=12, B=12, C=5, A&B=4, A&C=2, B&C=1, A&B&C=2")
                                  #                 h3("One of the most popular set visualization R packages is the venneuler package."),
                                  #                 h3("For this reason we have allowed the same input style for UpSetR."),
                                  #                 h3("Here is an example of the input. Copy and paste it above to see how it works."),
                                  #                 br(),
                                  #                 h4("A=12, B=12, C=5, A&B=4, A&C=2, B&C=1, A&B&C=2"),
                                  #                 br(),br(),
                                  #                 h5("** Restriction: No spaces allowed in the names. _ is acceptable.")
                                  ,
                                  width = 12
                                  ),
                                sidebarPanel(
                                  fluidRow(
                                    h5("expression input"),
                                    tags$style(type = "text/css", "textarea {width:100%}"),
                                    tags$textarea(
                                      id = "venn",
                                      placeholder = '',
                                      rows = 10
                                    )
                                  ),
                                  fluidRow(br(), tags$button(id="confirm3", 
                                                             type="button", 
                                                             class="btn action-button btn-large btn-primary", 
                                                             HTML('<i class="icon-star"></i>Plot!'))),
                                  width = 12)
                                )
                              )
                     )
                     )
                 ))
                 ),
  tabPanel(value="upset_plot",
           p("UpSet Plot", style = "padding-bottom: -0.5cm"),
           sidebarLayout(
             sidebarPanel(
               tabsetPanel(
                 tabPanel('Settings',
                          fluidRow(htmlOutput("sets")),
                          fluidRow(
                            checkboxInput('setorder', label = "Order sets as above", value = FALSE)
                          ),
                          fluidRow(
                            numericInput(
                              "nintersections",
                              label = h6("Limit number of intersections shown to :"),
                              value = 40,
                              min = 1,
                              max = 60
                            )
                          ),
                          fluidRow(
                            selectInput(
                              "order",
                              label = h6("Order by"),
                              choices = list("Degree" = "degree",
                                             "Frequency" = "freq"),
                              selected = "freq"
                            )
                          ),
                          fluidRow(selectInput(
                            "decreasing",
                            h6("Increasing/Decreasing"),
                            choices = list("Increasing" = "inc",
                                           "Decreasing" = "dec"),
                            selected = "dec"
                          )),
                          fluidRow(
                            sliderInput(
                              "mbratio",
                              label = h6("Bar : Matrix ratio"),
                              value = 0.30,
                              min = 0.20,
                              max = 0.80,
                              ticks = FALSE,
                              step = 0.01
                            )
                          ),
                          fluidRow(
                            sliderInput(
                              "angle",
                              h6("Number angles"),
                              min = -90,
                              max = 90,
                              value = 0,
                              step = 1,
                              ticks = F
                            )
                          ),
                          fluidRow(
                            checkboxInput('empty', label = "Show empty intersections", value = FALSE)
                          ),
                          fluidRow(numericInput(
                            "pointsize",
                            label = h6("Point Size"),
                            value = 4,
                            min = 1,
                            max = 15
                          )),
                          fluidRow(
                            radioButtons(
                              inputId = "filetype",
                              label = "File type",
                              choices = list("png", "svg", "pdf")
                            )
                          ),
                          fluidRow(downloadButton(outputId = "down", label = "Download!"))
                          ,
                          width = 2
                 ),
                 tabPanel('Advanced',
                          fluidRow(numericInput(
                            "intersection_title_scale",
                            label = h6("Intersection Size Label Text Scale"),
                            value = 1.2,
                            min = 1,
                            max = 1000
                          )),
                          fluidRow(numericInput(
                            "set_title_scale",
                            label = h6("Set Size Label Text Scale"),
                            value = 1.2,
                            min = 1,
                            max = 1000
                          )),
                          fluidRow(numericInput(
                            "intersection_ticks_scale",
                            label = h6("Intersection Size Ticks Text Scale"),
                            value = 1.2,
                            min = 1,
                            max = 1000
                          )),
                          fluidRow(numericInput(
                            "set_ticks_scale",
                            label = h6("Set Size Ticks Text Scale"),
                            value = 1.2,
                            min = 1,
                            max = 1000
                          )),
                          fluidRow(numericInput(
                            "intersection_size_numbers_scale",
                            label = h6("Intersection Size Numbers Text Scale"),
                            value = 1.2,
                            min = 1,
                            max = 1000
                          )),
                          fluidRow(numericInput(
                            "names_scale",
                            label = h6("Set Names Text Size"),
                            value = 1.2,
                            min = 1,
                            max = 1000
                          )))
               ), width=3),
             mainPanel(htmlOutput('plot_text'),
                       imageOutput('plot')
                       , width = 9)
           )
  ),
  tags$head(tags$style('
                       nav .container:first-child {
                       margin-left:50px; width: 100%;
                       }'))
                 )

# Define server logic required to draw a histogram
server <-shinyServer(function(input, output, session){
  
  attr(input, "readonly") <- FALSE
  
  pushed <- reactiveValues()
  pushed$B <- 0
  
  output$plot_text <- renderUI({
    
    if(is.null(My_data()) == T){
      p(HTML("This is where your plot will show! <br/> There is no data entered.
             Return to the <span> previous tab </span>"),
        HTML(" to input your data."))
    }
    else{
      HTML(" ")
    }
  })
  
  eventReactive(input$confirm1,{
    input$confirm1[1] <- 1
    input$confirm2[1] <- 0
    input$confirm3[1] <- 0
    pushed$B <- 1
    if(is.null(My_data())){
      withProgress(message = "Confirmed failure", value = 0, {setProgress(1)})
    }
    else{
      withProgress(message = "Confirmed success", value = 0, {setProgress(1)})
    }
    input$Select <- NULL
  })  
  
  eventReactive(input$confirm2,{
    input$confirm1[1] <- 0
    input$confirm2[1] <- 1
    input$confirm3[1] <- 0
    pushed$B <- 2
    if(is.null(My_data())){
      withProgress(message = "Confirmed failure", value = 0, {setProgress(1)})
    }
    else{
      withProgress(message = "Confirmed success", value = 0, {setProgress(1)})
    }
    input$Select <- NULL
  })  
  
  eventReactive(input$confirm3,{
    input$confirm1[1] <- 0
    input$confirm2[1] <- 0
    input$confirm3[1] <- 1
    pushed$B <- 3
    if(is.null(My_data())){
      withProgress(message = "Confirmed failure", value = 0, {setProgress(1)})
    }
    else{
      withProgress(message = "Confirmed success", value = 0, {setProgress(1)})
    }
    input$Select <- NULL
  })
  
  confirmed <- reactive({
    one <- input$confirm1[1]
    two <- input$confirm2[1]
    three <- input$confirm3[1]
    all <- c(one,two,three)
    maximum <- which(all == max(all))
    pushed$B <- maximum
    if(maximum==1){
      return(1)
    }
    if(maximum==2){
      return(2)
    }
    if(maximum==3){
      return(3)
    }
    else{
      return(0)
    }
  })
  
  My_dat <- reactive({  
    inFile <- input$file1
    
    if(pushed$B == 0 || length(pushed$B) > 1){
      return(NULL)
    }
    
    if (is.null(inFile))
      return(NULL)
    
    read.csv(inFile$datapath, header = input$header,
             sep = input$sep, quote = input$quote)
  })
  
  venneulerData <- reactive({
    string <- input$venn
    string <- gsub("\n", "", string)
    if(string != ""){
      string <- as.list(unlist(strsplit(string, ",")))
      names <- lapply(string, function(x){x <- unlist(strsplit(x, "=")); x <- x[1]})
      names <- unlist(lapply(names, function(x){x <- gsub(" ", "", x)}))
      values <- as.numeric(unlist(lapply(string, function(x){x <- unlist(strsplit(x,"=")); x <- x[2]})))
      names(values) <- names
      venneuler <- upsetVenneuler(values)
      return(venneuler)
    }
  })
  
  listData <- reactive({
    a <- input$list1; a <- as.list(unlist(strsplit(a,",")))
    a <- unlist(lapply(a, function(x){x <- gsub(" ", "", x)})); a <- a[a != ""]
    b <- input$list2
    b <- as.list(unlist(strsplit(b,",")))
    b <- unlist(lapply(b, function(x){x <- gsub(" ", "", x)})); b <- b[b != ""]
    c <- input$list3
    c <- as.list(unlist(strsplit(c,",")))
    c <- unlist(lapply(c, function(x){x <- gsub(" ", "", x)})); c <- c[c != ""]
    d <- input$list4
    d <- as.list(unlist(strsplit(d,",")))
    d <- unlist(lapply(d, function(x){x <- gsub(" ", "", x)})); d <- d[d != ""]
    e <- input$list5
    e <- as.list(unlist(strsplit(e,",")))
    e <- unlist(lapply(e, function(x){x <- gsub(" ", "", x)})); e <- e[e != ""]
    f <- input$list6
    f <- as.list(unlist(strsplit(f,",")))
    f <- unlist(lapply(f, function(x){x <- gsub(" ", "", x)})); f <- f[f != ""]
    all <- list(a,b,c,d,e,f)
    
    elements <- unique(unlist(all))
    name1 <- input$name1; name2 <- input$name2; name3 <- input$name3; name4 <- input$name4; 
    name5 <- input$name5; name6 <- input$name6;
    if(name1==""){name1<-"List 1"}; if(name2==""){name2<-"List 2"}; if(name3==""){name3<-"List 3"};
    if(name4==""){name4<-"List 4"}; if(name5==""){name5<-"List 5"}; if(name6==""){name6<-"List 6"};
    names <- c(name1, name2, name3, name4, name5, name6)
    data <- unlist(lapply(all, function(x){ x <- as.vector(match(elements, x));}))
    data[is.na(data)] <- as.integer(0); data[data != 0] <- as.integer(1);
    data <- data.frame(matrix(data, ncol = 6, byrow = F))
    names(data) <- names
    
    data <- data[, which(colSums(data) != 0)]
    if(nrow(data) == 0 || is.null(nrow(data))){
      data <- NULL
    }
    
    return(data)
  })
  
  
  My_data <- reactive({
    if(confirmed() == 1){
      My_data <- My_dat()
    }
    else if(confirmed()==2){
      My_data <- listData()
    }
    else if(confirmed()==3){
      My_data <- venneulerData()
    }
    else{
      return(NULL)
    }
    return(My_data)
  })
  
  
  output$data <- renderTable({
    head(My_data(), 10)
  })
  
  #   output$obs <- renderText({
  #     
  #      if(is.null(My_dat()) == F){x<-1} else{x<-0}
  #      if(is.null(listData()) == F){y<-1} else{y<-0}
  #      if(is.null(venneulerData()) == F){z<-1} else{z<-0}
  #      if((x+y+z)>1){
  #        totalobs <- "You have data in two different input formats. Please remove data from one of the formats."
  #      }
  #     if(length(My_data()) == 0){
  #       totalobs <- NULL
  #     }
  #     else{
  #       totalobs <- as.character(nrow(My_data()))
  #       totalobs <- paste("Total Columns:", totalobs, "\n", sep = " ")
  #     }
  #     return(totalobs)
  #   })
  
  #   output$datatable <- renderText({
  #     if(is.null(My_data()) == T){
  #       text <- paste("---Sample Table of Data---\n", "\n   No Data Entered")
  #     }
  #     else{
  #       text <- paste("---Sample Table of Data---\n")
  #     }
  #     return(text)
  #   })
  
  FindStartEnd <- function(data){
    startend <- c()
    for(i in 1:ncol(data)){
      column <- data[, i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1" && (length(column) == 2))){
        startend[1] <- i
        break
      }
      else{
        next
      }
    }
    for(i in ncol(data):1){
      column <- data[ ,i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1") && (length(column) == 2)){
        startend[2] <- i
        break
      }
      else{
        next
      }
    }
    return(startend)
  }
  
  startEnd <- reactive({
    startEnd <- FindStartEnd(My_data())
  })
  
  setSizes <- reactive({
    if(is.null(My_data()) != T){
      sizes <- colSums(My_data()[startEnd()[1]:startEnd()[2]])
      sizes <- sizes[order(sizes, decreasing = T)]
      #     if(length(Specific_sets()) == 0){
      #       sizes <- sizes[head(names(sizes), 5)]
      #     }
      #     else{
      #       sizes <- sizes[match(Specific_sets(), names(sizes))]
      #     }
      names <- names(sizes); sizes <- as.numeric(sizes);
      maxchar <- max(nchar(names))
      total <- list()
      for(i in 1:length(names)){
        spaces <- as.integer((maxchar - nchar(names[i]))+1)
        spaces <- paste(rep(" ", each=spaces), collapse = "")
        total[[i]] <- paste(paste(names[i], ":", sep=""), spaces, sizes[i], "\n", sep="")
      }
      total <- unlist(total)
      total <- paste(total, collapse = " ")
      return(total)
    }
    else{
      return(NULL)
    }
  })
  
  output$setsizes <- renderText({
    if(is.null(setSizes()) != T){
      paste("---Set Sizes---\n", setSizes())
    }
    else{
      paste("---Set Sizes---\n", "\n No Data Entered")
    }
  })
  
  #   intersectionSizes <- reactive({
  #     if(is.null(My_data()) != T){
  #     data <- My_data()[startEnd()[1]:startEnd()[2]]
  #     if(length(Specific_sets()) == 0){
  #       topfive <- colSums(data)
  #       topfive <- as.character(head(names(topfive[order(topfive, decreasing = T)]), 5))
  #       data <- data[topfive]
  #     }
  #     else{
  #       data <- data[Specific_sets()]
  #     }
  #     data <- data[which(rowSums(data) != 0), ]
  #     ncols <- ncol(data)
  #     data <- count(data)
  #     data <- data[order(data$freq, decreasing = T), ]
  #     names <- apply(data[1:ncols], 1, function(x){ name <- names(x[which(x == 1)]); return(name);})
  #     nameSize <- list()
  #     for(i in 1:length(names)){
  #       if(length(names[[i]]) > 1){
  #         names[[i]] <- paste(names[[i]], collapse = "|")
  #       }
  #     }
  #     maxchar <- max(nchar(names))
  #     for(i in 1:length(names)){
  #       spaces <- as.integer((maxchar - nchar(names[[i]]))+1)
  #       spaces <- paste(rep(" ", each=spaces), collapse = "")
  #       nameSize[[i]] <- paste(paste(names[[i]], ":", sep = ""), spaces, data$freq[i], "\n", sep = "")
  #     }
  #     
  #     namesSize <- unlist(nameSize)
  #     nameSize <- paste(nameSize, collapse = " ")
  #     return(nameSize)
  #     }
  #     else{
  #       return(NULL)
  #     }
  #     
  #   })
  
  #    output$intersections <- renderText({
  #      if(is.null(intersectionSizes()) != T){
  #      paste("---Intersection Sizes---\n", intersectionSizes())
  #      }
  #      else{
  #        paste("---Intersection Sizes---\n", "\n   No Data Entered")
  #      }
  #   })
  
  Specific_sets <- reactive({
    Specific_sets <- as.character(c(input$Select))
  })
  
  output$sets <- renderUI({
    if(is.null(My_data()) == T){
      sets <-  selectInput('Select', h6("Select at least two sets : "),
                           choices = NULL,
                           multiple=TRUE, selectize=TRUE, selected = Specific_sets())
    }
    else{
      data <- My_data()[startEnd()[1]:startEnd()[2]]
      topfive <- colSums(data)
      topfive <- as.character(head(names(topfive[order(topfive, decreasing = T)]), 5))
      sets <- selectInput('Select', h6("Select specific sets : "),
                          choices = as.character(colnames(My_data()[ , startEnd()[1]:startEnd()[2]])),
                          multiple=TRUE, selectize=TRUE, selected = topfive)
    }
    return(sets)
  })
  
  
  setOrder <- reactive({
    if(isTRUE(input$setorder)){
      return(TRUE)
    }
    else{
      return(FALSE)
    }
  })
  
  mat_prop <- reactive({
    mat_prop <- input$mbratio
  })
  bar_prop <- reactive({
    bar_prop <- (1 - input$mbratio)
  })
  
  orderdat <- reactive({
    orderdat <- as.character(input$order)
    if(orderdat == "degree"){
      orderdat <- c("degree")
    }
    else if(orderdat == "freq"){
      orderdat <- "freq"
    }
    return(orderdat)
  })
  
  decrease <- reactive({
    decrease <- as.character(input$decreasing)
    if(decrease == "inc"){
      decrease <- FALSE
    }
    else if(decrease == "dec"){
      decrease <- TRUE
    }
    return(decrease)
  })
  
  number_angle <- reactive({
    angle <- input$angle
    return(angle)
  })
  
  emptyIntersects <- reactive({
    if(isTRUE(input$empty)){choice <- "on"
    return(choice)
    }
    else{
      return(NULL)
    }
  })
  
  # A plot of fixed size
  output$plot <- renderImage({
    
    if(length(My_data()) == 0){stop()}
    if(length(Specific_sets()) == 1){
      stop()
    }
    width  <- session$clientData$output_plot_width
    height <- ((session$clientData$output_plot_height)*1.7)
    pixelratio <- session$clientData$pixelratio
    # A temp file to save the output. It will be deleted after renderImage
    # sends it, because deleteFile=TRUE.
    outfile <- tempfile(fileext='.png')
    # Generate a png
    png(outfile, width=width*pixelratio, height=height*pixelratio,
        res=72*pixelratio)
    upset(data = My_data(), 
          nintersects = input$nintersections,
          point.size = input$pointsize,
          sets = Specific_sets(),
          keep.order = setOrder(),
          order.by = orderdat(),
          decreasing = c(decrease()),
          number.angles = number_angle(),
          mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())),
          empty.intersections = emptyIntersects(),
          text.scale = c(input$intersection_title_scale, input$intersection_ticks_scale,
                         input$set_title_scale, input$set_ticks_scale, input$names_scale,
                         input$intersection_size_numbers_scale))
    dev.off()
    
    # Return a list
    list(src = outfile,
         width = width,
         height = height)
  }, deleteFile = TRUE)
  
  
  outputOptions(output, "plot", suspendWhenHidden = FALSE)
  
  observe({
    if(pushed$B != 0 && length(pushed$B) == 1){
      updateTabsetPanel(session, "main_panel", "upset_plot")
    }
  })
  
  output$down <- downloadHandler(
    
    filename = function(){
      paste("UpSetR", input$filetype, sep =".")
    }, 
    content = function(file){
      width  <- session$clientData$output_plot_width
      height <- ((session$clientData$output_plot_height)*2)
      pixelratio <- session$clientData$pixelratio
      if(input$filetype == "png")
        png(file, width=width*pixelratio, height=height*pixelratio,
            res=72*pixelratio)
      else if(input$filetype == "svg")
        devSVGTips(file, width=width/50, height=height/50)
      else
        pdf(file,width = 22, height = 14)
      upset(data = My_data(), 
            nintersects = input$nintersections,
            point.size = input$pointsize,
            sets = Specific_sets(),
            keep.order = setOrder(),
            order.by = orderdat(),
            decreasing = c(decrease()),
            number.angles = number_angle(),
            mb.ratio = c(as.double(bar_prop()), as.double(mat_prop())),
            empty.intersections = emptyIntersects(),
            text.scale = c(input$intersection_title_scale, input$intersection_ticks_scale,
                           input$set_title_scale, input$set_ticks_scale, input$names_scale,
                           input$intersection_size_numbers_scale))
      
      dev.off()
    }
  )
  
  
  })

# Run the application 
shinyApp(ui = ui, server = server)

