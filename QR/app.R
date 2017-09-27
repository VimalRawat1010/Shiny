library("shiny")
#library("shinyURL")
library("qrcode")

shinyApp(
  ui = fluidPage(
    titlePanel("Please scan for Coffee Entry!"),
    sidebarLayout(
      sidebarPanel(width=7,
        HTML("&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;"),
        plotOutput("qrcode")
      ),
      mainPanel(
              )
    )
  ),
  server = function(input, output) {
    #url = shinyURL.server()
    qrcode = reactive( t(qrcode_gen("http://130.60.200.66:3838/coffeeclub/", plotQRcode = FALSE, dataOutput = TRUE)) )
    nc = reactive( ncol(qrcode()) )
    nr = reactive( nrow(qrcode()) )
    scale = 18
    
    output$qrcode <- renderPlot({
      par(mar=c(0,0,0,0))
      image(1L:nc(), 1L:nr(), qrcode(), xlim = 0.5 + c(0, nc()),
            ylim = 0.5 + c(nr(), 0), axes = FALSE, xlab = "", ylab = "",
            col = c("white", "black"), asp = 1)
    }, width = function() scale*nc(), height = function() scale*nr())
  }
)  