library(shiny)
library(tidyverse)
library(cowplot)
library(magick)
library(extrafont)
library(bslib)
library(colourpicker)
library(base64enc)
library(Cairo)
options(shiny.usecairo = TRUE)

ui <- fluidPage(

    titlePanel("Old Faithful Geyser Data"),

    fileInput("myFile", "Upload PNG/JPG/JEPG image:",
              accept = c("image/png", "image/jpg", "image/jpeg", multiple = FALSE)),

    tableOutput("files"),

    uiOutput("image"),

    # imageOutput(outputId = "original_artwork", inline = TRUE),

    plotOutput(outputId = "plot", inline = TRUE, height = "100%")

)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$files <- renderTable(input$myFile)

    base64 <- reactive({
        inFile <- input$myFile
        if (!is.null(inFile)) {
            dataURI(file = inFile$datapath, mime = "image/png")
        }
    })

    # output$image <- renderUI({
    #     if (!is.null(base64())) {
    #         tags$div(
    #             tags$img(src = base64(), width = "40%"),
    #             style = "width: 400px;"
    #         )
    #     }
    # })

    output$plot <- renderPlot({
        ggplot() + draw_image(base64())
    })
}

# Run the application
shinyApp(ui = ui, server = server)
