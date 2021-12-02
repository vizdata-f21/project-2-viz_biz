library(shiny)
library(tidyverse)
library(cowplot)
library(magick)
library(extrafont)
library(bslib)
library(shinyvalidate)
library(RCurl)
library(shinyjs, exclude = 'colourInput')
library(colourpicker)

font_import(paths = "/home/guest/R/project-2-viz_biz/shiny/eli-shiny/Futura.tff")



# Define UI for application that draws a histogram
ui <- fluidPage(
    theme = bs_theme(
        bootswatch = "cosmo",
        bg = "#fff",
        fg = "#000",
        primary = "#4060ff",
        base_font = font_google("Zen Kaku Gothic Antique"),
        code_font = font_google("Zen Kaku Gothic Antique")
    ),
    navbarPage(
        title = "Viz Biz",
        tabPanel(
            title = "Barbara Kruger",
            sidebarLayout(
                sidebarPanel(
                    h4("Graphics Input"),
                    div(
                        id = "form",
                    textInput("path", "Image address:", "https://www.thebroad.org/sites/default/files/art/greenfieldsanders_kruger.jpeg"),
                    textInput("top", "Top text:", "Your body"),
                    textInput("middle", "Middle text:", "is a"),
                    textInput("bottom", "Bottom text:", "battleground"),
                    sliderInput(
                        inputId = "text_size",
                        label = "Text size",
                        min = 1, max = 20, value = 10, ticks = FALSE
                    ),
                    sliderInput(
                        inputId = "border_size",
                        label = "Border size",
                        min = 0, max = 20, value = 5, ticks = FALSE
                    ),
                    sliderInput(
                        inputId = "img_brightness",
                        label = "Brightness",
                        min = 0, max = 500, value = 100, step = 10, round = TRUE, ticks = FALSE
                    ),
                    sliderInput(
                        inputId = "img_saturation",
                        label = "Saturation",
                        min = 0, max = 200, value = 100, step = 10, round = TRUE, ticks = FALSE
                    ),
                    sliderInput(
                        inputId = "img_hue",
                        label = "Hue",
                        min = 0, max = 200, value = 100, step = 10, round = TRUE, ticks = FALSE
                    ),
                    p("Text Color"),
                    fluidRow(
                        column(width = 6, colourInput(
                            inputId = "text_color",
                            label = NULL, value = "#FCFCFC"
                        ))
                    ),
                    p("Border Color"),
                    fluidRow(
                        column(width = 6, colourInput(
                            inputId = "rect_color",
                            label = NULL, value = "#FF0000"
                        ))
                    )),
                    actionButton("resetAll", "Reset"),
                    hr(),
                    textInput("custom_filename", "Filename", "kruger_example.png"),
                    verbatimTextOutput("value"),
                    sliderInput(
                        inputId = "res",
                        label = "Resolution (in dpi)",
                        min = 100, max = 2000, value = 600, step = 100, round = TRUE, ticks = FALSE
                    ),
                    div(
                        align = "right",
                        downloadLink("save", strong("Download"))
                    )

                ), #sidebar panel

                mainPanel(
                    h2(strong("Barbara Kruger")),
                    h5(em("Untitled (Your body is a battleground)")),
                    p(""),
                    p("Inspired by Kruger's original artwork, we invite you to recreate your
            own modified image and adjust the
            settings in the", em("Graphics Input"), "sidebar on the left
            according to your preference."),

                    # plotOutput(outputId = "plot", inline = TRUE),
                    div(plotOutput(
                        outputId = "plot", inline = TRUE,
                        height = "100%"
                    ), align = "center"),
                    h4(" ")
                ) # main panel
            ) # sidebar 3 layout
        ) # tab 1 panel
    ) # navbar page
) # fluid page

# Define server logic required to draw a histogram
server <- function(input, output) {

    observeEvent(input$resetAll, {
        reset("form")
    })



    link <- reactive(input$path)


    linker <- reactive({
    if(url.exists(link()) == TRUE){
        link()
    }
        else{
            "https://www.thebroad.org/sites/default/files/art/greenfieldsanders_kruger.jpeg"
        }
    })





    # Validation rules are set in the server, start by
    # making a new instance of an `InputValidator()`
    iv <- InputValidator$new()

    # Basic usage: `sv_url()` works well with its
    # defaults; a message will be displayed if the
    # validation of `input$address` fails
    iv$add_rule("path", sv_url(message = "Not a valid URL", allow_multiple = FALSE, allow_na = FALSE))

    # Finally, `enable()` the validation rules
    iv$enable()


    magick_image <- reactive({image_read(linker())})


    magick_plot <- reactive({
        if(image_info(magick_image())$height>=image_info(magick_image())$width) {
        image_scale(magick_image(), "x700")
        }
        else{
        image_scale(magick_image(), "700")}
    })

    img_height <- reactive({image_info(magick_plot())$height})
    img_width <- reactive({image_info(magick_plot())$width})


    size <- reactive({
        if(img_height()>img_width()) {
        img_width()/500
    } else {
        img_height()/500
    }
    })

    top_plot <- reactive({ifelse(nchar(input$top) == 0, NA, input$top)})
    middle_plot <- reactive({ifelse(nchar(input$middle) == 0, NA, input$middle)})
    bottom_plot <- reactive({ifelse(nchar(input$bottom) == 0, NA, input$bottom)})

    x_plot <- reactive({c(img_width()/2, img_width()/2, img_width()/2)})
    y_plot <- reactive({c(img_height()-30*size(), img_height()/2, 30*size())})

    df <- reactive({
        data.frame(x_plot(), y_plot())
    })

    observe({output$plot <- renderPlot({
        plot_kruger <- ggplot() +
            geom_rect(aes(xmin = 0, xmax = img_width(),
                          ymin = 0, ymax = img_height()),
                      color = input$rect_color,
                      size = input$border_size,
                      fill = NA) +
            draw_image(image_modulate(magick_plot(),
                                      brightness = input$img_brightness,
                                      saturation = input$img_saturation,
                                      hue = input$img_hue),
                       x = 0, y=0, width = img_width(),  height = img_height()) +

            geom_label(data = df(),
                       mapping = aes(x = x_plot(),
                                     y = y_plot()
                       ),
                       label = c(top_plot(), middle_plot(), bottom_plot()),
                       size = input$text_size,
                       fill = input$rect_color,
                       color = input$text_color,
                       family = "Times New Roman",
                       fontface = "bold",
                       label.size = 0,
                       label.r = unit(0, "lines")) +

            coord_equal() +
            theme_void()

        plot_kruger}, height = img_height(), width = img_width())})

}

# Run the application
shinyApp(ui = ui, server = server)