
library(shiny)
library(tidyverse)
library(cowplot)
library(magick)
library(extrafont)
library(bslib)
library(colourpicker)
library(base64enc)

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
          fileInput("myFile", "Upload PNG/JPG/JEPG image:",
                    accept = c("image/png", "image/jpg", "image/jpeg")),
          uiOutput("image"),
          # textInput("path", "Image address:", "https://www.thebroad.org/sites/default/files/art/greenfieldsanders_kruger.jpeg"),
          hr(),
          textInput("top", "Top Text:", "Your body"),
          textInput("middle", "Center Text:", "is a"),
          textInput("bottom", "Bottom Text:", "battleground"),
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
          p("Text Color & Border Color"),
          fluidRow(
            column(width = 6, colourInput(
              inputId = "text_color",
              label = NULL, value = "#FCFCFC"
            )),
            column(width = 6, colourInput(
              inputId = "rect_color",
              label = NULL, value = "#FF0000"
            ))
          ),
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
            downloadLink("kruger_save", strong("Download"))
          )
        ), # sidebar panel

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

  # my_image <- image_read(input$path)
  # my_image <- image_read("https://www.thebroad.org/sites/default/files/art/greenfieldsanders_kruger.jpeg")
  # my_image <- image_read(reactive({input$myFile}))

  base64 <- reactive({
    inFile <- input$myFile
    if (!is.null(inFile)) {
      dataURI(file = inFile$datapath, mime = "image/png")
    }
  })

  output$image <- renderUI({
    if (!is.null(base64())) {
      tags$div(
        tags$img(src = base64(), width = "40%"),
        style = "width: 400px;"
      )
    }
  })

  observeEvent(input$myFile, {
    inFile <- input$myFile
    if (is.null(inFile)) {
      return()
    }
    file.copy(inFile$datapath, file.path("c:/temp", inFile$name))
  })

  my_image <- reactive({
      base64()
      })

  # if (image_info(my_image)$height>700 | image_info(my_image)$width>700) {
  if (image_info(my_image())$height >= image_info(my_image())$width) {
    my_image_scaled <- reactive({image_scale(my_image(), "x700")})
  } else {
    my_image_scaled <- reactive({image_scale(my_image(), "700")})
  }
  # }

  img_height <- reactive({image_info(my_image_scaled())$height})
  img_width <- reactive({image_info(my_image_scaled())$width})


  plot_size <- reactive({
      tribble(
    ~x, ~y,
    0, 0,
    img_width(), img_height()
  )
  })


  if (img_height() > img_width()) {
    size <- reactive({img_width() / 500})
  } else {
    size <- reactive({img_height() / 500})
  }


  label_maker <- reactive({
      tribble(
    ~x, ~y,
    img_width() / 2, img_height() - 30 * size(),
    img_width() / 2, img_height() / 2,
    img_width() / 2, 30 * size()
  )
  })


  output$plot <- renderPlot(
    {
      kruger_plot <- ggplot() +
        geom_point(data = plot_size(), aes(x = x, y = y), alpha = 0) +
        geom_rect(aes(
          xmin = 0, xmax = img_width(),
          ymin = 0, ymax = img_height()
        ),
        color = input$rect_color,
        size = input$border_size,
        fill = NA
        ) +
        draw_image(image_modulate(my_image_scaled(),
          brightness = input$img_brightness,
          saturation = input$img_saturation,
          hue = input$img_hue
        ),
        x = 0, y = 0, width = img_width(), height = img_height()
        ) +
        geom_label(
          data = label_maker(),
          mapping = aes(
            x = x,
            y = y
          ),
          label = c(input$top, input$middle, input$bottom),
          size = input$text_size,
          fill = input$rect_color,
          color = input$text_color,
          family = "Times New Roman",
          fontface = "bold",
          label.size = 0,
          label.r = unit(0, "lines")
        ) +
        coord_equal() +
        theme_void()

      kruger_plot
    },
    height = img_height(),
    width = img_width()
  )
}

# Run the application
shinyApp(ui = ui, server = server)