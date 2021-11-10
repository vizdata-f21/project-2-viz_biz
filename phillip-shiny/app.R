# http://shiny.rstudio.com/
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(shinyalert)
library(shinyWidgets)
library(ggiraph)
library(statebins)
library(tidyverse)
library(gganimate)
library(colorspace)
library(patchwork)
library(usmap)
library(lubridate)
library(ggrepel)
library(scales)
library(dsbox)
library(viridis)
library(ggridges)
library(magrittr)
library(forcats)
library(tibble)
library(janitor)
library(grid)
library(gridExtra)
library(styler)
library(emojifont)
library(RColorBrewer)
library(colorRamps)
library(glue)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(ggwordcloud)
library(ggtext)
library(glue)
library(tidytext)
library(stringr)
library(stopwords)
library(tm)
library(wordcloud)
library(wordcloud2)
library(RColorBrewer)
library(ggwordcloud)
library(bslib)
library(tidyverse)
library(Rcpp)
library(reshape2)
library(colourlovers)
library(cowplot)
library(generativeart) # https://github.com/cutterkom/generativeart
library(ggpolypath)
library(Cairo)
options(shiny.usecairo = TRUE)

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
      title = "Frank Stella",
      sidebarLayout(
        sidebarPanel(
          h4("Graphics Input"),
          sliderInput(
            inputId = "size",
            label = "Size (Number of layers):",
            min = 10, max = 30, value = 15, ticks = FALSE
          ),
          selectInput(
            inputId = "color1",
            label = h5("Primary Color:"),
            choices = list(
              Blues = "Blues",
              Greens = "Greens",
              Oranges = "Oranges",
              Reds = "Reds"
            ),
            selected = "Blues", multiple = FALSE
          ),
          prettyCheckbox(
            inputId = "color1_rev",
            label = "Reverse sequence",
            value = FALSE
          ),
          selectInput(
            inputId = "color2",
            label = h5("Secondary Color:"),
            choices = list(
              Greys = "Greys",
              "Pure White" = "Pure White",
              "Pure Black" = "Pure Black"
            ),
            selected = "Blues", multiple = FALSE
          ),
          prettyCheckbox(
            inputId = "color2_rev",
            label = "Reverse sequence",
            value = FALSE
          ),
          switchInput(
            inputId = "borderline",
            label = "Borderlines",
            value = FALSE
          ),
          hr(),
          textInput("custom_filename", "Filename", "frank_stella.png"),
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

          # div(align = "right",
          # actionButton(inputId = "getdata",
          # label = strong("Generate Output")))
        ), # sidebar panel

        mainPanel(
          h2(strong("Frank Stella: Experiment and Change")),
          h5(em("Lettre Sur Les Et Muets II (1974)")),

          # plotOutput(outputId = "plot", inline = TRUE),
          div(plotOutput(
            outputId = "plot", inline = TRUE,
            height = "100%"
          ), align = "center"),

          h4(" "),

          prettyCheckbox(
            inputId = "table_switch",
            label = "Raw dataframe used to recreate the masterpiece above",
            value = FALSE
          ),
          DT::dataTableOutput(outputId = "table_switch"),

          prettyCheckbox(
            inputId = "original_artwork",
            label = "Original Artwork",
            value = FALSE
          ),
          div(imageOutput(outputId = "original_artwork", inline = TRUE), align = "center"),

          h4(" "),

          div(textOutput(outputId = "original_artwork_text", inline = TRUE), align = "center"),

          h4(" ")

        ) # main panel
      ) # sidebar 3 layout
    ) # tab 1 panel
  ) # navbar page
) # fluid page

# Define server logic required to draw a histogram
server <- function(input, output) {
  n <- reactive({
    input$size * 2
  })

  dat <- reactive({

    # OUTER

    x_lower_left <- c(1:(n() / 2)) # 1, 2, 3, 4
    y_lower_left <- c(1:(n() / 2)) # 1, 2, 3, 4
    x_upper_left <- c(1:(n() / 2)) # 1, 2, 3, 4
    y_upper_left <- c(n():(n() / 2 + 1)) # 8, 7, 6, 5
    x_lower_right <- c(n():(n() / 2 + 1)) # 8, 7, 6, 5
    y_lower_right <- c(1:(n() / 2)) # 1, 2, 3, 4
    x_upper_right <- c(n():(n() / 2 + 1)) # 8, 7, 6, 5
    y_upper_right <- c(n():(n() / 2 + 1)) # 8, 7, 6, 5

    x_outer <- c(x_lower_left, x_upper_left, x_lower_right, x_upper_right)
    y_outer <- c(y_lower_left, y_upper_left, y_lower_right, y_upper_right)
    id_outer <- rep(c(1:(n() / 2)), times = 4)

    df_outer <- data.frame(x_outer, y_outer, id_outer) %>%
      rename(x = x_outer, y = y_outer, id = id_outer)

    # INNER

    x_lower_left <- c(2:(n() / 2), (n() + 1) / 2)
    y_lower_left <- c(2:(n() / 2), (n() + 1) / 2)
    x_upper_left <- c(2:(n() / 2), (n() + 1) / 2)
    y_upper_left <- c((n() - 1):(n() / 2 + 1), (n() + 1) / 2)
    x_lower_right <- c((n() - 1):(n() / 2 + 1), (n() + 1) / 2)
    y_lower_right <- c(2:(n() / 2), (n() + 1) / 2)
    x_upper_right <- c((n() - 1):(n() / 2 + 1), (n() + 1) / 2)
    y_upper_right <- c((n() - 1):(n() / 2 + 1), (n() + 1) / 2)

    x_inner <- c(x_lower_left, x_upper_left, x_lower_right, x_upper_right)
    y_inner <- c(y_lower_left, y_upper_left, y_lower_right, y_upper_right)
    id_inner <- rep(c(1:(n() / 2)), times = 4)

    df_inner <- data.frame(x_inner, y_inner, id_inner) %>%
      rename(x = x_inner, y = y_inner, id = id_inner)

    # FINAL

    df <- rbind(df_outer, df_inner) %>%
      arrange(id) %>%
      mutate(subid = rep(rep(c(1L, 2L), each = 4), n() / 2)) %>%
      mutate(order = rep(c(1, 2, 4, 3), times = n())) %>%
      arrange(id, subid, x, order)

    df_extra <- df[seq(1, 4 * (n() / 2) + 1, by = 4), ] %>%
      mutate(order = 5)

    df_final <- rbind(df, df_extra) %>%
      arrange(id, subid, order)

    rownames(df_final) <- NULL

    df_final
  })

  borderline <- reactive({
    if (input$borderline == FALSE) {
      return(0)
    }
    if (input$borderline == TRUE & n() < 17) {
      return(0.36)
    }
    if (input$borderline == TRUE & n() < 24) {
      return(0.24)
    }
    if (input$borderline == TRUE & n() > 24) {
      return(0.12)
    }
  })

  # eventReactive()
  # observeEvent()

  # palette1 = colorRampPalette(brewer.pal(8, input$color1))(size/2) %>% rev()
  # palette2 = colorRampPalette(brewer.pal(8, input$color2))(size/2)
  # palette3 = rep(c("#FFFFFF"), times = size/2)
  # palette <- c(rbind(palette1, palette2))

  output$table_switch <- DT::renderDataTable({
    if (input$table_switch == TRUE) {
      return(dat())
    }
  })

  plotInput <- reactive({
    ggplot(dat(), aes(x = x, y = y, group = id, subgroup = subid)) +
      geom_polygon(aes(fill = factor(id)), color = "white", size = borderline()) +
      # scale_fill_manual(values = palette) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$plot <- renderPlot(
    {
      plotInput()
    },
    height = 400,
    width = 400
  )

  custom_filename <- reactive({
    input$custom_filename
  })

  custom_res <- reactive({input$res})

  output$save <- downloadHandler(
    filename = function() {
      custom_filename()
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png", dpi = as.double(custom_res()))
    }
  )

  output$original_artwork <- renderImage({
    if(input$original_artwork == TRUE) {
      return(list(
        src = "./stella.jpg",
        width = 450,
        height = 325,
        contentType = "image/jpg",
        alt = "Original Artwork"))
    }
    if(input$original_artwork == FALSE) {
      return(list(
      src = "./stella.jpg",
      width = 0,
      height = 0,
      contentType = "image/jpg",
      alt = "Original Artwork"))}
  })

  output$original_artwork_text <- renderText({
    if(input$original_artwork == TRUE) {
    return(paste("Photo by Christopher Burke, © 2017 Frank Stella / Artists Rights Society (ARS), New York"))
  }
    if(input$original_artwork == FALSE) {
      return(NULL)}
    })
}

# Run the application
shinyApp(ui = ui, server = server)
