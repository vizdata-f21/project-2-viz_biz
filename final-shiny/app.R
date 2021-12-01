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
#library(generativeart) # https://github.com/cutterkom/generativeart
library(ggpolypath)
library(colourpicker)
library(Cairo)
options(shiny.usecairo = TRUE)
library(base64enc)

# Static Datasets

pietmondrianvertical <- tribble(
    ~xmin, ~xmax, ~ymin, ~ymax,
    0.1, 0.4, 0, 12,
    0.8, 1.1, 0, 12,
    2, 2.3, 0, 12,
    9.4, 9.7, 0, 12,
    8.7, 9, 0, 12
)

piet_geom_rect <- tribble(
    ~xmin, ~xmax, ~ymin, ~ymax, ~fill,
    0.3, 0.8, 0, 1.9, "yellow",
    1.1, 2, 4.0, 6.0, "red",
    0, 0.1, 4.0, 6.0, "yellow",
    0, 0.8, 10, 12, "yellow",
    2.05, 3, 0, 0.7, "blue",
    5, 5.8, 0, 0.7, "red",
    6.8, 7.5, 0, 0.7, "black",
    9, 9.7, 0, 0.7, "yellow",
    9, 9.7, 1.3, 2, "black",
    9, 9.7, 2.8, 3.4, "red",
    9, 9.7, 4.8, 5.4, "black",
    9.7, 10, 4, 6.0, "blue"
)

piet_segment <- tribble(
    ~x, ~y, ~xend, ~yend,
    0.3, 3, 2.2, 3,
    0.3, 2, 0.805, 2,
    0.3, 1.3, 0.805, 1.3,
    0.805, 1.7, 2.05, 1.7,
    2, 0.7, 9.7, 0.7
)

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
          p(""),
          p("Primary Gradient"),
          fluidRow(
            column(width = 6, colourInput(
              inputId = "color_primary_start",
              label = NULL, value = "#DBDFFF"
            )),
            column(width = 6, colourInput(
              inputId = "color_primary_end",
              label = NULL, value = "#0C0C3B"
            ))
          ),
          p(""),
          p("Secondary Gradient"),
          fluidRow(
            column(width = 6, colourInput(
              inputId = "color_secondary_start",
              label = NULL, value = "#EDEDF0"
            )),
            column(width = 6, colourInput(
              inputId = "color_secondary_end",
              label = NULL, value = "#525252"
            ))
          ),
          p(""),
          prettyCheckbox(
            inputId = "borderline",
            label = "Activate thin borderlines",
            value = FALSE,
            inline = TRUE
          ),
          hr(),
          textInput("custom_filename", "Filename", "frank_stella.png"),
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
          p(""),
          p("Inspired by the original artwork, we invite you to recreate your
            own modified piece of Frank Stella's masterpiece and adjust the
            settings in the", em("Graphics Input"), "sidebar on the left
            according to your preference."),
          p("Our recommendation is to have more layers and higher resolution
            if you are intending to print the plot for a huge decoration. Otherwise,
            hacing a smaller number of layers and lower resolution would be more appropriate.
            Feel free to toggle as much as you want and experiment varieties of ways on
            modifying this plot to figure out your desired unique style!"),

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
      ) # sidebar layout
    ), # tab 1 panel
    tabPanel(
      title = "Piet Mondrian",
      sidebarLayout(
        sidebarPanel(
          h4("Graphics Input"),
          div(align = "right"),
          sliderInput("piet_lines",
            "Number of Full Horizontal Lines:",
            min = 0, max = 5, value = 4, ticks = FALSE
          ),
          # sliderInput("sizehorizontal",
          #             "Thickness of Horizontal Lines:",
          #             min = 0, max = 10, value = 4.5, step = 0.25, ticks = FALSE),

          colourInput(inputId = "grid_color_piet", label = "Grid Color", value = "#3D3D3D"),

          h6("Box Colors"),
          fluidRow(
            column(width = 6, colourInput(inputId = "color_piet_1", label = NULL, value = "#3D3D3D")),
            column(width = 6, colourInput(inputId = "color_piet_2", label = NULL, value = "#2A4FE0"))
          ),

          fluidRow(
            column(width = 6, colourInput(inputId = "color_piet_3", label = NULL, value = "#CC0000")),
            column(width = 6, colourInput(inputId = "color_piet_4", label = NULL, value = "#F5D800"))
          ),

          hr(),
          textInput("custom_filename_piet", "Filename", "piet_mondrian.png"),
          sliderInput(
            inputId = "res_piet",
            label = "Resolution (in dpi)",
            min = 100, max = 2000, value = 600, step = 100, round = TRUE, ticks = FALSE
          ),
          div(
            align = "right",
            downloadLink("save_piet", strong("Download"))
          )
        ),
        mainPanel(
          h2(strong("Piet Mondrian: Finding Refuge")),
          h5(em("Trafalgar Square (1939-1943)")),
          p(""),
          p("Inspired by the original artwork, we invite you to recreate your
            own modified piece of Piet Mondrian's masterpiece and adjust the
            settings in the", em("Graphics Input"), "sidebar on the left
            according to your preference."),
          p("Feel free to toggle between different amount of horizontal lines
            in the artwork and to adjust the color of the boxes pictured. Experiment
            with your own varieties of abstract ways to
            modify this plot and figure out your desired unique style!"),
          div(plotOutput(
            outputId = "plot_piet", inline = TRUE,
            height = "100%"
          ), align = "center"),
          prettyCheckbox(
            inputId = "original_artwork_piet",
            label = "Original Artwork",
            value = FALSE
          ),
          div(imageOutput(outputId = "original_artwork_piet", inline = TRUE), align = "center"),
          h4(" "),
          div(textOutput(outputId = "original_artwork_text_piet", inline = TRUE), align = "center"),
          h4(" ")
        )
      )
    ) # tab 2 panel
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

  # COLOR

  size <- reactive({
    input$size
  })

  color_primary_start <- reactive({
    input$color_primary_start
  })
  color_primary_end <- reactive({
    input$color_primary_end
  })
  color_primary <- reactive({
    colorRampPalette(c(color_primary_start(), color_primary_end()))(ceiling(size() / 2))
  })

  color_secondary_start <- reactive({
    input$color_secondary_start
  })
  color_secondary_end <- reactive({
    input$color_secondary_end
  })
  color_secondary <- reactive({
    colorRampPalette(c(color_secondary_start(), color_secondary_end()))(floor(size() / 2))
  })

  full <- reactive(c(rbind(color_primary(), color_secondary())))

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
    ggplot(dat(), aes(x = x, y = y, group = factor(id), subgroup = subid)) +
      geom_polygon(aes(fill = factor(id)), colour = "white", size = borderline()) +
      scale_fill_manual(values = full()) +
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

  custom_res <- reactive({
    input$res
  })

  output$save <- downloadHandler(
    filename = function() {
      custom_filename()
    },
    content = function(file) {
      ggsave(file, plot = plotInput(), device = "png", dpi = as.double(custom_res()))
    }
  )

  output$original_artwork <- renderImage({
    if (input$original_artwork == TRUE) {
      return(list(
        src = "./stella.jpg",
        width = 450,
        height = 325,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
    if (input$original_artwork == FALSE) {
      return(list(
        src = "./stella.jpg",
        width = 0,
        height = 0,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
  })

  output$original_artwork_text <- renderText({
    if (input$original_artwork == TRUE) {
      return(paste("Photo by Christopher Burke, © 2017 Artists Rights Society (ARS), New York"))
    }
    if (input$original_artwork == FALSE) {
      return(NULL)
    }
  })

  # PIET MONDRIAN

  plotInput_piet <- reactive({
    ggplot() +
        geom_rect(data = piet_geom_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
        scale_fill_manual(values = c(input$color_piet_1,input$color_piet_2,
                                     input$color_piet_3, input$color_piet_4)) +
        geom_rect(data = pietmondrianvertical, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = input$grid_color_piet, size = 3) +
        geom_hline(yintercept = seq(4, 10, length.out = input$piet_lines), size = 4.5, #input$sizehorizontal,
                   color = input$grid_color_piet) +
        geom_segment(data = piet_segment, aes(x = x, y = y, xend = xend, yend = yend), color = input$grid_color_piet, size = 3.5) +
        scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2)) +
        scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
        coord_fixed(xlim = c(0, 10), ylim = c(0, 12)) +
        theme_void() +
        theme(legend.position = "none")
    })

  output$plot_piet <- renderPlot(
    {
      plotInput_piet()
    },
    height = 600,
    width = 500
  )

  custom_filename_piet <- reactive({
    input$custom_filename_piet
  })

  custom_res_piet <- reactive({
    input$res_piet
  })

  output$save_piet <- downloadHandler(
    filename = function() {
      custom_filename_piet()
    },
    content = function(file) {
      ggsave(file, plot = plotInput_piet(), device = "png", dpi = as.double(custom_res_piet()))
    }
  )

  output$original_artwork_piet <- renderImage({
    if (input$original_artwork_piet == TRUE) {
      return(list(
        src = "./piet.jpg",
        width = 250,
        height = 325,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
    if (input$original_artwork_piet == FALSE) {
      return(list(
        src = "./piet.jpg",
        width = 0,
        height = 0,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
  })

  output$original_artwork_text_piet <- renderText({
    if (input$original_artwork_piet == TRUE) {
      return(paste("Gift of Mr. and Mrs. William A. M. Burden, © 2010 The Museum of Modern Art (MoMA), New York"))
    }
    if (input$original_artwork_piet == FALSE) {
      return(NULL)
    }
  })

}

# Run the application
shinyApp(ui = ui, server = server)
