# http://shiny.rstudio.com/
library(shiny) # Eli
library(shinythemes)
library(tidyverse) # Eli
library(DT)
library(shinyalert)
library(shinyWidgets)
library(ggiraph)
library(magrittr)
library(tibble)
library(styler)
library(colorRamps)
library(RColorBrewer)
library(tidytext)
library(tm)
library(bslib) # Eli
library(Rcpp)
library(reshape2)
library(colourlovers)
library(cowplot) # Eli
library(ggpolypath)
library(colourpicker) # Eli
library(ggnewscale) # Lilly
library(Cairo)
library(ggforce) # Lilly
options(shiny.usecairo = TRUE)
library(base64enc)
library(magick) # Eli
library(shinyvalidate) # Eli
library(RCurl) # Eli
library(showtext) # Eli


## Kandinsky Prep
# load data
circles_l <- readRDS("data/circles.rds")
lines_l <- readRDS("data/lines.rds")
quads_l <- readRDS("data/quads.rds")
semicircle_fill_l <- readRDS("data/semicircle-fill.rds")
semicircle_stroke_l <- readRDS("data/semicircle-stroke.rds")
semicircle_stroke_color_l <- readRDS("data/semicircle-stroke-color.rds")
triangles_l <- readRDS("data/triangles.rds")

# load font
font_add(
  family = "Futura",
  regular = paste0(here::here(), "/shiny/final-shiny/data/Futura.ttf")
)
showtext_auto()


# write functions
clip <- function(x, low, high) {
  x[x < low] <- low
  x[x > high] <- high
  return(x)
}

add_noise <- function(df, layers = c(1), magnitude = 5) {
  for (i in layers) {
    df[[1]][[i]] <- as.data.frame(df[[1]][[i]]) %>%
      group_by(grouping) %>%
      mutate(
        noise_x = rnorm(1, 0, magnitude),
        noise_y = rnorm(1, 0, magnitude)
      ) %>%
      ungroup() %>%
      mutate(
        across(contains("x"), ~ clip(.x + noise_x, xmin, xmax)),
        across(contains("y"), ~ clip(.x + noise_y, ymin, ymax))
      )
  }
  return(df)
}

xmin <- 0
xmax <- 152
ymin <- 0
ymax <- 86

print_circles <- FALSE
print_quads <- FALSE
print_lines_curved <- FALSE
print_lines_straight <- FALSE
print_triangles <- FALSE


# Define UI for application
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
      title = "About Us",
      sidebarLayout(
        sidebarPanel(
          h5("Who Are We?"),
          p("", style = "margin-bottom: 24px;"),
          p("We are the ", em("Viz Biz"), " team:", style = "margin-bottom: 6px;"),
          p("1. Phillip Harmadi (ph119@duke.edu)",
            style = "margin-bottom: 3px;"
          ),
          p("2. Sana Pashankar (sdp47@duke.edu)",
            style = "margin-bottom: 3px;"
          ),
          p("3. Lillian Clark (lmc97@duke.edu)",
            style = "margin-bottom: 3px;"
          ),
          p("4. Eli Feder (esf10@duke.edu)",
            style = "margin-bottom: 3px;"
          ),
          p("", style = "margin-bottom: 24px;"),
          p(
            "This dahsboard website is the main portion of our final project for Duke University's ",
            em("STA 313 (Advanced Data Visualization)"), "course during the Fall 2021 semester."
          ),
          p("We hope that our project will increase your appreciation and awareness
            of using digital techniques to recreate modern and abstract artworks.")
        ),
        mainPanel(
          h2("Democratizing Modern Art"),
          p("", style = "margin-bottom: 24px;"),
          h5("💡 Customizable Artworks using the Shiny App"),
          p("", style = "margin-bottom: 12px;"),
          p("Our goal is to promote digital extensions of modern, abstract paintings
          and encourage users to play with and modify pieces with significance and
          status in the art world. Wealthy collectors, curators, and gallery owners
          have historically had a massive amount of power in determining what is
          considered “good art” and what sells."),
          p("We see our project as part of the
          move to question empty status designations and push back against institutional
          art gatekeeping. Hence, this Shiny App allow users to access several examples
          of recreated art pieces, adjust their aesthetics based on their personal
          preferences, and download them for their personal use with no conditions or payment."),
          p("", style = "margin-bottom: 24px;"),
          h5("🖌 Our chosen artworks for you to explore"),
          p("", style = "margin-bottom: 12px;"),
          p("1. Frank Stella, ", em("Lettre sur les sourds et muets II"), " (1974)",
            style = "margin-bottom: 3px;"
          ),
          p("2. Piet Mondrian, ", em("Trafalgar Square"), " (1939-1943)",
            style = "margin-bottom: 3px;"
          ),
          p("3. Wassily Kandinsky, ", em("Composition 8"), " (1923)",
            style = "margin-bottom: 3px;"
          ),
          p("4. Barbara Kruger, ", em("Untitled (Your body is a battleground)"), " (1989)",
            style = "margin-bottom: 3px;"
          ),
          p("", style = "margin-bottom: 24px;"),
          h5("❓ How to navigate the artworks?"),
          p("", style = "margin-bottom: 12px;"),
          p("We invite you to recreate your
            own modified piece of famous artworks and adjust their
            settings by using the", em("Graphics Input"), "sidebar on the left bar
            of each of our dashboard tabs according to your preference.")
        )
      )
    ),
    tabPanel(
      title = "1. Frank Stella",
      sidebarLayout(
        sidebarPanel(
          h4("Graphics Input"),
          sliderInput(
            inputId = "size",
            label = "Size (Number of layers):",
            min = 10, max = 30, value = 15, ticks = FALSE
          ),
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
          p("Stella alternates between bands of gray, lightening as they approach
            the center and a rich chromatic scale, which transitions from reds,
            to yellow, to greens and finally dark blue. The painting’s simultaneous
            formal cohesion and discordant color combinations create a precisely
            wrought tension within the painting, an effect Stella often strives
            for in his pictures."),
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
    ), # tab 2 panel
    tabPanel(
      title = "2. Piet Mondrian",
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
          colourInput(inputId = "background_color_piet", label = "Background Color", value = "#F5EFDF"),
          colourInput(inputId = "grid_color_piet", label = "Grid Color", value = "#000000"),
          h6("Box Colors"),
          fluidRow(
            column(width = 6, colourInput(inputId = "color_piet_1", label = NULL, value = "#000000")),
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
          p("In September 1938, Mondrian moved from Paris to London to escape the
            threat of a German invasion. There he made Trafalgar Square, the first
            in a series of paintings titled after locations in cities that gave him
            refuge during World War II. Mondrian revisited this painting after his
            flight to New York in 1940 to escape the escalating war. The small,
            subtly textured planes of primary colors vibrate within their black
            perimeters."),
          p("Feel free to toggle between different amount of horizontal lines
            in the artwork and to adjust the color of the boxes pictured. Experiment
            with your own varieties of abstract ways to modify and figure out your
            own custom style!"),
          div(plotOutput(
            outputId = "plot_piet", inline = TRUE,
            height = "100%"
          ), align = "center"),
          p(""),
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
    ),
    tabPanel(
      title = "3. Wassily Kandinsky",
      sidebarLayout(
        sidebarPanel(
          h4("Graphics Input"),
          div(align = "right"),
          sliderInput("circlesize",
            "Circle Size:",
            min = 1,
            max = 20,
            value = 10,
            ticks = FALSE
          ),
          sliderInput("linethickness",
            "Line Thickness:",
            min = 0.001,
            max = 3,
            value = c(0.3, 1.3),
            ticks = FALSE
          ),
          sliderInput("alpha",
            "Transparency:",
            min = 0,
            max = 1,
            value = c(0.95, 1),
            ticks = FALSE
          ),
          colourInput(
            inputId = "background_color",
            label = "Background Color",
            value = "#F1E8DC"
          ),
          checkboxGroupInput(
            inputId = "checkbox_layers",
            label = "Select layers to display:",
            choices = list(
              "Circles", "Quadrilaterals", "Triangles",
              "Straight Lines", "Curved Lines"
            ),
            selected = list(
              "Circles", "Quadrilaterals", "Triangles",
              "Straight Lines", "Curved Lines"
            )
          ),
          h4("Add random noise"),
          sliderInput("magnitudenoise",
            "Magnitude:",
            min = 0,
            max = 25,
            value = 0
          ),
          actionButton("go_kandinsky", "Apply Changes"),
          p(""),
          span(p("Please allow the artwork 10 to 15 seconds to render after applying changes"),
            style = "color:red"
          ),
          hr(),
          textInput("custom_filename_kandinsky", "Filename", "wassily_kandinsky.png"),
          sliderInput(
            inputId = "res_kandinsky",
            label = "Resolution (in dpi)",
            min = 100, max = 2000, value = 600, step = 100, round = TRUE, ticks = FALSE
          ),
          div(
            align = "right",
            downloadLink(outputId = "save_kandinsky", label = strong("Download"))
          )
        ),
        mainPanel(
          h2(strong("Wassily Kandinsky: Composing Oneself")),
          h5(em("Composition 8 (1923)")),
          p(""),
          prettyCheckbox(
            inputId = "original_artwork_kandinsky",
            label = "Show Original Artwork",
            value = FALSE
          ),
          div(imageOutput(outputId = "original_artwork_kandinsky", inline = TRUE), align = "center"),
          div(textOutput(outputId = "original_artwork_text_kandinsky", inline = TRUE), align = "center"),
          p("After World War I, Wassily Kandinsky returned to his birth city of Moscow to practice art there.
            However, despite their shared interest in the abstract, his ideas conflicted with those of the Russian
            avant-garde. Unlike his peers, who preferred systematic, rational abstraction, Kandinsky
            saw geometric forms as expressive and lyrical. Because of this tension, he moved to
            Germany and joined the Weimar Bauhaus faculty. There he found like-minded artists
            and produced Composition 8, an exploration of the psychospiritual influences of shape
            and color in which circles play a dominant role. Nancy Spector wrote of the piece,
            \"In Composition 8, the colorful, interactive geometric forms create a pulsating surface
            that is alternately dynamic and calm, aggressive and quiet.\" (1)"),
          p(
            "Modify Composition 8 to make it your own by adjusting the settings in the",
            em("Graphics Input"), "sidebar on the left."
          ),
          p("Change the size of the circles, line thickness, background color, shape transparency,
            or which kinds of geometries to include. You may also add random noise to the piece, generated
            from a normal distribution, and watch Kandinsky's careful composition fall apart. Remember to
            press the", em("Apply Changes"), "button to watch your modifications come to life."),
          p("(1) Spector, Nancy. “Vasily Kandinsky, Composition 8 (Komposition 8).” The Guggenheim
            Museums and Foundation. Accessed December 2, 2021. https://www.guggenheim.org/artwork/1924."),
          # div(plotOutput(
          #  outputId = "plot_kandinsky", inline = FALSE,
          #  height = "100%"
          # ), align = "center"),
          div(uiOutput(
            outputId = "ui_kandinsky",
            height = "100%"
          ), align = "center")
        )
      )
    ), # tab 3 panel
    tabPanel(
      title = "4. Barbara Kruger",
      sidebarLayout(
        sidebarPanel(
          h4("Graphics Input"),
          textInput(
            "path", "Image URL",
            "https://cdn.thecollector.com/wp-content/uploads/2020/03/image10-20.jpg"
          ),
          fluidRow(
            column(width = 6, colourInput(
              inputId = "text_color",
              label = "Text Color", value = "#FCFCFC"
            )),
            column(width = 6, colourInput(
              inputId = "rect_color",
              label = "Border Color", value = "#ED1C24"
            ))
          ),
          fluidRow(
            column(width = 6, sliderInput(
              inputId = "text_size",
              label = "Text size",
              min = 5, max = 50, value = 25, step = 0.5, ticks = FALSE
            )),
            column(width = 6, sliderInput(
              inputId = "border_size",
              label = "Border size",
              min = 0, max = 20, value = 12, step = 0.5, ticks = FALSE
            ))
          ),
          textInput("top", "top text:", "Your body"),
          textInput("middle", "Middle text:", "is a"),
          textInput("bottom", "Bottom text:", "battleground"),
          sliderInput(
            inputId = "img_brightness",
            label = "Brightness",
            min = 0, max = 500, value = 100, step = 5, round = TRUE, ticks = FALSE
          ),
          sliderInput(
            inputId = "img_saturation",
            label = "Saturation",
            min = -500, max = 500, value = 100, step = 5, round = TRUE, ticks = FALSE
          ),
          sliderInput(
            inputId = "img_hue",
            label = "Hue",
            min = -500, max = 500, value = 100, step = 5, round = TRUE, ticks = FALSE
          ),
          hr(),
          textInput("custom_filename_kruger", "Filename", "barbara_kruger.png"),
          verbatimTextOutput("value"),
          sliderInput(
            inputId = "res_kruger",
            label = "Resolution (in dpi)",
            min = 100, max = 2000, value = 600, step = 100, round = TRUE, ticks = FALSE
          ),
          div(
            align = "right",
            downloadLink("save_kruger", strong("Download"))
          )
        ), # sidebar panel,
        mainPanel(
          h2(strong("Barbara Kruger: Cultural Critique")),
          h5(em("Untitled (Your body is a battleground) (1989)")),
          p(""),
          p("Kruger is most known for her collage style that consists of black-and-white
            photographs, overlaid with declarative captions, stated in white-on-red
            text. The phrases in her works often include pronouns such as 'you', 'your',
            'I', 'we', and 'they', addressing cultural constructions of power, identity,
            consumerism, and sexuality."),
          p(
            "Kruger produced her ", em("Untitled (Your body is a battleground)"), " artwork for
          the Women’s March on Washington in support of reproductive freedom,
            simultaneously an art and a protest. We invite you to recreate your
            own version of Kruger's work and adjust the settings in the",
            em("Graphics Input"), "sidebar on the left. Change various elements
            of the text, border, and even replace the image from any image address
            to communicate a message you think is important!"
          ),
          div(plotOutput(
            outputId = "plot_kruger", inline = FALSE,
            height = "100%"
          ), align = "center"),
          p(""),
          prettyCheckbox(
            inputId = "original_artwork_kruger",
            label = "Original Artwork",
            value = FALSE
          ),
          div(imageOutput(outputId = "original_artwork_kruger", inline = TRUE), align = "center"),
          h4(" "),
          div(textOutput(outputId = "original_artwork_text_kruger", inline = TRUE), align = "center"),
          h4(" ")
        )
      )
    ) # tab 4 panel
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
    height = 450,
    width = 450
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
    0.3, 0.8, 0, 2.0, "yellow",
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

  plotInput_piet <- reactive({
    ggplot() +
      geom_rect(aes(xmin = 0, xmax = 10, ymin = 0, ymax = 12), fill = input$background_color_piet) +
      geom_rect(data = piet_geom_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
      scale_fill_manual(values = c(
        input$color_piet_1, input$color_piet_2,
        input$color_piet_3, input$color_piet_4
      )) +
      geom_rect(data = pietmondrianvertical, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = input$grid_color_piet, size = 3) +
      geom_hline(
        yintercept = seq(4, 10, length.out = input$piet_lines), size = 3, # input$sizehorizontal,
        color = input$grid_color_piet
      ) +
      geom_segment(data = piet_segment, aes(x = x, y = y, xend = xend, yend = yend), color = input$grid_color_piet, size = 3) +
      scale_x_continuous(limits = c(0, 10), expand = c(0, 0), breaks = seq(0, 10, by = 2)) +
      scale_y_continuous(limits = c(0, 12), expand = c(0, 0), breaks = seq(0, 12, by = 2)) +
      coord_fixed(xlim = c(0, 10), ylim = c(0, 12)) +
      theme_void() +
      theme(legend.position = "none")
  })

  output$plot_piet <- renderPlot(
    {
      plotInput_piet()
    },
    height = 400,
    width = 333
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
      ggsave(file,
        plot = plotInput_piet(), device = "png", dpi = as.double(custom_res_piet()),
        height = 4, width = 3.33
      )
    }
  )

  output$original_artwork_piet <- renderImage({
    if (input$original_artwork_piet == TRUE) {
      return(list(
        src = "./piet.jpg",
        width = 250,
        height = 333,
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

  ## KANDINSKY
  output$ui_kandinsky <- renderUI({
    imageOutput("static_kandinsky")
  })

  output$static_kandinsky <- renderImage(
    {
      list(
        src = "./kandinsky_ggplot.png",
        height = 510,
        width = 900,
        contentType = "image/png"
      )
    },
    deleteFile = FALSE
  )

  observeEvent(input$go_kandinsky, {
    output$ui_kandinsky <- renderUI({
      plotOutput("plot_kandinsky")
    })

    plotInput_kandinsky <- eventReactive(input$go_kandinsky, {

      # change circle size based on input$circlesize from ui.R
      for (i in c(1:6)) {
        circles_l[[1]][[i]] <- as.data.frame(circles_l[[1]][[i]]) %>%
          mutate(radius = radius * input$circlesize / 10)
      }

      # add random noise based on input$magnitudenoise from ui.R
      circles_l <- add_noise(circles_l, c(1:6), magnitude = input$magnitudenoise)
      lines_l <- add_noise(lines_l, c(1:2), magnitude = input$magnitudenoise)
      quads_l <- add_noise(quads_l, c(1:4), magnitude = input$magnitudenoise)
      semicircle_fill_l <- add_noise(semicircle_fill_l, magnitude = input$magnitudenoise)
      semicircle_stroke_l <- add_noise(semicircle_stroke_l, magnitude = input$magnitudenoise)
      semicircle_stroke_color_l <- add_noise(semicircle_stroke_color_l, magnitude = input$magnitudenoise)
      triangles_l <- add_noise(triangles_l, c(1:6), magnitude = input$magnitudenoise)

      # plotting functions
      plot_circles <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              new_scale_fill() +
              new_scale_color() +
              geom_circle(
                data = as.data.frame(circles_l[[1]][[i]]),
                aes(x0 = x, y0 = y, r = radius, fill = color, color = color, alpha = alpha)
              ) +
              scale_fill_manual(values = unique(circles_l[[2]][[i]][[1]])) +
              scale_color_manual(values = unique(circles_l[[2]][[i]][[1]]))
          }
          return(p)
        }
      }

      plot_semicircles <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              new_scale_fill() +
              new_scale_color() +
              geom_polygon(
                data = as.data.frame(semicircle_fill_l[[1]][[i]]),
                aes(x = x, y = y, group = id, fill = color, alpha = alpha)
              ) +
              scale_fill_manual(values = unique(semicircle_fill_l[[2]][[i]][[1]]))
          }

          return(p)
        }
      }

      plot_quads <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              new_scale_fill() +
              new_scale_color() +
              geom_polygon(
                data = as.data.frame(quads_l[[1]][[i]]),
                aes(x = x, y = y, group = id, fill = color, alpha = alpha)
              ) +
              scale_fill_manual(values = unique(quads_l[[2]][[i]][[1]]))
          }

          return(p)
        }
      }

      plot_triangles <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              new_scale_fill() +
              new_scale_color() +
              geom_polygon(
                data = as.data.frame(triangles_l[[1]][[i]]),
                aes(x = x, y = y, group = id, fill = color, alpha = alpha)
              ) +
              scale_fill_manual(values = unique(triangles_l[[2]][[i]][[1]]))
          }

          return(p)
        }
      }

      plot_lines <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              # new_scale_fill() +
              # new_scale_color() +
              geom_segment(
                data = as.data.frame(lines_l[[1]][[i]]),
                aes(x = x, xend = xend, y = y, yend = yend, size = thickness^2)
              )
          }

          return(p)
        }
      }

      plot_semicircle_stroke <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              new_scale_fill() +
              new_scale_color() +
              geom_path(
                data = as.data.frame(semicircle_stroke_l[[1]][[i]]),
                aes(x = x, y = y, group = id, color = color, size = thickness^2)
              ) +
              scale_color_manual(values = unique(semicircle_stroke_l[[2]][[i]][[1]]))
          }

          return(p)
        }
      }

      plot_semicircle_stroke_color <- function(layers = c(), execute = TRUE) {
        if (execute == FALSE) {
          return(p)
        } else {
          for (i in layers) {
            p <- p +
              new_scale_fill() +
              new_scale_color() +
              geom_path(
                data = as.data.frame(semicircle_stroke_color_l[[1]][[i]]),
                aes(x = x, y = y, group = id, color = color, size = thickness^2)
              ) +
              scale_color_manual(values = unique(semicircle_stroke_color_l[[2]][[i]][[1]]))
          }

          return(p)
        }
      }

      print_circles <- ("Circles" %in% input$checkbox_layers)
      print_quads <- ("Quadrilaterals" %in% input$checkbox_layers)
      print_lines_curved <- ("Curved Lines" %in% input$checkbox_layers)
      print_lines_straight <- ("Straight Lines" %in% input$checkbox_layers)
      print_triangles <- ("Triangles" %in% input$checkbox_layers)

      # plot artwork
      p <- ggplot() +
        scale_alpha(range = c(input$alpha[1], input$alpha[2])) +
        scale_size(range = c(input$linethickness[1], input$linethickness[2])) +
        coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
        theme_void() +
        theme(
          legend.position = "none",
          panel.background = element_rect(fill = input$background_color, color = input$background_color),
          panel.border = element_blank()
        )

      p <- plot_triangles(c(6:3), execute = print_triangles)
      p <- plot_semicircles(c(3:1), execute = print_circles)
      p <- plot_semicircle_stroke_color(c(1), execute = print_lines_curved)
      p <- plot_semicircle_stroke(c(1), execute = print_lines_curved)
      p <- plot_quads(c(4:1), execute = print_quads)
      p <- plot_circles(c(6:2), execute = print_circles)
      p <- plot_triangles(c(2:1), execute = print_triangles)
      p <- plot_circles(c(1), execute = print_circles)
      p <- plot_lines(c(2:1), execute = print_lines_straight)

      p
    })

    output$plot_kandinsky <- renderPlot(
      {
        plotInput_kandinsky()
      },
      height = 510,
      width = 900
    )
  })


  output$save_kandinsky <- downloadHandler(
    filename = function() {
      input$custom_filename_kandinsky
    },
    content = function(file) {
      ggsave(file, plot = plot_kandinsky(), device = "png", dpi = as.double(input$res_kandinsky))
    }
  )

  output$original_artwork_kandinsky <- renderImage({
    if (input$original_artwork_kandinsky == TRUE) {
      return(list(
        src = "./kandinsky.jpg",
        width = 450,
        height = 315,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
    if (input$original_artwork_kandinsky == FALSE) {
      return(list(
        src = "./kandinsky.jpg",
        width = 0,
        height = 0,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
  })

  output$original_artwork_text_kandinsky <- renderText({
    if (input$original_artwork_kandinsky == TRUE) {
      return(paste("Solomon R. Guggenheim Founding Collection, By gift © 2018 Artists Rights Society (ARS), New York/ADAGP, Paris"))
    }
    if (input$original_artwork_kandinsky == FALSE) {
      return(NULL)
    }
  })

  # BARBARA KRUGER

  link <- reactive(input$path)


  linker <- reactive({
    if (url.exists(link()) == TRUE) {
      link()
    } else {
      "https://cdn.thecollector.com/wp-content/uploads/2020/03/image10-20.jpg"
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


  magick_image <- reactive({
    image_read(linker())
  })


  magick_plot <- reactive({
    if (image_info(magick_image())$height >= image_info(magick_image())$width) {
      image_scale(magick_image(), "x550")
    } else {
      image_scale(magick_image(), "550")
    }
  })

  img_height <- reactive({
    image_info(magick_plot())$height
  })
  img_width <- reactive({
    image_info(magick_plot())$width
  })


  size_kruger <- reactive({
    if (img_height() > img_width()) {
      img_width() / 500
    } else {
      img_height() / 500
    }
  })

  top_plot <- reactive({
    ifelse(nchar(input$top) == 0, NA, input$top)
  })
  middle_plot <- reactive({
    ifelse(nchar(input$middle) == 0, NA, input$middle)
  })
  bottom_plot <- reactive({
    ifelse(nchar(input$bottom) == 0, NA, input$bottom)
  })

  x_plot <- reactive({
    c(img_width() / 2, img_width() / 2, img_width() / 2)
  })
  y_plot <- reactive({
    c(img_height() - 30 * size_kruger(), img_height() / 2, 30 * size_kruger())
  })

  df_kruger <- reactive({
    data.frame(x_plot(), y_plot())
  })


    plotInput_kruger <- reactive({
         ggplot() +
          geom_rect(aes(
            xmin = 0, xmax = img_width(),
            ymin = 0, ymax = img_height()
          ),
          color = input$rect_color,
          size = input$border_size,
          fill = NA
          ) +
          draw_image(image_modulate(magick_plot(),
            brightness = input$img_brightness,
            saturation = input$img_saturation,
            hue = input$img_hue
          ),
          x = 0, y = 0, width = img_width(), height = img_height()
          ) +
          geom_label(
            data = df_kruger(),
            mapping = aes(
              x = x_plot(),
              y = y_plot()
            ),
            label = c(top_plot(), middle_plot(), bottom_plot()),
            size = input$text_size,
            fill = input$rect_color,
            color = input$text_color,
            family = "Futura",
            fontface = "bold",
            label.size = 0,
            label.r = unit(0, "lines")
          ) +
          coord_equal() +
          theme_void()
  })

observe({
  output$plot_kruger <- renderPlot(
      {
        plotInput_kruger()

      },
      height = img_height(),
      width = img_width()
    )})



  custom_filename_kruger <- reactive({
    input$custom_filename_kruger
  })

  custom_res_kruger <- reactive({
    input$res_kruger
  })

  output$save_kruger <- downloadHandler(
    filename = function() {
      custom_filename_kruger()
    },
    content = function(file) {
      ggsave(file,
             plot = plotInput_kruger(), device = "png", dpi = as.double(custom_res_kruger()),
             height = 4/img_height()*img_height(), width = 4/img_height()*img_width())
    }
  )


  output$original_artwork_kruger <- renderImage({
    if (input$original_artwork_kruger == TRUE) {
      return(list(
        src = "./kruger.jpg",
        width = 300,
        height = 300,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
    if (input$original_artwork_kruger == FALSE) {
      return(list(
        src = "./kruger.jpg",
        width = 0,
        height = 0,
        contentType = "image/jpg",
        alt = "Original Artwork",
        deleteFile = FALSE
      ))
    }
  })

  output$original_artwork_text_kruger <- renderText({
    if (input$original_artwork_kruger == TRUE) {
      return(paste("The Inaugural Installation, © 1989 The Broad, Los Angeles"))
    }
    if (input$original_artwork_kruger == FALSE) {
      return(NULL)
    }
  })
}

# Run the application
shinyApp(ui = ui, server = server)
