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

# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "cosmo",
                                 bg = "#fff",
                                 fg = "#000",
                                 primary = "#4060ff",
                                 base_font = font_google("Zen Kaku Gothic Antique"),
                                 code_font = font_google("Zen Kaku Gothic Antique")
                                 ),

                navbarPage(title = "Viz Biz",
                           tabPanel(title = "L'esthétique des ondes mathématiques",

                                    sidebarLayout(
                                        sidebarPanel(

                                            h3("Graphics Input"),

                                            spectrumInput(
                                                inputId = "myColor",
                                                label = " Select color:",
                                                choices = list(
                                                    list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
                                                    as.list(brewer_pal(palette = "Blues")(9)),
                                                    as.list(brewer_pal(palette = "Greens")(9)),
                                                    as.list(brewer_pal(palette = "Spectral")(11)),
                                                    as.list(brewer_pal(palette = "Dark2")(8))
                                                ),
                                                options = list(`toggle-palette-more-text` = "Show more")
                                            ),
                                            # verbatimTextOutput(outputId = "res"),

                                            spectrumInput(
                                                inputId = "myBackgroundColor",
                                                label = "Select background Color:",
                                                choices = list(
                                                    list('black', 'white', 'blanchedalmond', 'steelblue', 'forestgreen'),
                                                    as.list(brewer_pal(palette = "Blues")(9)),
                                                    as.list(brewer_pal(palette = "Greens")(9)),
                                                    as.list(brewer_pal(palette = "Spectral")(11)),
                                                    as.list(brewer_pal(palette = "Dark2")(8))
                                                ),
                                                options = list(`toggle-palette-more-text` = "Show more")
                                            ),
                                            # verbatimTextOutput(outputId = "res"),

                                            checkboxInput(inputId = "myPolar",
                                                          label = "Polar coordinate",
                                                          value = FALSE),

                                            # plotOutput(outputId = "final", inline = FALSE),

                                            hr(),
                                            div(align = "right",
                                                actionButton(inputId = "getdata",
                                                             label   = strong("Generate random output")))

                                        ), # sidebar panel

                                        mainPanel(
                                            h1("Title"),
                                            h5("Text"),

                                            plotOutput(outputId = "myPlot", inline = FALSE),

                                        ) # main panel
                                    ) # sidebar 3 layout

                                    ) # tab 1 panel
                           ) # navbar page
                ) # fluid page

# Define server logic required to draw a histogram
server <- function(input, output) {

  IMG_DIR <- "img/"
  IMG_SUBDIR <- "everything/"
  IMG_SUBDIR2 <- "handpicked/"
  IMG_PATH <- paste0(IMG_DIR,
                     IMG_SUBDIR)
  LOGFILE_DIR <- "logfile/"
  LOGFILE <- "logfile.csv"
  LOGFILE_PATH <- paste0(LOGFILE_DIR,
                         LOGFILE)
  # create the directory structure
  generativeart::setup_directories(IMG_DIR,
                                   IMG_SUBDIR,
                                   IMG_SUBDIR2,
                                   LOGFILE_DIR)

  polar <- reactive({(input$myPolar)})
  color <- reactive({(input$myColor)})
  background_color <- reactive({(input$myBackgroundColor)})

  # output$myPlot <- renderPlot({generativeart::generate_img(
  #   formula = list(
  #     x = quote(runif(1, -1, 10) * x_i^2 - sin(y_i^2)),
  #     y = quote(runif(1, -1, 10) * y_i^3 - cos(x_i^2) * y_i^4)
  #   ),
  #   nr_of_img = 1, filetype = NULL,
  #   polar = polar(),
  #   color = color(),
  #   background_color = background_color())},
  #   deleteFile = FALSE)

  final_plot <- eventReactive(input$getdata == TRUE, {
    generativeart::generate_img(
      formula = list(x = quote(runif(1, -1, 10) * x_i^2 - sin(y_i^2)),
                     y = quote(runif(1, -1, 10) * y_i^3 - cos(x_i^2) * y_i^4)),
      nr_of_img = 1,
      polar = polar(),
      color = color(),
      background_color = background_color())
    })


  output$myPlot <- renderPlot({
    last_plot()
  })

  # final <- last_plot()
}

# Run the application
shinyApp(ui = ui, server = server)
