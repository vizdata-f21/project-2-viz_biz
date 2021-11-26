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
library(Cairo); options(shiny.usecairo = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = bs_theme(bootswatch = "cosmo",
                                 bg = "#fff",
                                 fg = "#000",
                                 primary = "#4060ff",
                                 base_font = font_google("Zen Kaku Gothic Antique"),
                                 code_font = font_google("Zen Kaku Gothic Antique")),


    navbarPage(title = "Viz Biz",
            tabPanel(title = "Piet Mondrian",

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Graphics Input"),
            div(align = "right"),
            sliderInput("lines",
                                 "Number of lines:",
                                 min = 2,
                                 max = 10,
                                 value = 4),
        checkboxGroupInput("color", "Choose 4 colors:",
                           c("Black",
                             "Blue",
                             "Red",
                             "Yellow",
                             "Purple",
                             "Green",
                             "Orange",
                             "Pink",
                             "Brown" = "tan4",
                             "Turquoise" = "cyan3"), selected = c("Black", "Blue", "Red", "Yellow"))),
        mainPanel(
            h2(strong("Piet Mondrian: Trafalgar Square")),
            h5(em("1939-1943")),
            plotOutput("plot")
        )
))))


server <- function(input, output) {

    pietmondrianvertical <- tribble(
        ~xmin, ~xmax, ~ymin, ~ymax,
        0.1, 0.4, 0, 12,
        0.8, 1.1, 0, 12,
        2, 2.3, 0, 12,
        9.4, 9.7, 0, 12,
        8.7, 9, 0, 12
    )

   geom_rect <- tribble(
            ~xmin, ~xmax, ~ymin, ~ymax, ~fill,
            0.3, 0.8, 0, 1.9, "yellow",
            1.1, 2, 4.05, 5.9, "red",
            0,0.1, 4.05, 5.9, "yellow",
            0, 0.8, 10, 12, "yellow",
            2.05, 3, 0, 0.7, "blue",
            5, 5.8, 0, 0.7, "red",
            6.8, 7.5, 0, 0.7, "black",
            9,9.7,0, 0.7, "yellow",
            9,9.7, 1.3, 2, "black",
            9, 9.7, 2.8, 3.4, "red",
            9, 9.7, 4.8, 5.4, "black",
            9.7, 10, 4, 6, "blue")



        segment <- tribble(
            ~x, ~y, ~xend, ~yend,
            0.3, 3, 2.2, 3,
            0.3, 2, 0.805, 2,
            0.3, 1.3, 0.805, 1.3,
            0.805, 1.7, 2.05, 1.7,
            2, 0.7, 9.7, 0.7)


    output$plot <- renderPlot({
        piet_mondrian_plot <- ggplot() +
            geom_rect(data = geom_rect, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = fill)) +
            scale_fill_manual(values = c(input$color, rep("white", 4))[1:4]) +
            geom_rect(data = pietmondrianvertical, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), fill = "black") +
            geom_hline(yintercept = seq(4, 10, length.out = input$lines), size = 3) +
            geom_segment(data= segment, aes(x = x, y = y, xend = xend, yend = yend), color = "black", size = 3) +
            scale_x_continuous(limits = c(0, 10),breaks = seq(0, 10, by = 2))  +
            scale_y_continuous(limits = c(0, 12), breaks = seq(0, 12, by = 2)) +
            coord_fixed(xlim = c(0, 10), ylim = c(0,12)) +
            theme_void() +
            theme(legend.position = "none")
        piet_mondrian_plot}, height = 600, width = 500)


}

# Run the application
shinyApp(ui = ui, server = server)