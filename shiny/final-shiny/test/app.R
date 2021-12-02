library(shiny)
library(tidyverse)
library(ggforce)
library(magrittr)
library(colourpicker)

knitr::opts_chunk$set(
    fig.width = 8, fig.asp = 0.618, fig.retina = 3, dpi = 300, out.width = "100%"
)

# load data
circles_l <- readRDS("data/circles.rds")
lines_l <- readRDS("data/lines.rds")
quads_l <- readRDS("data/quads.rds")
semicircle_fill_l <- readRDS("data/semicircle-fill.rds")
semicircle_stroke_l <- readRDS("data/semicircle-stroke.rds")
semicircle_stroke_color_l <- readRDS("data/semicircle-stroke-color.rds")
triangles_l <- readRDS("data/triangles.rds")

# write functions
clip <- function(x, low, high) {
    x[x < low] <- low
    x[x > high] <- high
    return(x)
}

add_noise <- function(df, layers = c(1), magnitude = 5){
    for (i in layers){
        df[[1]][[i]] <- as.data.frame(df[[1]][[i]]) %>%
            group_by(grouping) %>%
            mutate(noise_x = rnorm(1, 0, magnitude),
                   noise_y = rnorm(1, 0, magnitude)) %>%
            ungroup() %>%
            mutate(across(contains("x"), ~ clip(.x + noise_x, xmin, xmax)),
                   across(contains("y"), ~ clip(.x + noise_y, ymin, ymax)))
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

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wassily Kandinsky, Composition 8 (1923)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
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
        # Show a plot of the generated distribution
        mainPanel(h2(strong("Wassily Kandinsky: Composing Oneself")),
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
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ui_kandinsky <- renderUI({
        imageOutput("static_kandinsky")
    })

    output$static_kandinsky <- renderImage(
        {
            list(
                src = "./kandinsky_ggplot.png",
                height = 800,
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
            height = 800,
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
}

# Run the application
shinyApp(ui = ui, server = server)
