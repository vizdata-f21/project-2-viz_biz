#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggforce)
library(magrittr)

knitr::opts_chunk$set(
    fig.width = 8, fig.asp = 0.618, fig.retina = 3, dpi = 300, out.width = "60%"
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

plot_circles <- function(layers = c()){
    for(i in layers){
        p <- p +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[i]]),
                        aes(x0 = x, y0 = y, r = radius, fill = color, color = color, alpha = alpha)) +
            scale_fill_manual(values = unique(circles_l[[2]][[i]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[i]][[1]]))
    }

    return(p)
}

plot_semicircles <- function(layers = c()){
    for(i in layers){
        p <- p +
            new_scale_fill() +
            new_scale_color() +
            geom_polygon(data = as.data.frame(semicircle_fill_l[[1]][[i]]),
                         aes(x = x, y = y, group = id, fill = color, alpha = alpha)) +
            scale_fill_manual(values = unique(semicircle_fill_l[[2]][[i]][[1]]))
    }

    return(p)
}

plot_quads <- function(layers = c()){
    for(i in layers){
        p <- p +
            new_scale_fill() +
            new_scale_color() +
            geom_polygon(data = as.data.frame(quads_l[[1]][[i]]),
                         aes(x = x, y = y, group = id, fill = color, alpha = alpha)) +
            scale_fill_manual(values = unique(quads_l[[2]][[i]][[1]]))
    }

    return(p)
}

plot_triangles <- function(layers = c()){
    for(i in layers){
        p <- p +
            new_scale_fill() +
            new_scale_color() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[i]]),
                         aes(x = x, y = y, group = id, fill = color, alpha = alpha)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[i]][[1]]))
    }

    return(p)
}

plot_lines <- function(layers = c()){
    for(i in layers){
        p <- p +
            #new_scale_fill() +
            #new_scale_color() +
            geom_segment(data = as.data.frame(lines_l[[1]][[i]]),
                         aes(x = x, xend = xend, y = y, yend = yend, size = thickness ^ 2))
    }

    return(p)
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Wassily Kandinsky, Composition 8 (1923)"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h4("Circles"),
            sliderInput("circlesize",
                        "Size:",
                        min = 1,
                        max = 20,
                        value = 10),
            sliderInput("circlealpha",
                        "Transparency:",
                        min = 1,
                        max = 100,
                        value = 10),
            h4("Add random noise"),
            sliderInput("magnitudenoise",
                        "Magnitude:",
                        min = 0,
                        max = 50,
                        value = 0)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("kandinsky")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$kandinsky <- renderPlot({

        # change circle size based on input$circlesize from ui.R
        for (i in c(1:6)){
            circles_l[[1]][[i]] <- as.data.frame(circles_l[[1]][[i]]) %>%
                mutate(radius = radius * input$circlesize / 10)
        }

        # change circle alpha based on input$circlealpha from ui.R
        for (i in c(1:6)){
            circles_l[[1]][[i]] <- as.data.frame(circles_l[[1]][[i]]) %>%
                mutate(radius = radius * input$circlesize / 10)
        }

        # add random noise based on input$magnitudenoise from ui.R
#        circles_l <- add_noise(circles_l, c(1:6), magnitude = input$magnitudenoise)
#        lines_l <- add_noise(lines_l, c(1:2), magnitude = input$magnitudenoise)
#        quads_l <- add_noise(quads_l, c(1:4), magnitude = input$magnitudenoise)
#        semicircle_fill_l <- add_noise(semicircle_fill_l, magnitude = input$magnitudenoise)
#        semicircle_stroke_l <- add_noise(semicircle_stroke_l, magnitude = input$magnitudenoise)
#        semicircle_stroke_color_l <- add_noise(semicircle_stroke_color_l, magnitude = input$magnitudenoise)
#        triangles_l <- add_noise(triangles_l, c(1:6), magnitude = input$magnitudenoise)

        # plot artwork
        p <- ggplot() +
            scale_alpha(range = c(0.8, 1)) +
            scale_size(range = c(0.3, 1.3)) +
            coord_fixed(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
            theme_void() +
            theme(legend.position = "none",
                  panel.background = element_rect(fill = "#EBE6CE"))

        p <- plot_triangles(c(6:3))
        p <- plot_semicircles(c(2:1))

        p <- p +
            new_scale_fill() +
            new_scale_color() +
            geom_path(data = as.data.frame(semicircle_stroke_color_l[[1]][[1]]),
                      aes(x = x, y = y, group = id, color = color, size = thickness ^ 2)) +
            scale_color_manual(values = unique(semicircle_stroke_color_l[[2]][[1]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_path(data = as.data.frame(semicircle_stroke_l[[1]][[1]]),
                      aes(x = x, y = y, group = id, color = color, size = thickness ^ 2)) +
            scale_color_manual(values = unique(semicircle_stroke_l[[2]][[1]][[1]]))

        p <- plot_quads(c(4:1))
        p <- plot_circles(c(6:2))
        p <- plot_triangles(c(2:1))
        p <- plot_circles(c(1))
        p <- plot_lines(c(2:1))

        p
    })
}

# Run the application
shinyApp(ui = ui, server = server)