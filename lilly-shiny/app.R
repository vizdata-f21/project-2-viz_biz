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
                        max = 25,
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

        # plot artwork
        ggplot() +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[6]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[6]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[5]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[5]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[4]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[4]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[3]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[3]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(semicircle_fill_l[[1]][[2]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(semicircle_fill_l[[2]][[2]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(semicircle_fill_l[[1]][[1]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(semicircle_fill_l[[2]][[1]][[1]])) +
            new_scale_color() +
            geom_path(data = as.data.frame(semicircle_stroke_color_l[[1]][[1]]), aes(x = x, y = y, group = id, color = color)) +
            scale_color_manual(values = unique(semicircle_stroke_color_l[[2]][[1]][[1]])) +
            new_scale_color() +
            geom_path(data = as.data.frame(semicircle_stroke_l[[1]][[1]]), aes(x = x, y = y, group = id, color = color)) +
            scale_color_manual(values = unique(semicircle_stroke_l[[2]][[1]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(quads_l[[1]][[4]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(quads_l[[2]][[4]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(quads_l[[1]][[3]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(quads_l[[2]][[3]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(quads_l[[1]][[2]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(quads_l[[2]][[2]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(quads_l[[1]][[1]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(quads_l[[2]][[1]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[6]]), aes(x0 = x, y0 = y, r = radius, fill = color, color = color)) +
            scale_fill_manual(values = unique(circles_l[[2]][[6]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[6]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[5]]), aes(x0 = x, y0 = y, r = radius, fill = color, color = color)) +
            scale_fill_manual(values = unique(circles_l[[2]][[5]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[5]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[4]]), aes(x0 = x, y0 = y, r = radius, fill = color, color = color)) +
            scale_fill_manual(values = unique(circles_l[[2]][[4]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[4]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[3]]), aes(x0 = x, y0 = y, r = radius, fill = color, color = color)) +
            scale_fill_manual(values = unique(circles_l[[2]][[3]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[3]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[2]]), aes(x0 = x, y0 = y, r = radius, fill = color, color = color)) +
            scale_fill_manual(values = unique(circles_l[[2]][[2]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[2]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[2]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[2]][[1]])) +
            new_scale_fill() +
            geom_polygon(data = as.data.frame(triangles_l[[1]][[1]]), aes(x = x, y = y, group = id, fill = color)) +
            scale_fill_manual(values = unique(triangles_l[[2]][[1]][[1]])) +
            new_scale_fill() +
            new_scale_color() +
            geom_circle(data = as.data.frame(circles_l[[1]][[1]]), aes(x0 = x, y0 = y, r = radius, fill = color, color = color)) +
            scale_fill_manual(values = unique(circles_l[[2]][[1]][[1]])) +
            scale_color_manual(values = unique(circles_l[[2]][[1]][[1]])) +
            geom_segment(data = as.data.frame(lines_l[[1]][[2]]), aes(x = x, xend = xend, y = y, yend = yend)) +
            geom_segment(data = as.data.frame(lines_l[[1]][[1]]), aes(x = x, xend = xend, y = y, yend = yend)) +
            coord_fixed(xlim = c(0, 152), ylim = c(0, 86), expand = FALSE) +
            theme_void() +
            theme(legend.position = "none",
                  panel.background = element_rect(fill = "#EBE6CE"))
    })
}

# Run the application
shinyApp(ui = ui, server = server)
