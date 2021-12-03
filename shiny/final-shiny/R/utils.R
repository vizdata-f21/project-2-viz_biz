# load packages ----------------------------------------------------------------

# http://shiny.rstudio.com/
library(shiny) # Eli
library(shinythemes)
library(tidyverse) # Eli
library(DT)
library(shinyalert)
library(shinyWidgets)
library(ggiraph)
library(statebins)
library(gganimate)
library(colorspace)
library(ggrepel)
library(scales)
library(ggridges)
library(magrittr)
library(forcats)
library(tibble)
library(janitor)
library(grid)
library(gridExtra)
library(styler)
library(RColorBrewer)
library(colorRamps)
library(glue)
library(RColorBrewer)
library(ggwordcloud)
library(ggtext)
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

# set limits -------------------------------------------------------------------

xmin <- 0
xmax <- 152
ymin <- 0
ymax <- 86

# load font --------------------------------------------------------------------

font_add(
  family = "Futura",
  regular = "data/Futura.ttf"
)
showtext_auto()

# kandinsky prep ---------------------------------------------------------------

print_circles <- FALSE
print_quads <- FALSE
print_lines_curved <- FALSE
print_lines_straight <- FALSE
print_triangles <- FALSE

# functions: add noise ---------------------------------------------------------

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

# functions: plotting ----------------------------------------------------------

plot_circles <- function(layers = c(), execute = TRUE, skip_color = FALSE){
  if(execute == FALSE) {
    return(p)
  } else{
    if(skip_color == TRUE){
      for(i in layers){
        p <- p +
          geom_circle(data = as.data.frame(circles_l[[1]][[i]]),
                      aes(x0 = x, y0 = y, r = radius, fill = color_cat, color = color_cat, alpha = alpha))
      }
      return(p)
    } else{
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
  }
}

plot_semicircles <- function(layers = c(), execute = TRUE, skip_color = FALSE){
  if(execute == FALSE) {
    return(p)
  } else{
    if(skip_color == TRUE){
      for(i in layers){
        p <- p +
          geom_polygon(data = as.data.frame(semicircle_fill_l[[1]][[i]]),
                       aes(x = x, y = y, group = id, fill = color_cat, alpha = alpha))
      }
      return(p)
    } else{
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
  }
}

plot_quads <- function(layers = c(), execute = TRUE, skip_color = FALSE){
  if(execute == FALSE) {
    return(p)
  } else{
    if(skip_color == TRUE){
      for(i in layers){
        p <- p +
          geom_polygon(data = as.data.frame(quads_l[[1]][[i]]),
                       aes(x = x, y = y, group = id, fill = color_cat, alpha = alpha))
      }
      return(p)
    } else{
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
  }
}

plot_triangles <- function(layers = c(), execute = TRUE, skip_color = FALSE){
  if(execute == FALSE) {
    return(p)
  } else{
    if(skip_color == TRUE){
      for(i in layers){
        p <- p +
          geom_polygon(data = as.data.frame(triangles_l[[1]][[i]]),
                       aes(x = x, y = y, group = id, fill = color_cat, alpha = alpha))
      }
      return(p)
    } else{
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
  }
}

plot_lines <- function(layers = c(), execute = TRUE){
  if(execute == FALSE) {
    return(p)
  } else{
    for(i in layers){
      p <- p +
        geom_segment(data = as.data.frame(lines_l[[1]][[i]]),
                     aes(x = x, xend = xend, y = y, yend = yend, size = thickness ^ 2))
    }

    return(p)
  }
}

plot_semicircle_stroke <- function(layers = c(), execute = TRUE, skip_color = FALSE){
  if(execute == FALSE) {
    return(p)
  } else{
    if(skip_color == TRUE){
      for(i in layers){
        p <- p +
          geom_path(data = as.data.frame(semicircle_stroke_l[[1]][[i]]),
                    aes(x = x, y = y, group = id, color = color_cat, size = thickness ^ 2))
      }
      return(p)
    } else{
      for(i in layers){
        p <- p +
          new_scale_fill() +
          new_scale_color() +
          geom_path(data = as.data.frame(semicircle_stroke_l[[1]][[i]]),
                    aes(x = x, y = y, group = id, color = color, size = thickness ^ 2)) +
          scale_color_manual(values = unique(semicircle_stroke_l[[2]][[i]][[1]]))
      }
      return(p)
    }
  }
}

plot_semicircle_stroke_color <- function(layers = c(), execute = TRUE, skip_color = FALSE){
  if(execute == FALSE) {
    return(p)
  } else{
    if(skip_color == TRUE){
      for(i in layers){
        p <- p +
          geom_path(data = as.data.frame(semicircle_stroke_color_l[[1]][[i]]),
                    aes(x = x, y = y, group = id, color = color_cat, size = thickness ^ 2))
      }
      return(p)
    } else{
      for(i in layers){
        p <- p +
          new_scale_fill() +
          new_scale_color() +
          geom_path(data = as.data.frame(semicircle_stroke_color_l[[1]][[i]]),
                    aes(x = x, y = y, group = id, color = color, size = thickness ^ 2)) +
          scale_color_manual(values = unique(semicircle_stroke_color_l[[2]][[i]][[1]]))
      }
      return(p)
    }
  }
}