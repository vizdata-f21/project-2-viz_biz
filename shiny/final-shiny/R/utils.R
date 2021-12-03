# load packages ----------------------------------------------------------------

# http://shiny.rstudio.com/
library(shiny) # Eli
library(shinythemes)
library(tidyverse) # Eli
library(DT)
library(shinyalert)
library(ggiraph)
library(shinyWidgets)
library(magrittr)
library(tibble)
library(styler)
library(RColorBrewer)
library(colorRamps)
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
library(shinycssloaders) # Lilly

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
