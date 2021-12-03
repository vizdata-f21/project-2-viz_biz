# functions for Shiny
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