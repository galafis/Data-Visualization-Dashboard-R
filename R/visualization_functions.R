# =============================================================================
# VISUALIZATION FUNCTIONS - DATA VISUALIZATION DASHBOARD
# =============================================================================
# Author: Data Visualization Dashboard Project
# Description: Core visualization functions for creating dynamic plots and charts
# Dependencies: ggplot2, plotly, dplyr, shiny
# Last Updated: 2025-09-15
# =============================================================================

library(ggplot2)
library(plotly)
library(dplyr)
library(shiny)

#' Create Interactive Scatter Plot
#'
#' Creates an interactive scatter plot using ggplot2 and plotly
#' with proper error handling and validation
#'
#' @param data A data.frame containing the data to plot
#' @param x Character string specifying the x-axis variable name
#' @param y Character string specifying the y-axis variable name
#' @param color Optional character string for color grouping variable
#' @param title Optional plot title (default: "Scatter Plot")
#' @param x_label Optional x-axis label (default: x variable name)
#' @param y_label Optional y-axis label (default: y variable name)
#'
#' @return A plotly interactive plot object
#' @export
#'
#' @examples
#' \dontrun{
#' data(mtcars)
#' create_scatter_plot(mtcars, "wt", "mpg", color = "cyl")
#' }
create_scatter_plot <- function(data, x, y, color = NULL, 
                               title = "Scatter Plot",
                               x_label = NULL, y_label = NULL) {
  
  # Input validation
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data.frame")
  }
  
  if (nrow(data) == 0) {
    warning("Warning: Empty dataset provided")
    return(NULL)
  }
  
  if (!x %in% names(data)) {
    stop(paste("Error: Column", x, "not found in data"))
  }
  
  if (!y %in% names(data)) {
    stop(paste("Error: Column", y, "not found in data"))
  }
  
  if (!is.null(color) && !color %in% names(data)) {
    stop(paste("Error: Color column", color, "not found in data"))
  }
  
  # Set default labels
  if (is.null(x_label)) x_label <- x
  if (is.null(y_label)) y_label <- y
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x, y = y))
  
  # Add color aesthetic if specified
  if (!is.null(color)) {
    p <- p + geom_point(aes_string(color = color), alpha = 0.7, size = 3)
  } else {
    p <- p + geom_point(alpha = 0.7, size = 3, color = "steelblue")
  }
  
  # Customize plot appearance
  p <- p +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      axis.title = element_text(size = 12),
      legend.position = "bottom"
    )
  
  # Convert to interactive plotly
  interactive_plot <- ggplotly(p, tooltip = c("x", "y", "colour"))
  
  return(interactive_plot)
}

#' Create Bar Chart
#'
#' Creates an interactive bar chart with customization options
#'
#' @param data A data.frame containing the data to plot
#' @param x Character string specifying the categorical variable
#' @param y Character string specifying the numeric variable
#' @param fill Optional character string for fill color grouping
#' @param title Optional plot title (default: "Bar Chart")
#' @param horizontal Logical, whether to create horizontal bars (default: FALSE)
#'
#' @return A plotly interactive plot object
#' @export
create_bar_chart <- function(data, x, y, fill = NULL, 
                            title = "Bar Chart", horizontal = FALSE) {
  
  # Input validation
  validate_data_inputs(data, x, y, fill)
  
  # Create base plot
  if (horizontal) {
    p <- ggplot(data, aes_string(x = y, y = x))
  } else {
    p <- ggplot(data, aes_string(x = x, y = y))
  }
  
  # Add bars with optional fill
  if (!is.null(fill)) {
    p <- p + geom_col(aes_string(fill = fill), alpha = 0.8)
  } else {
    p <- p + geom_col(fill = "steelblue", alpha = 0.8)
  }
  
  # Customize appearance
  p <- p +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # Add horizontal coordinate flip if needed
  if (horizontal) {
    p <- p + coord_flip()
  }
  
  return(ggplotly(p))
}

#' Create Line Chart
#'
#' Creates an interactive line chart for time series or continuous data
#'
#' @param data A data.frame containing the data to plot
#' @param x Character string specifying the x-axis variable (typically time)
#' @param y Character string specifying the y-axis variable
#' @param group Optional character string for grouping lines
#' @param title Optional plot title (default: "Line Chart")
#'
#' @return A plotly interactive plot object
#' @export
create_line_chart <- function(data, x, y, group = NULL, title = "Line Chart") {
  
  # Input validation
  validate_data_inputs(data, x, y, group)
  
  # Create base plot
  p <- ggplot(data, aes_string(x = x, y = y))
  
  # Add lines with optional grouping
  if (!is.null(group)) {
    p <- p + geom_line(aes_string(color = group), size = 1.2, alpha = 0.8) +
             geom_point(aes_string(color = group), size = 2, alpha = 0.7)
  } else {
    p <- p + geom_line(color = "steelblue", size = 1.2, alpha = 0.8) +
             geom_point(color = "steelblue", size = 2, alpha = 0.7)
  }
  
  # Customize appearance
  p <- p +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text = element_text(size = 11),
      legend.position = "bottom"
    )
  
  return(ggplotly(p))
}

#' Create Histogram
#'
#' Creates an interactive histogram with customization options
#'
#' @param data A data.frame containing the data to plot
#' @param x Character string specifying the numeric variable
#' @param bins Number of bins for histogram (default: 30)
#' @param fill_color Fill color for bars (default: "steelblue")
#' @param title Optional plot title (default: "Histogram")
#'
#' @return A plotly interactive plot object
#' @export
create_histogram <- function(data, x, bins = 30, fill_color = "steelblue", 
                            title = "Histogram") {
  
  # Input validation
  if (!is.data.frame(data) || nrow(data) == 0) {
    stop("Error: Invalid or empty dataset")
  }
  
  if (!x %in% names(data)) {
    stop(paste("Error: Column", x, "not found in data"))
  }
  
  if (!is.numeric(data[[x]])) {
    stop(paste("Error: Column", x, "must be numeric for histogram"))
  }
  
  # Create histogram
  p <- ggplot(data, aes_string(x = x)) +
    geom_histogram(bins = bins, fill = fill_color, alpha = 0.7, 
                   color = "white", size = 0.2) +
    labs(title = title, y = "Frequency") +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
    )
  
  return(ggplotly(p))
}

#' Create Box Plot
#'
#' Creates an interactive box plot for comparing distributions
#'
#' @param data A data.frame containing the data to plot
#' @param x Character string specifying the categorical variable
#' @param y Character string specifying the numeric variable
#' @param fill_color Fill color for boxes (default: "lightblue")
#' @param title Optional plot title (default: "Box Plot")
#'
#' @return A plotly interactive plot object
#' @export
create_box_plot <- function(data, x, y, fill_color = "lightblue", 
                           title = "Box Plot") {
  
  # Input validation
  validate_data_inputs(data, x, y)
  
  # Create box plot
  p <- ggplot(data, aes_string(x = x, y = y)) +
    geom_boxplot(fill = fill_color, alpha = 0.7, color = "darkblue") +
    labs(title = title) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(ggplotly(p))
}

#' Helper Function: Validate Data Inputs
#'
#' Internal function to validate common data input parameters
#'
#' @param data A data.frame to validate
#' @param x Character string for x variable
#' @param y Character string for y variable
#' @param additional Optional additional variable to check
#'
#' @return NULL (stops execution if validation fails)
#' @keywords internal
validate_data_inputs <- function(data, x, y, additional = NULL) {
  
  if (!is.data.frame(data)) {
    stop("Error: 'data' must be a data.frame")
  }
  
  if (nrow(data) == 0) {
    warning("Warning: Empty dataset provided")
    return(NULL)
  }
  
  if (!x %in% names(data)) {
    stop(paste("Error: Column", x, "not found in data"))
  }
  
  if (!y %in% names(data)) {
    stop(paste("Error: Column", y, "not found in data"))
  }
  
  if (!is.null(additional) && !additional %in% names(data)) {
    stop(paste("Error: Column", additional, "not found in data"))
  }
}

#' Apply Consistent Theme
#'
#' Applies consistent theme across all dashboard visualizations
#'
#' @param plot A ggplot object
#' @param title_size Title font size (default: 14)
#' @param axis_text_size Axis text size (default: 11)
#'
#' @return Modified ggplot object with applied theme
#' @export
apply_dashboard_theme <- function(plot, title_size = 14, axis_text_size = 11) {
  
  plot +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, size = title_size, face = "bold"),
      axis.text = element_text(size = axis_text_size),
      axis.title = element_text(size = axis_text_size + 1),
      legend.position = "bottom",
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "white", color = NA),
      panel.background = element_rect(fill = "white", color = NA)
    )
}

#' Generate Color Palette
#'
#' Generates consistent color palettes for dashboard visualizations
#'
#' @param n Number of colors needed
#' @param palette_type Type of palette ("default", "viridis", "plasma")
#'
#' @return Vector of color codes
#' @export
generate_color_palette <- function(n, palette_type = "default") {
  
  if (palette_type == "viridis") {
    return(viridis::viridis(n))
  } else if (palette_type == "plasma") {
    return(viridis::plasma(n))
  } else {
    # Default dashboard colors
    default_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", 
                       "#6A994E", "#8E44AD", "#E67E22", "#16A085")
    
    if (n <= length(default_colors)) {
      return(default_colors[1:n])
    } else {
      return(rep(default_colors, ceiling(n/length(default_colors)))[1:n])
    }
  }
}

# =============================================================================
# END OF VISUALIZATION FUNCTIONS
# =============================================================================
