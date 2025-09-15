#' Utility Functions Module
#'
#' Lightweight helpers used across the dashboard.
#'
#' @author galafis
#' @version 1.0
#' @date 2025-09-15
#' @encoding UTF-8
#' @family utilities
#' @keywords utilities helpers logging safety

library(ggplot2)
library(dplyr)
library(shiny)
library(plotly)
library(DT)

#' Safely load a set of libraries, suppressing startup messages.
#'
#' @param pkgs character vector of package names
#' @param quietly logical; if TRUE, suppress messages
#' @return invisibly returns character vector of loaded packages
#' @examples
#' load_libraries(c("ggplot2", "dplyr"))
#' @export
load_libraries <- function(pkgs = c("ggplot2", "dplyr"), quietly = TRUE) {
  stopifnot(is.character(pkgs))
  loaded <- character()
  
  for (p in pkgs) {
    suppressPackageStartupMessages({
      ok <- require(p, character.only = TRUE, quietly = quietly)
      if (!ok) warning(sprintf("Package '%s' is not installed", p))
      else loaded <- c(loaded, p)
    })
  }
  invisible(loaded)
}

#' Simple logger with levels
#'
#' @param level one of "INFO","WARN","ERROR","DEBUG"
#' @param ... message parts to paste
#' @return NULL (prints to console)
#' @examples
#' log_message("INFO", "Data loaded successfully")
#' log_message("ERROR", "Failed to process file:", filename)
#' @export
log_message <- function(level = "INFO", ...) {
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  message <- paste(..., collapse = " ")
  cat(sprintf("[%s] %s: %s\n", timestamp, level, message))
}

#' Validate data frame columns
#'
#' @param df data frame to validate
#' @param required_cols character vector of required column names
#' @param numeric_cols character vector of columns that should be numeric
#' @return logical indicating if validation passed
#' @examples
#' validate_dataframe(mtcars, c("mpg", "cyl"), c("mpg", "hp"))
#' @export
validate_dataframe <- function(df, required_cols = NULL, numeric_cols = NULL) {
  if (!is.data.frame(df)) {
    log_message("ERROR", "Input is not a data frame")
    return(FALSE)
  }
  
  if (!is.null(required_cols)) {
    missing_cols <- setdiff(required_cols, names(df))
    if (length(missing_cols) > 0) {
      log_message("ERROR", "Missing required columns:", paste(missing_cols, collapse = ", "))
      return(FALSE)
    }
  }
  
  if (!is.null(numeric_cols)) {
    for (col in numeric_cols) {
      if (col %in% names(df) && !is.numeric(df[[col]])) {
        log_message("ERROR", "Column", col, "should be numeric")
        return(FALSE)
      }
    }
  }
  
  log_message("INFO", "Data validation passed")
  return(TRUE)
}

#' Safe file reading with error handling
#'
#' @param file_path path to the file
#' @param file_type type of file ("csv", "xlsx", "rds")
#' @param ... additional arguments passed to read functions
#' @return data frame or NULL if error
#' @examples
#' data <- safe_read_file("data.csv", "csv")
#' data <- safe_read_file("data.xlsx", "xlsx", sheet = 1)
#' @export
safe_read_file <- function(file_path, file_type = "csv", ...) {
  if (!file.exists(file_path)) {
    log_message("ERROR", "File does not exist:", file_path)
    return(NULL)
  }
  
  tryCatch({
    result <- switch(tolower(file_type),
      "csv" = read.csv(file_path, ...),
      "xlsx" = {
        if (!requireNamespace("readxl", quietly = TRUE)) {
          log_message("ERROR", "readxl package required for Excel files")
          return(NULL)
        }
        readxl::read_excel(file_path, ...)
      },
      "rds" = readRDS(file_path),
      {
        log_message("ERROR", "Unsupported file type:", file_type)
        return(NULL)
      }
    )
    
    log_message("INFO", "Successfully loaded file:", file_path)
    return(result)
  }, error = function(e) {
    log_message("ERROR", "Failed to read file:", file_path, "-", e$message)
    return(NULL)
  })
}

#' Format numbers for display
#'
#' @param x numeric vector
#' @param decimals number of decimal places
#' @param big_mark character for thousands separator
#' @return character vector of formatted numbers
#' @examples
#' format_number(1234.567, decimals = 2)
#' format_number(c(1000, 2000, 3000), big_mark = ",")
#' @export
format_number <- function(x, decimals = 0, big_mark = ",") {
  if (!is.numeric(x)) {
    warning("Input is not numeric")
    return(as.character(x))
  }
  
  format(round(x, decimals), 
         big.mark = big_mark, 
         nsmall = decimals, 
         scientific = FALSE)
}

#' Create consistent theme for ggplot2
#'
#' @param base_size base font size
#' @param base_family base font family
#' @return ggplot2 theme object
#' @examples
#' ggplot(mtcars, aes(mpg, hp)) + geom_point() + dashboard_theme()
#' @export
dashboard_theme <- function(base_size = 12, base_family = "sans") {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = base_size * 1.2, face = "bold", hjust = 0.5),
      plot.subtitle = ggplot2::element_text(size = base_size * 0.9, hjust = 0.5),
      axis.title = ggplot2::element_text(size = base_size * 0.9),
      axis.text = ggplot2::element_text(size = base_size * 0.8),
      legend.title = ggplot2::element_text(size = base_size * 0.9, face = "bold"),
      legend.text = ggplot2::element_text(size = base_size * 0.8),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_rect(fill = NA, color = "grey80"),
      strip.background = ggplot2::element_rect(fill = "grey90", color = "grey80"),
      strip.text = ggplot2::element_text(face = "bold")
    )
}

#' Create color palette for dashboard
#'
#' @param n number of colors needed
#' @param type type of palette ("qualitative", "sequential", "diverging")
#' @return character vector of hex colors
#' @examples
#' colors <- dashboard_colors(5, "qualitative")
#' colors <- dashboard_colors(10, "sequential")
#' @export
dashboard_colors <- function(n = 5, type = "qualitative") {
  switch(type,
    "qualitative" = {
      base_colors <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")
      if (n <= length(base_colors)) {
        return(base_colors[1:n])
      } else {
        return(colorRampPalette(base_colors)(n))
      }
    },
    "sequential" = {
      colorRampPalette(c("#F7F9FC", "#2E86AB"))(n)
    },
    "diverging" = {
      colorRampPalette(c("#C73E1D", "#FFFFFF", "#2E86AB"))(n)
    },
    {
      log_message("WARN", "Unknown palette type, using qualitative")
      dashboard_colors(n, "qualitative")
    }
  )
}

#' Convert data frame to DT datatable with consistent styling
#'
#' @param df data frame to display
#' @param pageLength number of rows per page
#' @param scrollX enable horizontal scrolling
#' @param ... additional arguments passed to datatable
#' @return DT datatable object
#' @examples
#' create_datatable(mtcars)
#' create_datatable(iris, pageLength = 20)
#' @export
create_datatable <- function(df, pageLength = 10, scrollX = TRUE, ...) {
  if (!requireNamespace("DT", quietly = TRUE)) {
    log_message("ERROR", "DT package required for datatables")
    return(NULL)
  }
  
  DT::datatable(
    df,
    options = list(
      pageLength = pageLength,
      scrollX = scrollX,
      dom = 'Bfrtip',
      buttons = c('copy', 'csv', 'excel'),
      language = list(
        search = "Buscar:",
        lengthMenu = "Mostrar _MENU_ registros",
        info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
        paginate = list(
          first = "Primeiro",
          last = "Último",
          next = "Próximo",
          previous = "Anterior"
        )
      )
    ),
    extensions = 'Buttons',
    ...
  ) %>%
    DT::formatStyle(columns = names(df), fontSize = '12px')
}

#' Error handler for Shiny applications
#'
#' @param expr expression to evaluate
#' @param error_message custom error message
#' @return result of expr or error handling
#' @examples
#' result <- handle_errors({
#'   # some risky operation
#'   read.csv("nonexistent.csv")
#' }, "Failed to load data")
#' @export
handle_errors <- function(expr, error_message = "An error occurred") {
  tryCatch({
    eval(expr)
  }, error = function(e) {
    log_message("ERROR", error_message, ":", e$message)
    if (exists("showNotification", mode = "function")) {
      showNotification(
        paste(error_message, "Verifique os logs para mais detalhes."),
        type = "error",
        duration = 5
      )
    }
    return(NULL)
  }, warning = function(w) {
    log_message("WARN", w$message)
    if (exists("showNotification", mode = "function")) {
      showNotification(
        paste("Aviso:", w$message),
        type = "warning",
        duration = 3
      )
    }
  })
}

#' Check if all required packages are installed
#'
#' @param packages character vector of package names
#' @return logical indicating if all packages are available
#' @examples
#' check_packages(c("ggplot2", "dplyr", "shiny"))
#' @export
check_packages <- function(packages) {
  missing <- setdiff(packages, rownames(installed.packages()))
  
  if (length(missing) > 0) {
    log_message("ERROR", "Missing packages:", paste(missing, collapse = ", "))
    log_message("INFO", "Install with: install.packages(c('" + 
                paste(missing, collapse = "', '") + "'))")
    return(FALSE)
  }
  
  log_message("INFO", "All required packages are installed")
  return(TRUE)
}

#' Initialize dashboard environment
#'
#' @return logical indicating successful initialization
#' @examples
#' init_dashboard()
#' @export
init_dashboard <- function() {
  required_packages <- c(
    "shiny", "shinydashboard", "DT", "plotly", 
    "ggplot2", "dplyr", "readr", "readxl"
  )
  
  if (!check_packages(required_packages)) {
    return(FALSE)
  }
  
  load_libraries(required_packages, quietly = TRUE)
  log_message("INFO", "Dashboard environment initialized successfully")
  return(TRUE)
}
