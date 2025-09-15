#' Data Processing Module
#'
#' This module contains functions for processing and transforming data
#' for the Data Visualization Dashboard.
#'
#' @author galafis
#' @version 1.1
#' @date 2025-09-15
#' @keywords data-processing cleaning validation summary
#' @seealso process_data, clean_column_names, validate_data_types, generate_data_summary
#' @family data-processing
#' @encoding UTF-8

# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(stringr)
})

#' Process Data Function
#'
#' Processes raw data by adding a processed flag and performing
#' basic data cleaning operations (trim strings, normalize blanks to NA,
#' drop fully-empty rows), and stamping processed_at.
#'
#' @param df A data.frame or tibble to be processed
#' @return A processed data.frame with additional columns
#' @examples
#' data <- data.frame(x = 1:3, y = c(" a ", "", NA))
#' process_data(data)
#'
#' @export
process_data <- function(df) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  if (nrow(df) == 0) {
    warning("Input data frame is empty")
    df$processed <- logical()
    df$processed_at <- as.POSIXct(character())
    return(df)
  }

  # Trim character columns and convert empty strings to NA
  df <- df %>% mutate(across(where(is.character), ~{
    x <- stringr::str_trim(.)
    x[x == ""] <- NA_character_
    x
  }))

  # Remove any completely empty rows (all NA)
  df <- df[rowSums(is.na(df)) < ncol(df), , drop = FALSE]

  # Add processed flag and timestamp
  df$processed <- TRUE
  df$processed_at <- Sys.time()

  df
}

#' Clean Column Names
#'
#' Standardizes column names by converting to lowercase,
#' replacing non-alphanumeric characters with underscores, collapsing
#' repeated underscores, and trimming leading/trailing underscores.
#'
#' @param df A data frame with columns to clean
#' @return A data frame with cleaned column names
#' @export
clean_column_names <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  new_names <- names(df) %>%
    tolower() %>%
    str_replace_all("[^a-z0-9_]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_replace_all("^_+|_+$", "")

  names(df) <- new_names
  df
}

#' Validate Data Types
#'
#' Validates and converts data types based on an expected schema.
#'
#' @param df A data frame to validate
#' @param schema A named list mapping column -> target type
#'   Supported types: "numeric", "integer", "double", "character",
#'   "factor", "logical", "Date", "POSIXct"
#' @return A data frame with coerced types where possible
#' @export
validate_data_types <- function(df, schema = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  if (is.null(schema)) {
    message("No schema provided, returning input unchanged")
    return(df)
  }

  for (col_name in names(schema)) {
    if (col_name %in% names(df)) {
      target_type <- schema[[col_name]]
      df[[col_name]] <- tryCatch({
        switch(target_type,
               numeric = as.numeric(df[[col_name]]),
               integer = as.integer(df[[col_name]]),
               double  = as.double(df[[col_name]]),
               character = as.character(df[[col_name]]),
               factor = as.factor(df[[col_name]]),
               logical = as.logical(df[[col_name]]),
               Date = as.Date(df[[col_name]]),
               POSIXct = as.POSIXct(df[[col_name]]),
               { warning(sprintf("Unsupported target type '%s' for %s", target_type, col_name)); df[[col_name]] }
        )
      }, error = function(e) {
        warning(sprintf("Could not convert column %s to %s: %s", col_name, target_type, e$message))
        df[[col_name]]
      })
    } else {
      warning(sprintf("Column %s not found in data frame", col_name))
    }
  }

  df
}

#' Generate Data Summary
#'
#' Creates a comprehensive summary of the dataset with shape, types,
#' missingness, unique counts, and memory usage.
#'
#' @param df A data frame to summarize
#' @return A list containing summary statistics
#' @export
generate_data_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }

  summary_info <- list(
    nrows = nrow(df),
    ncols = ncol(df),
    column_names = names(df),
    column_types = sapply(df, function(x) paste(class(x), collapse = ",")),
    missing_values = sapply(df, function(x) sum(is.na(x))),
    missing_ratio = sapply(df, function(x) mean(is.na(x))),
    unique_values = sapply(df, function(x) length(unique(x))),
    memory_usage = as.numeric(object.size(df)),
    memory_units = "bytes"
  )

  summary_info
}

#' Ensure Required Columns
#'
#' Checks presence of required columns and optionally adds them with NA
#' when absent.
#'
#' @param df data.frame
#' @param required character vector of required column names
#' @param add_missing if TRUE, missing columns are added filled with NA
#' @return data.frame (possibly augmented) and emits warnings for missing
#' @export
ensure_required_columns <- function(df, required, add_missing = TRUE) {
  if (!is.data.frame(df)) stop("df must be a data frame")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    warning(sprintf("Missing required columns: %s", paste(missing, collapse = ", ")))
    if (isTRUE(add_missing)) {
      for (m in missing) df[[m]] <- NA
    }
  }
  df
}
