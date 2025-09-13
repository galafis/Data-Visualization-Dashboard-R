#' Data Processing Module
#' 
#' This module contains functions for processing and transforming data
#' for the Data Visualization Dashboard.
#' 
#' @author galafis
#' @version 1.0
#' @date 2025-09-13

# Load required libraries
library(dplyr)
library(tidyr)
library(stringr)

#' Process Data Function
#' 
#' Processes raw data by adding a processed flag and performing
#' basic data cleaning operations.
#' 
#' @param df A data frame to be processed
#' @return A processed data frame with additional processed column
#' @examples
#' data <- data.frame(x = 1:10, y = letters[1:10])
#' processed_data <- process_data(data)
#' 
#' @export
process_data <- function(df) {
  # Input validation
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  if (nrow(df) == 0) {
    warning("Input data frame is empty")
    return(df)
  }
  
  # Add processed flag
  df$processed <- TRUE
  
  # Add processing timestamp
  df$processed_at <- Sys.time()
  
  # Remove any completely empty rows
  df <- df[!apply(is.na(df) | df == "", 1, all), ]
  
  return(df)
}

#' Clean Column Names
#' 
#' Standardizes column names by converting to lowercase,
#' replacing spaces with underscores, and removing special characters.
#' 
#' @param df A data frame with columns to clean
#' @return A data frame with cleaned column names
#' @export
clean_column_names <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Clean column names
  names(df) <- names(df) %>%
    tolower() %>%
    str_replace_all("[^a-zA-Z0-9_]", "_") %>%
    str_replace_all("_{2,}", "_") %>%
    str_remove_all("^_|_$")
  
  return(df)
}

#' Validate Data Types
#' 
#' Validates and converts data types based on expected schema.
#' 
#' @param df A data frame to validate
#' @param schema A named list specifying expected data types
#' @return A data frame with corrected data types
#' @export
validate_data_types <- function(df, schema = NULL) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  if (is.null(schema)) {
    message("No schema provided, performing basic type inference")
    return(df)
  }
  
  # Apply schema transformations
  for (col_name in names(schema)) {
    if (col_name %in% names(df)) {
      target_type <- schema[[col_name]]
      
      tryCatch({
        if (target_type == "numeric") {
          df[[col_name]] <- as.numeric(df[[col_name]])
        } else if (target_type == "character") {
          df[[col_name]] <- as.character(df[[col_name]])
        } else if (target_type == "factor") {
          df[[col_name]] <- as.factor(df[[col_name]])
        } else if (target_type == "Date") {
          df[[col_name]] <- as.Date(df[[col_name]])
        }
      }, error = function(e) {
        warning(paste("Could not convert column", col_name, "to", target_type))
      })
    }
  }
  
  return(df)
}

#' Generate Data Summary
#' 
#' Creates a comprehensive summary of the dataset.
#' 
#' @param df A data frame to summarize
#' @return A list containing various summary statistics
#' @export
generate_data_summary <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  summary_info <- list(
    nrows = nrow(df),
    ncols = ncol(df),
    column_names = names(df),
    column_types = sapply(df, class),
    missing_values = sapply(df, function(x) sum(is.na(x))),
    unique_values = sapply(df, function(x) length(unique(x))),
    memory_usage = object.size(df)
  )
  
  return(summary_info)
}
