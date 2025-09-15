#' Statistical Analysis Module
#'
#' This module provides comprehensive statistical analysis functions
#' for the Data Visualization Dashboard.
#'
#' @author galafis
#' @version 1.1
#' @date 2025-09-15
#' @keywords statistics inference correlation regression time-series anova
#' @family analytics
#' @encoding UTF-8
# Load required libraries
suppressPackageStartupMessages({
  library(dplyr)
  library(broom)
  library(corrplot)
  library(forecast)
})

#' Calculate Descriptive Statistics
#'
#' Computes comprehensive descriptive statistics for numeric variables.
#'
#' @param data A data frame containing numeric variables
#' @param group_var Optional grouping variable name
#' @return A data frame with descriptive statistics
#' @examples
#' data <- data.frame(value = rnorm(100), category = rep(c("A", "B"), 50))
#' stats <- calculate_descriptive_stats(data)
#' grouped_stats <- calculate_descriptive_stats(data, "category")
#' @export
calculate_descriptive_stats <- function(data, group_var = NULL) {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  numeric_cols <- sapply(data, is.numeric)
  if (!any(numeric_cols)) stop("No numeric columns found in data")

  if (!is.null(group_var)) {
    if (!group_var %in% names(data)) stop(sprintf("Group variable %s not found in data", group_var))
    result <- data %>%
      group_by(!!rlang::sym(group_var)) %>%
      summarise(
        across(where(is.numeric), list(
          count = ~sum(!is.na(.x)),
          mean = ~mean(.x, na.rm = TRUE),
          median = ~median(.x, na.rm = TRUE),
          sd = ~sd(.x, na.rm = TRUE),
          min = ~min(.x, na.rm = TRUE),
          max = ~max(.x, na.rm = TRUE),
          q25 = ~quantile(.x, 0.25, na.rm = TRUE),
          q75 = ~quantile(.x, 0.75, na.rm = TRUE)
        )),
        .groups = "drop"
      )
  } else {
    numeric_data <- data[numeric_cols]
    result <- data.frame(
      variable = names(numeric_data),
      count = sapply(numeric_data, function(x) sum(!is.na(x))),
      mean = sapply(numeric_data, mean, na.rm = TRUE),
      median = sapply(numeric_data, median, na.rm = TRUE),
      sd = sapply(numeric_data, sd, na.rm = TRUE),
      min = sapply(numeric_data, min, na.rm = TRUE),
      max = sapply(numeric_data, max, na.rm = TRUE),
      q25 = sapply(numeric_data, quantile, 0.25, na.rm = TRUE),
      q75 = sapply(numeric_data, quantile, 0.75, na.rm = TRUE),
      row.names = NULL
    )
  }
  result
}

#' Perform Correlation Analysis
#'
#' Calculates correlation matrix and significance tests.
#'
#' @param data A data frame with numeric variables
#' @param method Correlation method ("pearson", "spearman", "kendall")
#' @param p_adjust Method for p-value adjustment
#' @return A list containing correlation matrix and p-values
#' @export
perform_correlation_analysis <- function(data, method = "pearson", p_adjust = "holm") {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  numeric_data <- data[sapply(data, is.numeric)]
  if (ncol(numeric_data) < 2) stop("Need at least 2 numeric variables for correlation analysis")

  cor_matrix <- stats::cor(numeric_data, use = "complete.obs", method = method)
  p_values <- cor_p_matrix(numeric_data, method = method)
  p_adj <- matrix(p.adjust(p_values, method = p_adjust), nrow = nrow(p_values), dimnames = dimnames(p_values))

  list(
    correlation = cor_matrix,
    p_values = p_values,
    p_adjusted = p_adj,
    method = method,
    n_obs = nrow(numeric_data)
  )
}

#' Compute correlation p-value matrix
#' @keywords internal
cor_p_matrix <- function(data, method = "pearson") {
  n <- ncol(data)
  p_matrix <- matrix(NA_real_, n, n, dimnames = list(colnames(data), colnames(data)))
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i == j) {
        p_matrix[i, j] <- 0
      } else {
        p_matrix[i, j] <- suppressWarnings(cor.test(data[[i]], data[[j]], method = method)$p.value)
      }
    }
  }
  p_matrix
}

#' Perform t-test Analysis
#'
#' Conducts one-sample, two-sample, or paired t-tests.
#'
#' @param data A data frame
#' @param value_var Name of the value variable
#' @param group_var Name of the grouping variable (for two-sample test)
#' @param mu Null hypothesis mean (for one-sample test)
#' @param paired Logical, whether to perform paired t-test
#' @param alternative Alternative hypothesis ("two.sided", "less", "greater")
#' @return A tidy data frame with test results
#' @export
perform_t_test <- function(data, value_var, group_var = NULL, mu = 0, paired = FALSE, alternative = "two.sided") {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (!value_var %in% names(data)) stop(sprintf("Value variable %s not found in data", value_var))

  if (is.null(group_var)) {
    test_result <- t.test(data[[value_var]], mu = mu, alternative = alternative)
  } else {
    if (!group_var %in% names(data)) stop(sprintf("Group variable %s not found in data", group_var))
    groups <- unique(stats::na.omit(data[[group_var]]))
    if (length(groups) != 2) stop("Group variable must have exactly 2 levels for t-test")
    group1 <- data[data[[group_var]] == groups[1], value_var]
    group2 <- data[data[[group_var]] == groups[2], value_var]
    test_result <- t.test(group1, group2, paired = paired, alternative = alternative)
  }
  broom::tidy(test_result)
}

#' Perform ANOVA Analysis
#'
#' Conducts one-way ANOVA with optional post-hoc tests.
#'
#' @param data A data frame
#' @param value_var Name of the dependent variable
#' @param group_var Name of the grouping variable
#' @param post_hoc Logical, whether to perform post-hoc tests
#' @return A list containing ANOVA results and post-hoc tests
#' @export
perform_anova <- function(data, value_var, group_var, post_hoc = TRUE) {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (!value_var %in% names(data) || !group_var %in% names(data)) stop("Variables not found in data")

  formula_str <- paste(value_var, "~", group_var)
  anova_result <- aov(as.formula(formula_str), data = data)
  res <- list(
    anova = broom::tidy(anova_result),
    summary = summary(anova_result)
  )
  if (post_hoc) {
    tukey_result <- TukeyHSD(anova_result)
    res$tukey <- broom::tidy(tukey_result)
  }
  res
}

#' Perform Linear Regression Analysis
#'
#' Fits linear regression model with diagnostics.
#'
#' @param data A data frame
#' @param formula A formula object or string
#' @param diagnostics Logical, whether to include model diagnostics
#' @return A list containing model results and diagnostics
#' @export
perform_linear_regression <- function(data, formula, diagnostics = TRUE) {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (is.character(formula)) formula <- as.formula(formula)

  model <- lm(formula, data = data)
  result <- list(
    model = model,
    summary = summary(model),
    coefficients = broom::tidy(model),
    model_stats = broom::glance(model),
    residuals = broom::augment(model)
  )
  if (diagnostics) {
    result$diagnostics <- list(
      normality_test = shapiro.test(residuals(model)),
      durbin_watson = tryCatch(car::durbinWatsonTest(model), error = function(e) NA),
      vif = tryCatch(if (length(coef(model)) > 2) car::vif(model) else NA, error = function(e) NA)
    )
  }
  result
}

#' Calculate Growth Rate
#'
#' Calculates period-over-period growth rates and percent.
#'
#' @param data A data frame with time series data
#' @param value_col Name of the value column
#' @param time_col Name of the time column
#' @param periods Number of periods for growth calculation
#' @return A data frame with growth_rate and growth_rate_pct
#' @export
calculate_growth_rate <- function(data, value_col, time_col, periods = 1) {
  if (!is.data.frame(data)) stop("Input must be a data frame")
  if (!value_col %in% names(data) || !time_col %in% names(data)) stop("Specified columns not found in data")
  data <- data[order(data[[time_col]]), ]
  n <- nrow(data)
  gr <- rep(NA_real_, n)
  if (n > periods) {
    # Align numerator and denominator safely
    gr[(periods + 1):n] <- diff(data[[value_col]], lag = periods) / data[[value_col]][1:(n - periods)]
  }
  data$growth_rate <- gr
  data$growth_rate_pct <- gr * 100
  data
}

#' Perform Time Series Analysis
#'
#' Basic time series analysis with decomposition and ARIMA forecast.
#'
#' @param data A numeric vector or time series object
#' @param frequency Frequency of the time series
#' @param forecast_periods Number of periods to forecast
#' @return A list containing decomposition and forecast results
#' @export
perform_time_series_analysis <- function(data, frequency = 12, forecast_periods = 6) {
  if (!is.numeric(data)) stop("Data must be numeric")
  ts_data <- if (!is.ts(data)) ts(data, frequency = frequency) else data

  decomp <- NULL
  if (length(ts_data) > 2 * frequency) {
    decomp <- stats::decompose(ts_data)
  } else {
    warning("Not enough data points for decomposition")
  }
  forecast_result <- forecast::forecast(forecast::auto.arima(ts_data), h = forecast_periods)
  list(
    original_data = ts_data,
    decomposition = decomp,
    forecast = forecast_result,
    model_info = forecast_result$model
  )
}
