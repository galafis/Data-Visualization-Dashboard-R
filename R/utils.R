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
#' @return NULL (invisible)
#' @export
log_msg <- function(level = "INFO", ...) {
  level <- toupper(level)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  cat(sprintf("[%s] %s: %s\n", level, ts, paste(..., collapse = " ")))
  invisible(NULL)
}

#' Null-coalescing operator
#'
#' Returns y when x is NULL, otherwise x.
#'
#' @param x any
#' @param y fallback value
#' @return value
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x
