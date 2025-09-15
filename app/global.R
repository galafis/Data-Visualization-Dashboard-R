# =============================================================================
# Global Configuration File for Data Visualization Dashboard
# Autor: Gabriel Demetrios Lafis
# Data: 2025-09-11
# Última atualização: 2025-09-15
# =============================================================================

# =============================================================================
# BIBLIOTECAS E DEPENDÊNCIAS
# =============================================================================
# Verificar e instalar pacotes necessários
required_packages <- c(
  # Core Shiny
  "shiny", "shinydashboard", "shinyWidgets", "shinyjs", "shinycssloaders",
  
  # Visualização
  "ggplot2", "plotly", "DT", "leaflet", "visNetwork", "corrplot",
  
  # Manipulação de dados
  "dplyr", "tidyr", "data.table", "readr", "readxl",
  
  # Análise estatística
  "forecast", "caret", "cluster", "factoextra",
  
  # Relatórios
  "rmarkdown", "knitr", "flexdashboard",
  
  # Utilitários
  "lubridate", "stringr", "scales", "RColorBrewer", "viridis"
)

# Função para instalar pacotes ausentes
install_if_missing <- function(packages) {
  new_packages <- packages[!(packages %in% installed.packages()[, "Package"])]
  if (length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Instalar pacotes ausentes (silencioso em produção)
install_if_missing(required_packages)

# Carregar bibliotecas
suppressPackageStartupMessages({
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(shinycssloaders)
  library(ggplot2)
  library(plotly)
  library(DT)
  library(dplyr)
  library(tidyr)
  library(data.table)
  library(readr)
  library(readxl)
  library(lubridate)
  library(stringr)
  library(scales)
  library(RColorBrewer)
  library(viridis)
})

# =============================================================================
# CONFIGURAÇÕES GLOBAIS
# =============================================================================
# Opções globais do R
options(
  shiny.maxRequestSize = 30*1024^2,  # 30MB max upload
  scipen = 999,                      # Evitar notação científica
  digits = 3,                        # Número de dígitos
  warn = -1                          # Suprimir warnings (ajuste em dev)
)

# Configurações do Shiny
shinyOptions(
  plot.autocolor = TRUE,
  cache = "memory"
)

# Toggle de modo debug via variável de ambiente
options(dashboard.debug = identical(Sys.getenv("DASHBOARD_DEBUG"), "1"))

# =============================================================================
# VARIÁVEIS E CONSTANTES GLOBAIS
# =============================================================================
# Paleta de cores padrão
COLOR_PALETTE <- list(
  primary = "#3498db",
  secondary = "#2ecc71",
  warning = "#f39c12",
  danger = "#e74c3c",
  info = "#17a2b8",
  light = "#f8f9fa",
  dark = "#343a40",
  success = "#28a745"
)

# Configurações do dashboard
DASHBOARD_CONFIG <- list(
  title = "Data Visualization Dashboard",
  version = "1.0.0",
  author = "Gabriel Demetrios Lafis",
  theme = "blue",
  skin = "blue"
)

# Formatos de data padrão
DATE_FORMATS <- list(
  display = "%d/%m/%Y",
  input = "%Y-%m-%d",
  timestamp = "%Y-%m-%d %H:%M:%S"
)

# Configurações de gráficos
PLOT_CONFIG <- list(
  theme = theme_minimal(),
  height = 400,
  dpi = 96,
  font_size = 12,
  colors = viridis::viridis
)

# =============================================================================
# FUNÇÕES UTILITÁRIAS GLOBAIS
# =============================================================================
#' Log de debug (condicional)
#' @param message Texto a ser logado
#' @return Invisível
#' @keywords internal
debug_log <- function(message) {
  if (getOption("dashboard.debug", FALSE)) {
    cat("[DEBUG]", format(Sys.time(), DATE_FORMATS$timestamp), ":", message, "\n")
  }
  invisible(NULL)
}

#' Validação genérica de data.frame
#' @param data Objeto a validar
#' @return TRUE/FALSE
#' @export
validate_data <- function(data) {
  is.data.frame(data) && nrow(data) > 0
}

#' Formatar número com separadores BR
#' @param x Vetor numérico
#' @param digits Casas decimais
#' @return Character
#' @export
format_number <- function(x, digits = 2) {
  if (is.numeric(x)) {
    return(format(round(x, digits), big.mark = ".", decimal.mark = ","))
  }
  as.character(x)
}

#' Formatar percentual
#' @param x Vetor numérico (0-1)
#' @param digits Casas decimais
#' @return Character com "%"
#' @export
format_percent <- function(x, digits = 1) {
  if (is.numeric(x)) {
    return(paste0(format(round(x * 100, digits), decimal.mark = ","), "%"))
  }
  as.character(x)
}

#' Cores seguras (até n)
#' @param n Inteiro de cores
#' @return Vetor de cores hex
#' @export
safe_colors <- function(n) {
  if (n <= 8) {
    return(RColorBrewer::brewer.pal(max(3, min(8, n)), "Set2")[1:n])
  }
  viridis::viridis(n)
}

#' Wrapper seguro para leitura de CSV
#' @param path Caminho do arquivo
#' @param ... Args para readr::read_csv
#' @return tibble/data.frame
#' @export
read_csv_safe <- function(path, ...) {
  stopifnot(file.exists(path))
  suppressWarnings(readr::read_csv(path, show_col_types = FALSE, ...))
}

#' Aplicar tema padrão a um ggplot
#' @param p ggplot
#' @return ggplot
#' @export
apply_theme <- function(p) {
  p + theme_minimal(base_size = PLOT_CONFIG$font_size) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.position = "bottom"
    )
}

# =============================================================================
# CARREGAMENTO DE MÓDULOS
# =============================================================================
# Carregar módulos do diretório app/modules
modules_path <- "modules"
if (dir.exists(modules_path)) {
  module_files <- list.files(path = modules_path, pattern = "\\.(R|r)$", full.names = TRUE)
  if (length(module_files) > 0) {
    for (file in module_files) {
      tryCatch({
        source(file, local = TRUE)
        debug_log(paste("Módulo carregado:", basename(file)))
      }, error = function(e) {
        warning(paste("Erro ao carregar módulo", basename(file), ":", e$message))
      })
    }
  }
}

# =============================================================================
# CARREGAMENTO DE FUNÇÕES AUXILIARES (diretório R)
# =============================================================================
if (dir.exists("../R")) {
  r_files <- list.files(path = "../R", pattern = "\\.(R|r)$", full.names = TRUE)
  for (file in r_files) {
    tryCatch({
      source(file, local = TRUE)
      debug_log(paste("Função carregada:", basename(file)))
    }, error = function(e) {
      warning(paste("Erro ao carregar função", basename(file), ":", e$message))
    })
  }
}

# =============================================================================
# DADOS DE EXEMPLO
# =============================================================================
#' Gerar dados de exemplo para o dashboard
#' @param n Número de linhas
#' @return data.frame de exemplo
#' @export
generate_sample_data <- function(n = 100) {
  set.seed(42)
  data.frame(
    id = 1:n,
    date = seq.Date(from = as.Date("2024-01-01"), to = as.Date("2024-12-31"), length.out = n),
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    value = round(rnorm(n, mean = 100, sd = 20), 2),
    metric1 = round(runif(n, 0, 100), 2),
    metric2 = round(rexp(n, rate = 0.1), 2),
    region = sample(c("Norte", "Sul", "Leste", "Oeste"), n, replace = TRUE),
    status = sample(c("Ativo", "Inativo", "Pendente"), n, replace = TRUE, prob = c(0.6, 0.2, 0.2)),
    latitude = runif(n, -30, -20),
    longitude = runif(n, -50, -40)
  )
}

# Carregamento inicial de dados
if (!exists("sample_data")) {
  sample_data <- generate_sample_data()
  debug_log("Dados de exemplo gerados")
}

# =============================================================================
# CONFIGURAÇÕES FINAIS
# =============================================================================
# Mensagens de inicialização
Sys.setlocale("LC_TIME", "Portuguese_Brazil.1252")
startup_banner <- paste0(
  "\n=== Data Visualization Dashboard ===\n",
  "Versão: ", DASHBOARD_CONFIG$version, "\n",
  "Autor: ", DASHBOARD_CONFIG$author, "\n",
  "Carregado em: ", format(Sys.time(), DATE_FORMATS$timestamp), "\n",
  "====================================\n\n"
)
cat(startup_banner)

debug_log("global.R carregado com sucesso")
debug_log(paste("Dashboard version:", DASHBOARD_CONFIG$version))
