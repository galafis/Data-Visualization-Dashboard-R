# =============================================================================
# Global Configuration File for Data Visualization Dashboard
# Autor: Gabriel Demetrios Lafis
# Data: 2025-09-11
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
  new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
  if(length(new_packages)) {
    install.packages(new_packages, dependencies = TRUE)
  }
}

# Instalar pacotes ausentes
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
  scipen = 999,  # Evitar notação científica
  digits = 3,    # Número de dígitos
  warn = -1      # Suprimir warnings
)

# Configurações do Shiny
shinyOptions(
  plot.autocolor = TRUE,
  cache = "memory"
)

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
  colors = viridis_discrete()
)

# =============================================================================
# FUNÇÕES UTILITÁRIAS GLOBAIS
# =============================================================================

# Função para debug
debug_log <- function(message) {
  if (getOption("dashboard.debug", FALSE)) {
    cat("[DEBUG]", Sys.time(), ":", message, "\n")
  }
}

# Função para validar dados
validate_data <- function(data) {
  if (is.null(data) || nrow(data) == 0) {
    return(FALSE)
  }
  return(TRUE)
}

# Função para formatar números
format_number <- function(x, digits = 2) {
  if (is.numeric(x)) {
    return(format(round(x, digits), big.mark = ".", decimal.mark = ","))
  }
  return(x)
}

# Função para formatar percentuais
format_percent <- function(x, digits = 1) {
  if (is.numeric(x)) {
    return(paste0(format(round(x * 100, digits), decimal.mark = ","), "%"))
  }
  return(x)
}

# Função para gerar cores seguras
safe_colors <- function(n) {
  if (n <= 8) {
    return(RColorBrewer::brewer.pal(max(3, n), "Set2")[1:n])
  } else {
    return(viridis::viridis(n))
  }
}

# =============================================================================
# CARREGAMENTO DE MÓDULOS
# =============================================================================

# Carregar módulos se existirem
module_files <- list.files(
  path = "modules", 
  pattern = "\\.(R|r)$", 
  full.names = TRUE
)

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

# =============================================================================
# CARREGAMENTO DE FUNÇÕES AUXILIARES
# =============================================================================

# Carregar funções do diretório R se existir
if (dir.exists("../R")) {
  r_files <- list.files(
    path = "../R", 
    pattern = "\\.(R|r)$", 
    full.names = TRUE
  )
  
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

# Função para gerar dados de exemplo
generate_sample_data <- function(n = 100) {
  set.seed(42)
  
  data.frame(
    id = 1:n,
    date = seq.Date(from = as.Date("2024-01-01"), 
                    to = as.Date("2024-12-31"), 
                    length.out = n),
    category = sample(c("A", "B", "C", "D"), n, replace = TRUE),
    value = round(rnorm(n, mean = 100, sd = 20), 2),
    metric1 = round(runif(n, 0, 100), 2),
    metric2 = round(rexp(n, rate = 0.1), 2),
    region = sample(c("Norte", "Sul", "Leste", "Oeste"), n, replace = TRUE),
    status = sample(c("Ativo", "Inativo", "Pendente"), n, replace = TRUE,
                   prob = c(0.6, 0.2, 0.2)),
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

# Mensagem de inicialização
debug_log("Global.R carregado com sucesso")
debug_log(paste("Dashboard version:", DASHBOARD_CONFIG$version))

# Definir encoding
Sys.setlocale("LC_TIME", "Portuguese_Brazil.1252")

cat("\n=== Data Visualization Dashboard ===\n")
cat("Versão:", DASHBOARD_CONFIG$version, "\n")
cat("Autor:", DASHBOARD_CONFIG$author, "\n")
cat("Carregado em:", format(Sys.time(), "%d/%m/%Y %H:%M:%S"), "\n")
cat("====================================\n\n")
