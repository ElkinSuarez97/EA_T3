# ============================================================
# 00_Config.R - Configuración base para todo el proyecto
# ============================================================

# Limpiar entorno
rm(list = ls())

# Lista de paquetes requeridos para el análisis completo
# instalar pacman
if(!require(pacman)) install.packages("pacman") ; require(pacman)

p_load(
  rio,          # Importar .dta
  here,         # Rutas relativas
  tidyverse,    # Manipulación y gráficas
  patchwork,    # Combinar gráficas en paneles
  stargazer,    # Tablas de regresión en LaTeX
  xtable,       # Tablas en LaTeX
  skimr,        # Estadísticas descriptivas rápidas
  writex,       # Exportar tablas a Excel
  ggplot2,      # Gráficas
  rdrobust,     # Para estimaciones RD y rdplot
  rddensity,    # Para test de no manipulación
  patchwork,
  dplyr,
  lmtest,
  sandwich,
  readr,
  haven,
  lpdensity,
  fixest,
  AER,
  ggrepel,
  Synth,
  did

)

# Definir rutas 
# Identificamos la ruta del script actual
script_path <- rstudioapi::getSourceEditorContext()$path
script_dir  <- dirname(script_path)

# Creamos carpeta stores (si no existe) en la raíz del proyecto
stores_path <- file.path(dirname(script_dir), "stores")
if (!dir.exists(stores_path)) {
  dir.create(stores_path, recursive = TRUE)
}

# Función de ayuda para construir rutas hacia stores
store_file <- function(filename) {
  file.path(stores_path, filename)
}