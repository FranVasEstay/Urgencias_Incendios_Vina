# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - Enlace a tu repositorio público]
# =============================================================================

# -------- SET UP --------- #
# Clean workspace
# rm(list = ls())

# LIBRERÍAS
library(tidyverse)   # Manipulación y gráficos
library(lubridate)   # Manejo de fechas
library(MASS)        # Regresión binomial negativa (glm.nb)
library(sandwich)    # Errores robustos Newey-West
library(lmtest)      # Tests de coeficientes con errores robustos
library(mgcv)        # GAM para imputación de valores faltantes
library(ggplot2)     # Gráficos avanzados
library(forecast)    # Diagnóstico de autocorrelación
library(dlnm)        # Para explorar interacciones meteorológicas
library(readr)
library(dplyr)
library(sf)
library(here)
library(janitor)
library(httr)      # Para hacer la solicitud HTTP
library(jsonlite)  # Para procesar la respuesta JSON de la API
library(ggspatial)

## Definir Directorios 
dir_data   <- here("Datos")
dir_mapa   <- here("Datos", "Shapes")
dir_figs   <- here("Output", "Figuras")
dir_tabs   <- here("Output", "Tablas")
dir_modelos <- here("Output", "Modelos")

# Create directories if they don't exist
dirs_list <- c(dir_data, dir_mapa, dir_figs, dir_tabs, dir_modelos)
sapply(dirs_list, function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

# Configurar opciones
options(scipen = 999)   # Evitar notación científica
set.seed(444)          # Reproducibilidad
