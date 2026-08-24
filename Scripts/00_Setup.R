# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - https://github.com/FranVasEstay/Urgencias_Incendios_Vina.git]
# =============================================================================

# -------- SET UP --------- #
# Clean workspace
rm(list = ls())

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
library(paletteer)
library(ggrepel)
library(scales)
library(flextable)
library(officer)
#library(conflict) 

## Definir Directorios 
dir_data   <- here("Datos")
dir_mapa   <- here("Datos", "Shapes")
dir_figs   <- here("Output", "Figuras")
dir_tabs   <- here("Output", "Tablas")
dir_modelos <- here("Output", "Modelos")

# Crear directorios si no existen
dirs_list <- c(dir_data, dir_mapa, dir_figs, dir_tabs, dir_modelos)
sapply(dirs_list, function(x) if(!dir.exists(x)) dir.create(x, recursive = TRUE))

# Configurar opciones
options(scipen = 999)   # Evitar notación científica
set.seed(444)          # Reproducibilidad
conflicted::conflict_prefer("filter", "dplyr")
conflicted::conflict_prefer("select","dplyr")
conflicted::conflict_prefer("lag", "stats")

# Tema personalizado de ggplot
tema_tesis <- theme_minimal(base_size = 12) +
  theme(panel.grid.minor = element_blank(),
        plot.caption = element_text(size = 8, color = "gray40"))

# Colores incendio
colores_incendio <- c("antes" = "gray60", "post" = "firebrick")