# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - Enlace a tu repositorio público]
# =============================================================================

# PREPARACIÓN DE LOS DATOS

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

# Configurar opciones
options(scipen = 999)   # Evitar notación científica
set.seed(1234)          # Reproducibilidad

# Carga y preparación de los datos
urgencias
mp25
meteo
feriados

#Unir las bases de datos