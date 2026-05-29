# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - Enlace a tu repositorio público]
# =============================================================================

# EXPLORATORIA DE DATOS

# LIBRERÍAS
library(readr)


# DATOS 
## 2023
AtencionesUrgencia2023 <- read_delim("Datos/AtencionesUrgencia2024.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

## 2024
AtencionesUrgencia2024 <- read_delim("Datos/AtencionesUrgencia2024.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

colnames(AtencionesUrgencia2024)
unique(AtencionesUrgencia2024$NombreRegion)

# "De Valpara\xedso"

## 2025
AtencionesUrgencia2025 <-