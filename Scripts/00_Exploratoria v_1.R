# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - https://github.com/FranVasEstay/Wildfire_Valparaiso2024.git]
# =============================================================================

# -------- EXPLORATORIA DE DATOS --------- #

# LIBRERÍAS
library(readr)
library(dplyr)
library(tidyverse)
library(lubridate)
library(ggplot2)

# DATOS 
## 2023
AtencionesUrgencia2023 <- read_delim("Datos/AtencionesUrgencia2023.csv",
                                     delim = ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1"),
                                     col_names = TRUE) %>%
  filter(CodigoRegion == 5)

## 2024
AtencionesUrgencia2024 <- read_delim("Datos/AtencionesUrgencia2024.csv",
                                     delim = ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1"),
                                     col_names = TRUE) %>%
  filter(CodigoRegion == 5)

## 2025
AtencionesUrgencia2025 <- read_delim("Datos/AtencionesUrgencia2025.csv",
                                     delim = ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1"),
                                     col_names = TRUE) %>%
  filter(CodigoRegion == 5)

## Unión de bases de datos
urgencias_valpo <- bind_rows(AtencionesUrgencia2023, AtencionesUrgencia2024, AtencionesUrgencia2025)

## Convertir fechas a Date y filtros de fecha
urgencias_valpo <- urgencias_valpo %>% 
  mutate(fecha = dmy(fecha))

fecha_inicio <- ymd("2023-02-01")
fecha_fin    <- ymd("2025-02-28")

urgencias_valpo <- urgencias_valpo %>%
  filter(fecha >= fecha_inicio & fecha <= fecha_fin)  

range(urgencias_valpo$fecha)

## Filtro de duplicados
duplicados <- duplicated(urgencias_valpo)
n_duplicados <- sum(duplicados, na.rm = TRUE)
n_duplicados # No existen duplicados en la data

## Filtro de comunas Juan Fernández e Isla de Pascua
urgencias_valpo <- urgencias_valpo %>%
  filter(NombreComuna != "Isla de Pascua") %>%
  filter(NombreComuna != "Juan Fernández")

# Exploratoria
str(urgencias_valpo)
summary(urgencias_valpo)
colSums(is.na(urgencias_valpo)) # No hay NAs

## Establecimientos
table(urgencias_valpo$NEstablecimiento)
table(urgencias_valpo$GLOSATIPOESTABLECIMIENTO)

ggplot_establecimiento <- ggplot(urgencias_valpo)+
  geom_bar(aes(x = GLOSATIPOESTABLECIMIENTO, fill = GLOSATIPOESTABLECIMIENTO)) +
  labs(x = "Tipo de establecimiento", 
       y = "Nº observaciones") +  
  theme(axis.text.x = element_text(angle = 30))
ggplot_establecimiento

## Causas de urgencia
table(urgencias_valpo$GlosaCausa)
# Aquí hay que aplicar la clasificación CIE-10

## Edades
totales_edad <- urgencias_valpo %>%
  summarise(
    Menores_1   = sum(Menores_1, na.rm = TRUE),
    De_1_a_4    = sum(De_1_a_4, na.rm = TRUE),
    De_5_a_14   = sum(De_5_a_14, na.rm = TRUE),
    De_15_a_64  = sum(De_15_a_64, na.rm = TRUE),
    De_65_y_mas = sum(De_65_y_mas, na.rm = TRUE)
  ) %>%
  pivot_longer(everything(), names_to = "Rango_etario", values_to = "Total_atenciones")

print(totales_edad)

totales_edad <- totales_edad %>%
  mutate(porcentaje = Total_atenciones / sum(Total_atenciones) * 100)

print(totales_edad)

totales_vector <- colSums(urgencias_valpo[, c("Menores_1", "De_1_a_4", "De_5_a_14", "De_15_a_64", "De_65_y_mas")], na.rm = TRUE)
totales_df <- data.frame(Rango = names(totales_vector), Total = totales_vector)
print(totales_df)

ggplot(totales_edad, aes(x = Rango_etario, y = Total_atenciones, fill = Rango_etario)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::comma(Total_atenciones)), vjust = -0.3, size = 3) +
  labs(x = "Rango etario", 
       y = "Total de atenciones de urgencia",
       title = "Distribución de atenciones por grupo de edad",
       subtitle = "Región de Valparaíso, periodo febrero 2023 - febrero 2025") +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal() +
  theme(legend.position = "none")

## Comunas
table(urgencias_valpo$NombreComuna)
ggplot_comuna <- ggplot(urgencias_valpo)+
  geom_bar(aes(x = NombreComuna, fill = NombreComuna)) +
  labs(x = "Comunas", 
       y = "Nº observaciones") +  
  theme(axis.text.x = element_text(angle = 30))
ggplot_comuna

# Series diarias
## Totales
diario_total <- urgencias_valpo %>%
  group_by(fecha) %>%
  summarise(total_consultas = sum(Total, na.rm = TRUE)) %>%
  ungroup()

fecha_evento1 <- as.Date("2024-02-02")
fecha_evento2 <- as.Date("2024-02-03")

ggplot(diario_total, aes(x = fecha, y = total_consultas)) +
  geom_line(color = "steelblue", linewidth = 0.6) +
  geom_vline(xintercept = as.numeric(c(fecha_evento1, fecha_evento2)),
             linetype = "dashed", color = "red", alpha = 0.7) +
  labs(title = "Consultas de urgencia totales diarias",
       subtitle = "Región de Valparaíso, febrero 2023 – febrero 2025",
       y = "Número de consultas", x = "Fecha",
       caption = "Líneas rojas: 2 y 3 de febrero 2024 (mega incendio)") +
  scale_x_date(date_breaks = "2 months", date_labels = "%b %Y") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

## Series diarias por comuna
diario_comuna <- urgencias_valpo %>%
  group_by(fecha, NombreComuna) %>%
  summarise(total_consultas = sum(Total, na.rm = TRUE)) %>%
  ungroup()

ggplot(diario_comuna, aes(x = fecha, y = total_consultas)) +
  geom_line(color = "darkorange", linewidth = 0.3) +
  geom_vline(xintercept = as.numeric(c(fecha_evento1, fecha_evento2)),
             linetype = "dashed", color = "red", alpha = 0.5) +
  facet_wrap(~ NombreComuna, scales = "free_y", ncol = 4) +
  labs(title = "Consultas de urgencia diarias por comuna",
       subtitle = "Región de Valparaíso, febrero 2023 – febrero 2025",
       y = "Consultas", x = "Fecha",
       caption = "Líneas rojas: 2 y 3 de febrero 2024 (mega incendio)") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b\n%Y") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5),
        strip.text = element_text(face = "bold"))

# Hay que quitar Santo Domingo porque no tiene datos para el periodo completo. 
# Quizás hacer una agregación geográfica (interior, gran valparaíso, litoral central, etc)

# ----- SUBSET COMUNAS ESCOGIDAS ---- #
# Se va a trabajar mejor con algunas comunas seleccionadas: Valparaíso, Viña del mar, Quilpué, Villa Alemana, Limache, Placilla de Peñuelas.
selected_comunas <- c("Valparaíso", "Viña del Mar","Quilpué","Villa Alemana","Limache","Placilla de Peñuelas")
urgencias_subset <-
  select(urgencias_subset, Comunas in selected_comunas)

# GUARDAR DATOS FILTRADOS
dir.create("Datos")
save(urgencias_valpo, file = "Datos/Urgencias_valpo_limpio.RData") # toda la data
save(urgencias_subset)
