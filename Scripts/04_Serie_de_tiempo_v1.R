## Primer intento 

# Librerías necesarias
library(tidyverse)  # Manipulación de datos y fechas (dplyr, lubridate, ggplot2)
library(mgcv)       # Modelos GAM
library(MASS)       # Regresión Binomial Negativa (glm.nb) y mvrnorm
library(sandwich)   # Errores estándar robustos (NeweyWest)
library(lmtest)     # Inferencia estadística (coeftest)

## Preparación/completitud de la serie

# 1. CALENDARIO DE FERIADOS (Simulado)

# 2. VARIABLES TEMPORALES Y COMPLETITUD
# Comparamos el rango temporal con una secuencia completa
df_completo <- tibble(fecha = seq(min(datos_crudos$fecha), max(datos_crudos$fecha), by = "day")) %>%
  left_join(datos_crudos, by = "fecha") %>%
  mutate(
    # Variables temporales explícitas
    ano = year(fecha),
    mes = as.factor(month(fecha)),
    nombre_mes = month(fecha, label = TRUE, abbr = FALSE),
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1),
    # Índice temporal continuo (desde el inicio + 1)
    indice_temporal = as.numeric(fecha - min(fecha)) + 1,
    # Variable indicadora de feriado
    es_feriado = if_else(fecha %in% feriados_oficiales, 1, 0)
  )

## Tratamiento de ausencias y exclusiones
# 4. IMPUTACIÓN ESTRUCTURAL Y EXCLUSIÓN DE DÍAS NO REGULARES
df_filtrado <- df_completo %>%
  mutate(
    # Aplicar imputación por ceros según el criterio del Anexo
    conteos = case_when(
      tipo_centro == "hospitalario" & (dia_semana == "domingo" | es_feriado == 1) & is.na(conteos) ~ 0,
      tipo_centro == "ambulatorio" & es_feriado == 1 & is.na(conteos) ~ 0,
      TRUE ~ conteos
    )
  ) %>%
  # Exclusión de la serie según el tipo de centro
  filter(
    !(tipo_centro == "hospitalario" & (dia_semana == "domingo" | es_feriado == 1)),
    !(tipo_centro == "ambulatorio" & es_feriado == 1)
  )

## Imputación de valores faltantes remanentes (GAM)
# MODELADO GAM PARA IMPUTACIÓN
# Convertir día de la semana a factor para el efecto aleatorio
df_filtrado$dia_semana_fac <- as.factor(df_filtrado$dia_semana)

# Ajustar el modelo usando mgcv
modelo_gam <- gam(conteos ~ s(indice_temporal) + s(dia_semana_fac, bs = "re"), 
                  family = quasipoisson, 
                  data = df_filtrado)

# PREDICCIÓN E IMPUTACIÓN
df_imputado <- df_filtrado %>%
  mutate(
    pred_gam = predict(modelo_gam, newdata = ., type = "response"),
    # Restringir a no negativos y redondear al entero
    pred_gam_redondeada = round(pmax(pred_gam, 0)),
    # Reemplazar solo los NA
    conteos_final = if_else(is.na(conteos), pred_gam_redondeada, conteos)
  )

## Definición de evento (Series de tiempo interrumpidas)
# VARIABLES ITS
fecha_evento <- as.Date("2024-02-02")

df_its <- df_imputado %>%
  mutate(
    # Días desde el evento para calcular ventanas
    dias_desde_evento = as.numeric(fecha - fecha_evento),
    
    # Pulse: primeros 7 días post-evento (incluyendo el día del evento si aplica)
    # Rango [0, 6] días desde el evento equivale a 7 días de impacto inmediato
    pulse = if_else(dias_desde_evento >= 0 & dias_desde_evento <= 6, 1, 0),
    
    # Post: primeros 30 días post-evento (Rango [0, 29])
    post = if_else(dias_desde_evento >= 0 & dias_desde_evento <= 29, 1, 0)
  )

## Ajuste del modelo y errores robustos
# MODELO BINOMIAL NEGATIVO
modelo_its <- glm.nb(conteos_final ~ indice_temporal + pulse + post + dia_semana_fac + mes, 
                     data = df_its)

# ERRORES ESTÁNDAR ROBUSTOS DE NEWEY-WEST (Rezago de 7 días)
vcov_nw <- NeweyWest(modelo_its, lag = 7, prewhite = FALSE)

# Inferencia estadística basada en errores robustos
tabla_coeficientes <- coeftest(modelo_its, vcov. = vcov_nw)
print(tabla_coeficientes) # Aquí revisas los p-values robustos

## Generación de predicciones y contrafactual
# MATRICES DE DISEÑO PARA PREDICCIÓN
# Matriz Observada
X_obs <- model.matrix(modelo_its)

# Matriz Contrafactual (Forzamos pulse y post a 0)
X_cf <- X_obs
X_cf[, "pulse"] <- 0
X_cf[, "post"] <- 0

# Coeficientes estimados
coefs_est <- coef(modelo_its)

# Predicciones puntuales en la escala original (exponencial)
df_its <- df_its %>%
  mutate(
    pred_observada = exp(X_obs %*% coefs_est),
    pred_contrafactual = exp(X_cf %*% coefs_est)
  )

## Cuantificación del impacto mediante simulación paramétrica
# 11. SIMULACIÓN PARAMÉTRICA
n_sims <- 10000
simulaciones_coefs <- mvrnorm(n_sims, mu = coefs_est, Sigma = vcov_nw)

# Función auxiliar para calcular el impacto en una ventana específica
calcular_impacto <- function(datos, X_mat_obs, X_mat_cf, coefs_sim, dias_max) {
  
  # Filtrar índices correspondientes a la ventana de tiempo
  idx_ventana <- which(datos$dias_desde_evento >= 0 & datos$dias_desde_evento < dias_max)
  
  X_obs_vent <- X_mat_obs[idx_ventana, ]
  X_cf_vent <- X_mat_cf[idx_ventana, ]
  
  impactos_absolutos <- numeric(n_sims)
  impactos_relativos <- numeric(n_sims)
  
  for(i in 1:n_sims) {
    beta <- coefs_sim[i, ]
    y_obs_sim <- sum(exp(X_obs_vent %*% beta))
    y_cf_sim <- sum(exp(X_cf_vent %*% beta))
    
    impactos_absolutos[i] <- y_obs_sim - y_cf_sim
    impactos_relativos[i] <- (y_obs_sim - y_cf_sim) / y_cf_sim * 100
  }
  
  list(
    absoluto = quantile(impactos_absolutos, probs = c(0.025, 0.5, 0.975)),
    relativo = quantile(impactos_relativos, probs = c(0.025, 0.5, 0.975))
  )
}

# Impacto a 7 días y 30 días
impacto_7_dias <- calcular_impacto(df_its, X_obs, X_cf, simulaciones_coefs, 7)
impacto_30_dias <- calcular_impacto(df_its, X_obs, X_cf, simulaciones_coefs, 30)

cat("Impacto Relativo (%) a 7 días [IC 95%]:\n")
print(impacto_7_dias$relativo)

cat("\nImpacto Relativo (%) a 30 días [IC 95%]:\n")
print(impacto_30_dias$relativo)

## Visualización
# 12. GRÁFICO DE TRAYECTORIAS
# Seleccionamos una ventana alrededor del evento (ej: 60 días antes y 60 después)
df_plot <- df_its %>%
  filter(abs(dias_desde_evento) <= 60)

ggplot(df_plot, aes(x = fecha)) +
  # Puntos observados (reales + imputados)
  geom_point(aes(y = conteos_final), color = "gray50", alpha = 0.6, size = 1.5) +
  # Predicción observada (modelo)
  geom_line(aes(y = pred_observada, color = "Observada (Modelo)"), size = 1) +
  # Predicción contrafactual
  geom_line(aes(y = pred_contrafactual, color = "Contrafactual"), linetype = "dashed", size = 1) +
  # Línea vertical del evento
  geom_vline(xintercept = as.numeric(fecha_evento), linetype = "dotted", color = "red", size = 1) +
  annotate("text", x = fecha_evento + 2, y = max(df_plot$conteos_final), 
           label = "Evento", color = "red", hjust = 0) +
  scale_color_manual(name = "Trayectoria", 
                     values = c("Observada (Modelo)" = "blue", "Contrafactual" = "darkorange")) +
  theme_minimal() +
  labs(
    title = "Efecto del Evento en Consultas Ambulatorias",
    x = "Fecha",
    y = "Número de Atenciones Diarias"
  ) +
  theme(legend.position = "bottom")