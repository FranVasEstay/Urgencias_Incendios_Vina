# Librerías
library(tidyverse)
library(mgcv)
library(MASS)
library(sandwich)
library(lmtest)
# Nuevas librerías para iteración paralela
library(furrr) 
library(future)

## Datos simulados 
# Parámetros del estudio
fecha_inicio <- as.Date("2023-01-01")
fecha_fin <- as.Date("2024-06-30")
fecha_evento <- as.Date("2024-02-02")
feriados_oficiales <- as.Date(c("2023-01-01", "2023-04-07", "2023-05-01", 
                                "2023-09-18", "2023-09-19", "2023-12-25",
                                "2024-01-01", "2024-03-29", "2024-05-01"))

# Simulación de un panel de datos completo
set.seed(42)
datos_panel <- expand_grid(
  fecha = seq(fecha_inicio, fecha_fin, by = "day"),
  region = c("Valparaiso", "Santiago"),
  tipo_centro = c("hospitalario", "ambulatorio"),
  prestacion = c("pediatrica", "adultos", "respiratoria")
) %>%
  mutate(
    # Generamos conteos aleatorios con algunas diferencias por grupo
    lambda_base = case_when(
      tipo_centro == "hospitalario" ~ 300,
      TRUE ~ 150
    ),
    conteos = rpois(n(), lambda = lambda_base),
    
    # Pre-cálculo de variables temporales estructurales
    dia_semana = wday(fecha, label = TRUE, abbr = FALSE, week_start = 1),
    dia_semana_fac = as.factor(dia_semana),
    mes = as.factor(month(fecha)),
    indice_temporal = as.numeric(fecha - min(fecha)) + 1,
    es_feriado = if_else(fecha %in% feriados_oficiales, 1, 0),
    
    # Variables de la intervención ITS
    dias_desde_evento = as.numeric(fecha - fecha_evento),
    pulse = if_else(dias_desde_evento >= 0 & dias_desde_evento <= 6, 1, 0),
    post = if_else(dias_desde_evento >= 0 & dias_desde_evento <= 29, 1, 0)
  )

# Introducimos NA's aleatorios para probar la imputación en todas las series
datos_panel$conteos[sample(1:nrow(datos_panel), 300)] <- NA

## wrapper
evaluar_impacto_its <- function(df_serie, tipo_centro_actual) {
  
  # A. EXCLUSIÓN ESTRUCTURAL E IMPUTACIÓN POR CEROS
  df_filtrado <- df_serie %>%
    mutate(
      conteos = case_when(
        tipo_centro_actual == "hospitalario" & (dia_semana == "domingo" | es_feriado == 1) & is.na(conteos) ~ 0,
        tipo_centro_actual == "ambulatorio" & es_feriado == 1 & is.na(conteos) ~ 0,
        TRUE ~ conteos
      )
    ) %>%
    filter(
      !(tipo_centro_actual == "hospitalario" & (dia_semana == "domingo" | es_feriado == 1)),
      !(tipo_centro_actual == "ambulatorio" & es_feriado == 1)
    )
  
  # Si por alguna razón la serie quedó vacía, devolvemos NULL
  if(nrow(df_filtrado) < 100) return(NULL) 
  
  # B. IMPUTACIÓN GAM PARA NA's REMANENTES
  modelo_gam <- tryCatch({
    gam(conteos ~ s(indice_temporal) + s(dia_semana_fac, bs = "re"), 
        family = quasipoisson, data = df_filtrado)
  }, error = function(e) NULL)
  
  if(is.null(modelo_gam)) return(NULL)
  
  df_filtrado <- df_filtrado %>%
    mutate(
      pred_gam = round(pmax(predict(modelo_gam, newdata = ., type = "response"), 0)),
      conteos_final = if_else(is.na(conteos), pred_gam, conteos)
    )
  
  # C. MODELO ITS (BINOMIAL NEGATIVA)
  modelo_its <- tryCatch({
    glm.nb(conteos_final ~ indice_temporal + pulse + post + dia_semana_fac + mes, 
           data = df_filtrado)
  }, error = function(e) NULL)
  
  if(is.null(modelo_its)) return(NULL)
  
  # D. ERRORES ROBUSTOS NEWEY-WEST Y SIMULACIÓN
  vcov_nw <- NeweyWest(modelo_its, lag = 7, prewhite = FALSE)
  coefs_est <- coef(modelo_its)
  
  X_obs <- model.matrix(modelo_its)
  X_cf <- X_obs
  X_cf[, "pulse"] <- 0
  X_cf[, "post"] <- 0
  
  n_sims <- 10000
  simulaciones_coefs <- mvrnorm(n_sims, mu = coefs_est, Sigma = vcov_nw)
  
  # Función interna para calcular impacto en ventana
  calc_impacto_ventana <- function(dias_max) {
    idx <- which(df_filtrado$dias_desde_evento >= 0 & df_filtrado$dias_desde_evento < dias_max)
    if(length(idx) == 0) return(c(NA, NA, NA))
    
    X_obs_v <- X_obs[idx, , drop = FALSE]
    X_cf_v <- X_cf[idx, , drop = FALSE]
    
    imp_relativo <- sapply(1:n_sims, function(i) {
      beta <- simulaciones_coefs[i, ]
      y_obs_sim <- sum(exp(X_obs_v %*% beta))
      y_cf_sim <- sum(exp(X_cf_v %*% beta))
      (y_obs_sim - y_cf_sim) / y_cf_sim * 100
    })
    
    quantile(imp_relativo, probs = c(0.5, 0.025, 0.975), na.rm = TRUE)
  }
  
  res_7d <- calc_impacto_ventana(7)
  res_30d <- calc_impacto_ventana(30)
  
  # E. RETORNAR RESULTADOS COMO TIBBLE (Fila resumen)
  tibble(
    impacto_rel_7d_pct = res_7d[1],
    ic_inf_7d_pct = res_7d[2],
    ic_sup_7d_pct = res_7d[3],
    impacto_rel_30d_pct = res_30d[1],
    ic_inf_30d_pct = res_30d[2],
    ic_sup_30d_pct = res_30d[3],
    # Guardamos los p-values robustos de pulse y post para la tabla de coeficientes
    p_value_pulse = coeftest(modelo_its, vcov. = vcov_nw)["pulse", "Pr(>|z|)"],
    p_value_post  = coeftest(modelo_its, vcov. = vcov_nw)["post", "Pr(>|z|)"]
  )
}

## ejecución múltiple en paralelo
# 1. Configurar el procesamiento en paralelo
plan(multisession, workers = availableCores() - 1) # Usa todos tus núcleos menos 1

# 2. Agrupar, anidar y mapear
resultados_finales <- datos_panel %>%
  # Agrupamos por las variables que definen cada serie única
  group_by(region, tipo_centro, prestacion) %>%
  # Anidamos el resto de los datos en una columna llamada "data"
  nest() %>%
  # Aplicamos la función a cada fila usando future_map2 para iteración paralela
  mutate(
    analisis = future_map2(
      .x = data, 
      .y = tipo_centro, # Pasamos el tipo_centro como segundo argumento a la función
      .f = ~evaluar_impacto_its(.x, .y),
      .options = furrr_options(seed = TRUE) # Necesario para la reproducibilidad de simulaciones
    )
  ) %>%
  # Desanidamos los resultados para tener una tabla plana y limpia
  unnest(analisis) %>%
  # Eliminamos la columna de datos anidados para dejar solo los resultados
  select(-data)

# 3. Volver al procesamiento normal secuencial
plan(sequential)

## Revisión de resultados
# Ver los resultados ordenados
print(resultados_finales)