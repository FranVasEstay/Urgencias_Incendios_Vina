# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - https://github.com/FranVasEstay/Urgencias_Incendios_Vina.git]
# =============================================================================

# -------- ANÁLISIS DESCRIPTIVO --------- # 
# Preparación de los datos para el análisis descriptivo
#Conteo diario total
diario_total <- urgencias_subset %>%
  group_by(fecha) %>%
  summarise(
    Total              = sum(Total_conteo, na.rm = TRUE),
    Pediatricos        = sum(Conteo_pediatricos, na.rm = TRUE),
    Adultos            = sum(Conteo_adultos, na.rm = TRUE),
    Adultos_Mayores    = sum(Conteo_adultos_mayores, na.rm = TRUE),
    Respiratorios      = sum(Conteo_respiratorios, na.rm = TRUE),
    Cardiovasculares   = sum(Conteo_cardiovasculares, na.rm = TRUE),
    Salud_Mental       = sum(Conteo_salud_mental, na.rm = TRUE),
    .groups = "drop"
  )
# conteo diario por complejidad
diario_complejidad <- urgencias_subset %>%
  group_by(fecha, Complejidad) %>%
  summarise(
    Total              = sum(Total_conteo, na.rm = TRUE),
    Pediatricos        = sum(Conteo_pediatricos, na.rm = TRUE),
    Adultos            = sum(Conteo_adultos, na.rm = TRUE),
    Adultos_Mayores    = sum(Conteo_adultos_mayores, na.rm = TRUE),
    Respiratorios      = sum(Conteo_respiratorios, na.rm = TRUE),
    Cardiovasculares   = sum(Conteo_cardiovasculares, na.rm = TRUE),
    Salud_Mental       = sum(Conteo_salud_mental, na.rm = TRUE),
    n_centros          = n(),                           # n de centros que reportaron ese día
    .groups = "drop"
  )

# Secuencia completa de fechas
fecha_seq <- seq.Date(as.Date("2023-02-01"), as.Date("2025-02-28"), by = "day")

# Fechas del incendio
incendio_inicio <- as.Date("2024-02-02")
incendio_fin    <- as.Date("2024-02-03")

# ==============================================================================
# 1. TABLA DESCRIPTIVA ESTRATIFICADA POR COMPLEJIDAD
# ==============================================================================
formato_media_iqr <- function(x) {
  m  <- mean(x, na.rm = TRUE)
  p25 <- quantile(x, 0.25, na.rm = TRUE)
  p75 <- quantile(x, 0.75, na.rm = TRUE)
  sprintf("%.1f (%.1f–%.1f)", m, p25, p75)
}

tabla_comp_simple <- diario_complejidad %>%
  pivot_longer(
    cols = c(Total, Pediatricos, Adultos, Adultos_Mayores,
             Respiratorios, Cardiovasculares, Salud_Mental),
    names_to = "Variable",
    values_to = "Conteo"
  ) %>%
  group_by(Complejidad, Variable) %>%
  summarise(
    `Media (P25–P75)` = formato_media_iqr(Conteo),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Variable,
    names_from = Complejidad,
    values_from = `Media (P25–P75)`
  )
# Mostrar tabla
print(tabla_comp_simple, n = 50)
write.csv(tabla_comp_simple, file.path(dir_tabs, "Tabla_descriptiva_por_complejidad.csv"), 
          row.names = FALSE, fileEncoding = "UTF-8")

# Tabla formateada para Word
ft_comp <- flextable(tabla_comp_simple) %>%
  set_header_labels(values = c("Variable", "APS", "Hospital")) %>%
  add_header_row(values = c("", "Media (P25–P75)"), colwidths = c(1, 2)) %>%
  align(part = "header", align = "center") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:3, align = "center", part = "body") %>%
  bold(part = "header") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  add_footer_lines("Valores expresados como Media (Percentil 25 – Percentil 75).") %>%
  color(i = 1, part = "footer", color = "gray40") %>%
  fontsize(i = 1, part = "footer", size = 8)

save_as_docx(ft_comp, path = file.path(dir_tabs, "Tabla_descriptiva_por_complejidad.docx"))

# TABLA POR COMUNA
# Crear agregado diario por comuna
diario_comuna <- urgencias_subset %>%
  group_by(fecha, comuna) %>%
  summarise(
    Total            = sum(Total_conteo, na.rm = TRUE),
    Pediatricos      = sum(Conteo_pediatricos, na.rm = TRUE),
    Adultos          = sum(Conteo_adultos, na.rm = TRUE),
    Adultos_Mayores  = sum(Conteo_adultos_mayores, na.rm = TRUE),
    Respiratorios    = sum(Conteo_respiratorios, na.rm = TRUE),
    Cardiovasculares = sum(Conteo_cardiovasculares, na.rm = TRUE),
    Salud_Mental     = sum(Conteo_salud_mental, na.rm = TRUE),
    .groups = "drop"
  )

# Aplicar mismo formato
tabla_comuna_simple <- diario_comuna %>%
  pivot_longer(
    cols = c(Total, Pediatricos, Adultos, Adultos_Mayores,
             Respiratorios, Cardiovasculares, Salud_Mental),
    names_to = "Variable",
    values_to = "Conteo"
  ) %>%
  group_by(comuna, Variable) %>%
  summarise(
    `Media (P25–P75)` = formato_media_iqr(Conteo),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = Variable,
    names_from = comuna,
    values_from = `Media (P25–P75)`
  )

# Asegurar el orden deseado de las comunas
orden_comunas <- c("Concón", "Limache", "Quilpué", "Valparaíso", "Villa Alemana", "Viña del Mar")
tabla_comuna_simple <- tabla_comuna_simple %>%
  select(Variable, all_of(orden_comunas))

# Flextable para comuna
ft_comuna <- flextable(tabla_comuna_simple) %>%
  set_header_labels(values = c("Variable", orden_comunas)) %>%
  add_header_row(values = c("", "Media (P25–P75)"), colwidths = c(1, length(orden_comunas))) %>%
  align(part = "header", align = "center") %>%
  align(j = 1, align = "left", part = "body") %>%
  align(j = 2:(length(orden_comunas)+1), align = "center", part = "body") %>%
  bold(part = "header") %>%
  border_outer(border = fp_border(color = "black", width = 2)) %>%
  border_inner_h(border = fp_border(color = "gray", width = 0.5)) %>%
  fontsize(size = 10, part = "all") %>%
  font(fontname = "Times New Roman", part = "all") %>%
  autofit() %>%
  add_footer_lines("Valores expresados como Media (Percentil 25 – Percentil 75).") %>%
  color(i = 1, part = "footer", color = "gray40") %>%
  fontsize(i = 1, part = "footer", size = 8)

save_as_docx(ft_comuna, path = file.path(dir_tabs, "Tabla_descriptiva_comuna.docx"))

# =============================================================================
# 2. VERIFICACIÓN DE TIEMPOS COMPLETOS
# =============================================================================
# Total diario
faltantes_total <- sum(!fecha_seq %in% diario_total$fecha)
cat("Días faltantes en serie total diaria:", faltantes_total, "\n") # NO HAY DÍAS FALTANTES

# Por complejidad
completitud_complejidad <- diario_complejidad %>%
  group_by(Complejidad) %>%
  summarise(
    n_dias = n(),
    faltantes = sum(!fecha_seq %in% fecha),
    .groups = "drop"
  )
print(completitud_complejidad) # NO HAY NINGÚN DÍA FALTANTE

# =============================================================================
# 3. VISUALIZACIONES
# =============================================================================
# Tema común para todas las figuras
theme_set(tema_tesis)

# Series de tiempo diarias (total y por tipo de consulta)
# Preparar datos largos para todos los indicadores
diario_largo <- diario_total %>%
  pivot_longer(-fecha, names_to = "Indicador", values_to = "Consultas") %>%
  mutate(
    Indicador = factor(Indicador,
                       levels = c("Total", "Pediatricos", "Adultos", "Adultos_Mayores",
                                  "Respiratorios", "Cardiovasculares", "Salud_Mental"))
  )

p_series <- ggplot(diario_largo, aes(x = fecha, y = Consultas)) +
  geom_line(color = "#4682B4", linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE, 
              color = "red", linewidth = 0.8) +
  geom_vline(xintercept = c(incendio_inicio, incendio_fin),
             linetype = "dashed", color = "darkorange") +
  facet_wrap(~ Indicador, scales = "free_y", ncol = 2) +
  labs(title = "Series diarias de consultas de urgencia",
       subtitle = "Comunas seleccionadas, feb 2023 – feb 2025. LOESS rojo. Líneas naranjas: incendio",
       y = "Número de consultas", x = "") +
  tema_tesis
print(p_series)
ggsave(file.path(dir_figs, "series_tiempo_indicadores.png"), 
       p_series, width = 14, height = 12, dpi = 300)

# Boxplots por periodo respecto al incendio
# Definir periodos
diario_largo <- diario_largo %>%
  mutate(
    periodo = case_when(
      fecha < incendio_inicio ~ "Pre-incendio",
      fecha >= incendio_inicio & fecha <= (incendio_fin + days(5)) ~ "Incendio (7 días)",
      TRUE ~ "Post-incendio"
    ),
    periodo = factor(periodo, 
                     levels = c("Pre-incendio", "Incendio (7 días)", "Post-incendio"))
  )

p_box <- ggplot(diario_largo, aes(x = periodo, y = Consultas, fill = periodo)) +
  geom_boxplot(alpha = 0.7, outlier.size = 0.8) +
  stat_summary(fun = mean, geom = "point", shape = 18, 
               color = "darkred", size = 2.5) +
  facet_wrap(~ Indicador, scales = "free_y", ncol = 2) +
  labs(title = "Distribución de consultas diarias según periodo respecto al incendio",
       y = "Consultas diarias", x = "") +
  scale_fill_paletteer_d("tvthemes::gravityFalls") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1))
print(p_box)
ggsave(file.path(dir_figs, "boxplots_periodo.png"), 
       p_box, width = 14, height = 14, dpi = 300)

# Series de tiempo por complejidad
p_serie_comp <- ggplot(diario_complejidad, aes(x = fecha, y = Total, color = Complejidad)) +
  geom_line(linewidth = 0.4) +
  facet_wrap(~ Complejidad, scales = "free_y", ncol = 1) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  geom_vline(xintercept = c(incendio_inicio, incendio_fin), 
             linetype = "dashed", color = "black") +
  labs(title = "Consultas diarias totales por nivel de complejidad",
       subtitle = "Suavizamiento LOESS. Líneas punteadas: incendio 2-3 feb 2024",
       y = "Consultas", x = "") +
  scale_color_paletteer_d("tvthemes::gravityFalls") +
  theme(legend.position = "none")
print(p_serie_comp)
ggsave(file.path(dir_figs, "series_complejidad.png"), 
       p_serie_comp, width = 10, height = 12, dpi = 300)

# Gráficos de causas por periodo
diario_total_periodo <- diario_total %>%
  mutate(periodo = case_when(
    fecha < incendio_inicio ~ "Pre-incendio",
    fecha >= incendio_inicio & fecha <= incendio_fin + days(6) ~ "Incendio (7d)",
    TRUE ~ "Post-incendio"
  )) %>%
  group_by(periodo) %>%
  summarise(
    Respiratorias = sum(Respiratorios),
    Cardiovasculares = sum(Cardiovasculares),
    Salud_Mental = sum(Salud_Mental),
    .groups = "drop"
  ) %>%
  pivot_longer(-periodo, names_to = "Causa", values_to = "n") %>%
  group_by(periodo) %>%
  mutate(prop = n/sum(n))

ggplot(diario_total_periodo, aes(x = periodo, y = prop, fill = Causa)) +
  geom_col() +
  scale_fill_brewer(palette = "Set2") +
  labs(title = "Composición de consultas de urgencia según período")

# Mapa de calor día de la semana vs mes
diario_total <- diario_total %>%
  mutate(
    dia_semana = wday(fecha, label = TRUE, abbr = TRUE),
    mes        = month(fecha, label = TRUE, abbr = TRUE)
  )

p_heatmap <- diario_total %>%
  group_by(mes, dia_semana) %>%
  summarise(Media_total = mean(Total, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = dia_semana, y = mes, fill = Media_total)) +
  geom_tile(color = "white") +
  scale_fill_viridis_c(option = "C", name = "Media\nconsultas") +
  labs(title = "Media de consultas diarias por día de la semana y mes",
       x = "Día de la semana", y = "Mes") +
  tema_tesis
print(p_heatmap)

# SERIES
# Ventanas de exposición para sombreado
pulso_7d      <- c(incendio_inicio, incendio_inicio + days(6))
extendido_30d <- c(incendio_inicio, incendio_inicio + days(29))

col_etiq <- c("Pediatricos" = "#E69F00", "Adultos" = "#0072B2",
              "Respiratorios" = "#00BFC4", "Cardiovasculares" = "#F8766D",
              "Salud_Mental" = "#C77CFF")

# Por edad
diario_etario <- diario_total %>%
  select(fecha, Pediatricos, Adultos) %>%
  pivot_longer(-fecha, names_to = "Grupo", values_to = "Consultas")

# Etiquetas más descriptivas
diario_etario$Grupo <- factor(diario_etario$Grupo,
                              levels = c("Pediatricos", "Adultos"),
                              labels = c("Pediátricos (<15 años)", "Adultos (≥15 años)"))

p_serie_etario <- ggplot(diario_etario, aes(x = fecha, y = Consultas, color = Grupo)) +
  # Rectángulos de exposición
  annotate("rect", xmin = pulso_7d[1], xmax = pulso_7d[2],
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.12) +
  annotate("rect", xmin = extendido_30d[1], xmax = extendido_30d[2],
           ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.08) +
  # Líneas diarias y suavizamiento
  geom_line(alpha = 0.35, linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, linewidth = 1.2) +
  # Colores manuales
  scale_color_manual(values = c("Pediátricos (<15 años)" = "#E69F00",
                                "Adultos (≥15 años)" = "#0072B2")) +
  labs(title = "Consultas diarias de urgencia por grupo etario",
       subtitle = "Sombreado rojo: pulso 7 días | Sombreado naranja: período extendido 30 días",
       y = "Número de consultas", x = "",
       color = "Grupo etario") +
  tema_tesis + theme(legend.position = "bottom")
p_serie_etario
ggsave(file.path(dir_figs, "serie_etaria_incendio.png"), p_serie_etario, width = 12, height = 7, dpi = 300)

# SERIE DE CAUSAS: RESPIRATORIAS, CARDIOVASCULARES Y SALUD MENTAL
diario_causas <- diario_total %>%
  select(fecha, Respiratorios, Cardiovasculares, Salud_Mental) %>%
  pivot_longer(-fecha, names_to = "Causa", values_to = "Consultas") %>%
  mutate(Causa = factor(Causa,
                        levels = c("Respiratorios", "Cardiovasculares", "Salud_Mental"),
                        labels = c("Respiratorias", "Cardiovasculares", "Salud mental")))

p_serie_causas <- ggplot(diario_causas, aes(x = fecha, y = Consultas, color = Causa)) +
  # Rectángulos de exposición
  annotate("rect", xmin = pulso_7d[1], xmax = pulso_7d[2],
           ymin = -Inf, ymax = Inf, fill = "red", alpha = 0.12) +
  annotate("rect", xmin = extendido_30d[1], xmax = extendido_30d[2],
           ymin = -Inf, ymax = Inf, fill = "orange", alpha = 0.08) +
  # Línea fina y suavizamiento
  geom_line(alpha = 0.35, linewidth = 0.3) +
  geom_smooth(method = "loess", span = 0.25, se = FALSE, linewidth = 1.2) +
  # Facetas para cada causa (escalas libres)
  facet_wrap(~ Causa, scales = "free_y", ncol = 1) +
  scale_color_manual(values = c("Respiratorias"   = "#00BFC4",
                                "Cardiovasculares" = "#F8766D",
                                "Salud mental"     = "#C77CFF")) +
  labs(title = "Consultas diarias por tipo de diagnóstico",
       subtitle = "Sombreado rojo: pulso 7 días | Sombreado naranja: período extendido 30 días",
       y = "Número de consultas", x = "") +
  tema_tesis + theme(legend.position = "none")
p_serie_causas
ggsave(file.path(dir_figs, "serie_causas_incendio.png"), p_serie_causas, width = 12, height = 10, dpi = 300)
