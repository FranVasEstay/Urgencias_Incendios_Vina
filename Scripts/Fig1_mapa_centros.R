# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - Enlace a tu repositorio público]
# =============================================================================

# Figura 1. Mapa con incendio y centros de salud de comunas incluidas.
crs_base <- st_crs(comunas)
limites <- st_transform(limites, crs_base)
incendio_shape <- st_transform(incendio_shape, crs_base)

# Extraer tipos de centro de interés
table(establecimientos_valpo$urgencia, useNA = "ifany")
centros_urgencia <- establecimientos_valpo %>%
  filter(urgencia == "SI")
comunas_incluidas <- c("Valparaíso", "Viña del Mar", "Villa Alemana", 
                       "Quilpué", "Limache", "Concón")# Sólo centros de interés
comunas_incluidas_MAYUS <- c("VALPARAÍSO", "VIÑA DEL MAR", "VILLA ALEMANA", 
                       "QUILPUÉ", "LIMACHE", "CONCÓN")
centros_urgencia <- centros_urgencia %>%
  filter(nom_comuna %in% comunas_incluidas_MAYUS)

centros_urgencia <- centros_urgencia %>%
  mutate(
    categoria = case_when(
      grepl("Hospital", nombre, ignore.case = TRUE) ~ "Hospital",
      grepl("CESFAM|PSR|Consultorio|SAPU|SAR|Posta|Centro de Salud", tipo, ignore.case = TRUE) ~ "APS",
      grepl("SUR", tipo, ignore.case = TRUE) ~ "SUR",
      TRUE ~ "Clínica"
    )
  )
# Eliminar SUR de Laguna Verde
centros_urgencia <- centros_urgencia %>%
  filter(categoria != "SUR")

# Ver distribución
table(centros_urgencia$categoria)

# Mapa
fondo_comunas <- comunas %>% filter(comuna %in% comunas_incluidas)
mapa_incendio_centros <- ggplot() +
  # Base: comunas seleccionadas en gris claro
  geom_sf(data = fondo_comunas, fill = "#e7e7e7", color = "white", size = 0.2) +
  geom_sf(data = limites, fill = NA, color = "#1D3557", size = 0.4) +
  # Etiquetas con nombres de las comunas
    geom_sf_text(data = fondo_comunas,
               aes(label = comuna), 
               size = 3.5,
               color = "#333333",
               fontface = "bold",
               check_overlap = TRUE) +  # Evita que se superpongan etiquetas
  # Polígono del incendio
  geom_sf(data = incendio_shape, fill = "#D62828", alpha = 0.5, color = NA) +
  # Centros de salud: tamaño según complejidad, color
  geom_sf(data = centros_urgencia, 
          aes(color = categoria), 
          alpha = 1,
          size = 3) +
    scale_shape_manual(name = "Tipo de centro",
                     values = c("Hospital" = 16,   # Círculo relleno
                                "APS" = 17,         # Triángulo relleno
                                "Otro" = 15)) +
  # Escalas
  scale_color_paletteer_d("tvthemes::Alexandrite", 
                          name = "Tipo de centro") +
  labs(title = "Centros de salud con servicios de urgencia en las comunas seleccionadas",
       subtitle = "Zona afectada por mega incendio en Rojo, comunas seleccionadas en gris, Región de Valparaíso, febrero 2024",
       caption = "Fuente: CIGIDEN | DEIS") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(hjust = 0.5, face = "bold", size = 19),
        plot.subtitle = element_text(hjust = 0.5, size = 15),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20, face = "bold")) +
  # Enfocar en el área del incendio
  coord_sf(xlim = c(-71.8, -71), ylim = c(-33.3, -32.8)) +
  # Agregar barra de escala
  annotation_scale(location = "bl", width_hint = 0.3)

# Mostrar
print(mapa_incendio_centros)

# Guardar
ggsave(file.path(dir_figs, "Fig1.mapa_incendio_centros_urgencia.png"), 
       mapa_incendio_centros, width = 12, height = 10, dpi = 300)
