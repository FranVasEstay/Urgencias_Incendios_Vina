# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - Enlace a tu repositorio público]
# =============================================================================

# Figura 2. Mapa con incendio y centros de salud de comunas incluidas.
crs_base <- st_crs(comunas)
limites <- st_transform(limites, crs_base)
incendio_shape <- st_transform(incendio_shape, crs_base)

# Extraer tipos de centro de interés
table(establecimientos_valpo$urgencia, useNA = "ifany")
centros_urgencia <- establecimientos_valpo %>%
  filter(urgencia == "SI")

centros_urgencia <- centros_urgencia %>%
  mutate(
    categoria = case_when(
      grepl("Hospital", nombre, ignore.case = TRUE) ~ "Hospital",
      grepl("Hospital", tipo, ignore.case = TRUE) ~ "Hospital",
      complejida == "Alta Complejidad" ~ "Hospital",
      complejida == "Mediana Complejidad" ~ "Hospital", # o podrías dejarlo aparte
      grepl("CESFAM|PSR|Consultorio|SAPU|SAR|Posta|Centro de Salud", tipo, ignore.case = TRUE) ~ "APS",
      TRUE ~ "Otro"
    ),
    # También podemos agrupar complejidad para simbolizar
    nivel_complejidad = case_when(
      complejida %in% c("Alta Complejidad", "Mediana Complejidad") ~ "Alta/Mediana",
      complejida == "Baja Complejidad" ~ "Baja",
      TRUE ~ "Sin especificar"
    )
  )

# Ver distribución
table(centros_urgencia$categoria, centros_urgencia$nivel_complejidad)

# Mapa
comunas_incluidas <- c("Valparaíso", "Viña del Mar", "Villa Alemana", 
                       "Quilpué", "Limache", "Concón")
fondo_comunas <- comunas %>% filter(comuna %in% comunas_incluidas)
mapa_incendio_centros <- ggplot() +
  # Base: comunas seleccionadas en gris claro
  geom_sf(data = fondo_comunas, fill = "#F5F5F5", color = "white", size = 0.2) +
  # Polígono del incendio
  geom_sf(data = incendio_shape, fill = "#D62828", alpha = 0.5, color = NA) +
  # Centros de salud: tamaño según complejidad, color según categoría
  geom_sf(data = centros_urgencia, 
          aes(color = categoria, size = nivel_complejidad), 
          alpha = 0.8) +
  # Escalas
  scale_color_manual(name = "Tipo de centro", 
                     values = c("Hospital" = "#264653", "APS" = "#2A9D8F", "Otro" = "#E9C46A")) +
  scale_size_manual(name = "Complejidad", 
                    values = c("Alta/Mediana" = 3, "Baja" = 2, "Sin especificar" = 1.5)) +
  # Etiquetas y tema
  labs(title = "Zona afectada por mega incendio y centros de salud con urgencia",
       subtitle = "Comunas seleccionadas, Región de Valparaíso, febrero 2024",
       caption = "Fuente: CIGIDEN (Zenodo) | DEIS (Geoportal MINSAL)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        legend.box = "vertical",
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5)) +
  # Enfocar en el área del incendio (ajusta según tu shapefile)
  coord_sf(xlim = c(-71.7, -71.3), ylim = c(-33.2, -32.9)) +
  # Opcional: agregar barra de escala
  annotation_scale(location = "bl", width_hint = 0.3)

# Mostrar
print(mapa_incendio_centros)

# Guardar
ggsave(file.path(dir_figs, "Fig2.mapa_incendio_centros_urgencia.png"), 
       mapa_incendio_centros, width = 12, height = 10, dpi = 300)
