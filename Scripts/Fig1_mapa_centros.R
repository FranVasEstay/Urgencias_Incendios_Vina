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
                       "Quilpué", "Limache", "Concón")

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

# =============================================================================
# (1) MATCHING APROXIMADO: nombres de establecimientos en DEIS vs shapefile
# =============================================================================

library(stringdist)

# Nombres únicos en la base DEIS (comunas seleccionadas)
nombres_deis <- unique(urgencias_subset$NEstablecimiento)

# Nombres únicos en el shapefile de centros de urgencia
nombres_shape <- unique(centros_urgencia$nombre)

# Función para encontrar el mejor match por similitud de texto
encontrar_match <- function(nombre_deis, candidatos_shape, umbral = 0.75) {
  limpio_deis <- toupper(trimws(nombre_deis))
  limpios_shape <- toupper(trimws(candidatos_shape))
  
  similitudes <- stringsim(limpio_deis, limpios_shape, method = "jw")
  
  mejor_idx <- which.max(similitudes)
  mejor_sim <- similitudes[mejor_idx]
  
  if (mejor_sim >= umbral) {
    return(candidatos_shape[mejor_idx])
  } else {
    return(NA)
  }
}

# Crear tabla de equivalencias
tabla_match <- data.frame(
  nombre_deis  = nombres_deis,
  nombre_shape = sapply(nombres_deis, function(x) encontrar_match(x, nombres_shape)),
  stringsAsFactors = FALSE
)

# Filtrar centros_urgencia solo con los que aparecen en DEIS
centros_urgencia_filtrado <- centros_urgencia %>%
  filter(nombre %in% tabla_match$nombre_shape[!is.na(tabla_match$nombre_shape)])

cat("\n=== Centros en el mapa después del matching ===\n")
print(table(centros_urgencia_filtrado$categoria))

# =============================================================================
# (2) CREAR POLÍGONO DE MAR
# =============================================================================

# Crear rectángulo del área del mapa y usarlo como mar de fondo
  ocean_bbox <- st_as_sfc(st_bbox(c(xmin = -71.8, ymin = -33.3, xmax = -71.0, ymax = -32.8), crs = 4326))

# =============================================================================
# MAPA
# =============================================================================
# Mapa
library(ggrepel)

crs_utm <- 32719

fondo_comunas <- comunas %>% filter(comuna %in% comunas_incluidas)
fondo_comunas_utm    <- st_transform(fondo_comunas, crs_utm)
comunas_utm          <- st_transform(comunas, crs_utm)  # Todas las comunas
limites_utm          <- st_transform(limites, crs_utm)
incendio_shape_utm   <- st_transform(incendio_shape, crs_utm)
centros_urgencia_utm <- st_transform(centros_urgencia_filtrado, crs_utm)

xlim_utm <- c(245000, 310000)
ylim_utm <- c(6310000, 6370000)

margen <- 5000  # 5 km extra en cada dirección
ocean_bbox_utm <- st_sfc(
  st_polygon(list(rbind(
    c(xlim_utm[1] - margen, ylim_utm[1] - margen),
    c(xlim_utm[2] + margen, ylim_utm[1] - margen),
    c(xlim_utm[2] + margen, ylim_utm[2] + margen),
    c(xlim_utm[1] - margen, ylim_utm[2] + margen),
    c(xlim_utm[1] - margen, ylim_utm[1] - margen)
  ))),
  crs = crs_utm
)
comunas_no_seleccionadas_utm <- comunas_utm %>%
  filter(!comuna %in% comunas_incluidas)

centros_coords <- centros_urgencia_utm %>%
  mutate(
    x = st_coordinates(.)[, 1],
    y = st_coordinates(.)[, 2],
    etiqueta = nombre  # o la columna que quieras mostrar
  ) %>%
  st_drop_geometry() 

mapa_incendio_centros <- ggplot() +
  # Océano
  geom_sf(data = ocean_bbox_utm, fill = "#87CEEB", color = NA, alpha = 0.1) +
  # Base: comunas seleccionadas en gris claro
  geom_sf(data = comunas_no_seleccionadas_utm, fill = "white", color = NA) +
  geom_sf(data = fondo_comunas_utm, fill = "#e7e7e7", color = "white", size = 0.2) +
  geom_sf(data = limites_utm, fill = NA, color = "#1D3557", size = 0.4) +
  # Etiquetas con nombres de las comunas
  geom_sf_text(data = fondo_comunas_utm,
               aes(label = comuna), 
               size = 3.5,
               color = "#333333",
               fontface = "bold",
               check_overlap = TRUE) +
  # Polígono del incendio
  geom_sf(data = incendio_shape_utm, fill = "#D62828", alpha = 0.5, color = NA) +
  # Centros de salud: shape y color mapeados a categoria
  geom_sf(data = centros_urgencia_utm, 
          aes(shape = categoria, color = categoria), 
          alpha = 1,
          size = 3) +
  # Escalas manuales (shape + color sincronizados)
  scale_shape_manual(name = "Tipo de centro",
                     values = c("Hospital" = 16,   # Círculo relleno
                                "APS"     = 17,    # Triángulo relleno
                                "Clínica" = 15)) + # Cuadrado relleno (por si aparece)
  scale_color_paletteer_d("tvthemes::gravityFalls", 
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
  # Enfocar en el área del incendio (ahora en metros UTM)
  coord_sf(xlim = xlim_utm, ylim = ylim_utm) +
  # Barra de escala (funciona con proyección métrica)
  annotation_scale(location = "bl", width_hint = 0.3)

# Mostrar
print(mapa_incendio_centros)

# Guardar
ggsave(file.path(dir_figs, "Fig1.mapa_incendio_centros_urgencia.png"), 
       mapa_incendio_centros, width = 12, height = 10, dpi = 300)
