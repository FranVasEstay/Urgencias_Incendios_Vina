# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - Enlace a tu repositorio público]
# =============================================================================

# Figura 1. Mapa de comunas seleccionadas y no seleccionadas

# Extraer lista de comunas incluidas
comunas_incluidas <- unique(urgencias_subset$NombreComuna)

# Crear columna de selección en el shapefile de comunas
comunas <- comunas %>%
  mutate(seleccionada = ifelse(comuna %in% comunas_incluidas, "Seleccionada", "No seleccionada"))

# Mapa
mapa1 <- ggplot() +
  geom_sf(data = comunas, aes(fill = seleccionada), color = "white", size = 0.2) +
  geom_sf(data = limites, fill = NA, color = "#1D3557", size = 0.4) +
  scale_fill_manual(values = c("Seleccionada" = "#E63946", "No seleccionada" = "#D3D3D3")) +
  labs(title = "Comunas seleccionadas para el estudio",
       subtitle = "Región de Valparaíso",
       caption = "Fuente: SUBDERE") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  coord_sf(xlim = c(-72.0, -70.5), ylim = c(-33.5, -32.0))

print(mapa1)

# Guardar mapa
ggsave(file.path(dir_figs, "Fig1.mapa_comunas_seleccionadas.png"), mapa1, width = 10, height = 8, dpi = 300)
