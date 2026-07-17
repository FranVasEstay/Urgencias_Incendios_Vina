# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - https://github.com/FranVasEstay/Urgencias_Incendios_Vina.git]
# =============================================================================

# ==============================================================================
# A. DATOS DE URGENCIAS PARA COMUNAS SELECCIONADAS
# ==============================================================================
load(file.path(dir_data, "Processed/Urgencias_valpo_selected.RData"))

# ==============================================================================
# B. DATOS CARTOGRAFÍA MAPA 
# ==============================================================================
## Límites comunales
#zip_url <- "https://ide.subdere.gov.cl/wp-content/uploads/74_limites_dpa_2022.zip"
#zip_path <- file.path(dir_mapa, "74_limites_dpa_2022.zip")
#download.file(zip_url, zip_path, mode = "wb")
#unzip(zip_path, exdir = dir_mapa)
limites <- read_sf(file.path(dir_mapa, "LIMITES_DPA_V0702_SIRGASCHILE_GCS.shp")) |> clean_names()

## Comunas
#rar_url <- "https://ide.subdere.gov.cl/descargas/SHP/Limite_DPA_03082023.rar"
#rar_path <- file.path(dir_mapa, "Limite_DPA_03082023.rar")
#download.file(rar_url, rar_path, mode = "wb") 
#system(paste("unrar x", shQuote(rar_path), shQuote(dir_mapa))) #Descomprimir manualmente
comunas <- read_sf(file.path(dir_mapa, "DPA_2023/COMUNAS/COMUNAS_v1.shp")) |> clean_names()

#Unificar crs
crs_comunas <- st_crs(comunas) 
if(is.na(st_crs(limites))) {
  st_crs(limites) <- crs_comunas
} else if(st_crs(limites) != crs_comunas) {
  limites <- st_transform(limites, crs_comunas)
}

# ==============================================================================
# C. DATOS CARTOGRAFIA ZONA INCENDIOS 2024 (CIGIDEN ZENODO)
# ==============================================================================
#zenodo_id <- "13749149"
#api_url <- paste0("https://zenodo.org/api/records/", zenodo_id)
#resp <- GET(api_url) # Obtener metadatos
#datos_api <- fromJSON(content(resp, as = "text", encoding = "UTF-8"))
#file_url <- datos_api$files$links$self[1] # Extraer la URL del primer archivo (el ZIP)
#zip_path <- file.path(dir_mapa, "cigiden_fire_zone.zip")
#download.file(file_url, zip_path, mode = "wb") # Descargar
#unzip(zip_path, exdir = dir_mapa) # Descomprimir
#file.remove(zip_path) #Remover
shp_list <- list.files(dir_mapa, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE) # Leer shapefile
incendio_shape <- read_sf(shp_list[grepl("cigiden|incendio", shp_list, ignore.case = TRUE)][1]) %>%
  clean_names()
if(exists("incendio_shape")) {
  if(is.na(st_crs(incendio_shape))) {
    st_crs(incendio_shape) <- crs_comunas
  } else if(st_crs(incendio_shape) != crs_comunas) {
    incendio_shape <- st_transform(incendio_shape, crs_comunas)
  }
}

# ==============================================================================
# D. DATOS CARTOGRAFIA CENTROS DE SALUD
# ==============================================================================
# Cargar datos de establecimientos de slaud georef.
establecimientos_raw<- read_sf(file.path(dir_mapa, "l_910_v1_establecimientos_de_sal/l_910_v1_establecimientos_de_salud_diciembre_2025.shp")) |> clean_names()

# Filtrar solo la Región de Valparaíso (código 5)
establecimientos_valpo <- establecimientos_raw %>%
  filter(cod_reg == 5)

# ==============================================================================
# E. DATOS DE MP2.5 SINCA
# ==============================================================================
load("Datos/Processed/MP25.RData")

# ==============================================================================
# F. DATOS METEOROLÓGICOS
# ==============================================================================
meteo_raw <- readxl::read_excel("data/raw/meteorologia/dmc_valpo_2023_2025.xlsx",
                                 sheet = 1)

# ==============================================================================
# G. CALENDARIO DE FERIADOS
# ==============================================================================
url <- "https://apis.digital.gob.cl/fl/feriados"
feriados_raw <- GET(url) %>% 
  content("text") %>% 
  fromJSON(simplifyDataFrame = TRUE)

# Filtrar los años de interés
feriados_chile <- feriados_raw %>%
  filter(as.Date(fecha) >= "2023-02-01" & 
         as.Date(fecha) <= "2025-02-28") %>%
  mutate(fecha = as.Date(fecha))

rm(feriados_raw)

