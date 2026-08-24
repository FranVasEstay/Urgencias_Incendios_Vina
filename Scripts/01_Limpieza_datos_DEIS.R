# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - https://github.com/FranVasEstay/Urgencias_Incendios_Vina.git]
# =============================================================================

# -------- LIMPIEZA DE DATOS DEIS --------- #

# DATOS 
## 2023
AtencionesUrgencia2023 <- read_delim("Datos/Raw/DEIS/AtencionesUrgencia2023.csv",
                                     delim = ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1"),
                                     col_names = TRUE) %>%
  filter(CodigoRegion == 5)

## 2024
AtencionesUrgencia2024 <- read_delim("Datos/Raw/DEIS/AtencionesUrgencia2024.csv",
                                     delim = ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1"),
                                     col_names = TRUE) %>%
  filter(CodigoRegion == 5)

## 2025
AtencionesUrgencia2025 <- read_delim("Datos/Raw/DEIS/AtencionesUrgencia2025.csv",
                                     delim = ";",
                                     escape_double = FALSE,
                                     trim_ws = TRUE,
                                     locale = locale(encoding = "ISO-8859-1"),
                                     col_names = TRUE) %>%
  filter(CodigoRegion == 5)

## Unión de bases de datos
urgencias_valpo <- bind_rows(AtencionesUrgencia2023, AtencionesUrgencia2024, AtencionesUrgencia2025)
rm(AtencionesUrgencia2023,AtencionesUrgencia2024,AtencionesUrgencia2025)

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

## Causas de urgencia
### Selección de causas respiratorias
causas_respiratorias <- c(
  "Neumonía (J12-J18)",
  "IRA Alta (J00-J06)",
  "Bronquitis/bronquiolitis aguda (J20-J21)",
  "Crisis obstructiva bronquial (J40-J46)",
  "Influenza (J09-J11)",
  "Otra causa respiratoria (J22, J30-J39, J47, J60-J98)",
  "Otra causa respiratoria no contenidas en las categorías anteriores (J22, J30-J39, J47, J60-J98)",
  "- COVID-19, VIRUS IDENTIFICADO U07.1",
  "- COVID-19, VIRUS NO IDENTIFICADO U07.2",
  "Covid-19, Virus identificado U07.1",
  "Covid-19, Virus no identificado U07.2",
  "- Por covid-19, virus identificado U07.1",
  "- Por covid-19, virus no identificado U07.2"
)
### Selección de causas cardiovasculares
causas_cardiovasculares <- c(
  "Infarto agudo miocardio",
  "Infarto agudo miocardio (I21-I22)",
  "Accidente vascular encefálico",
  "Accidente vascular encefálico (I60-I66, I67.8-I67.9, I69)",
  "Crisis hipertensiva",
  "Crisis hipertensiva (I10.X)",
  "Arritmia grave",
  "Arritmia grave (I44-I46.0, I46.9-I49)",
  "Otras causas circulatorias",
  "Otras causas circulatorias no contenidas en las categorías anteriores (I00-I09, I11-I15, I20, I23-I28, I30-I42, I50-I52, I67.0-I67.7 e I70-I99)"
)
### Selección de causas de salud mental
causas_salud_mental <- c(
  "Trastornos neuróticos, trastornos relacionados con el estrés y trastornos somatomorfos (F40-F48) Incluído el trastorno de pánico (F41.0)",
  "Trastornos neuróticos, trastornos relacionados con el estrés y trastornos somatomorfos (F40-F48)",
  "Ideación Suicida (R45.8)",
  "Ideación suicida (R45.8)",
  "Trastornos del Humor (Afectivos) (F30-F39)",
  "Trastornos mentales y del comportamiento debidos al uso de sustancias psicoactivas (F10-F19)",
  "Lesiones autoinfligidas intencionalmente (X60-X84)",
  "Lesiones Autoinflingidas Intencionalmente (Causa Externa X60-X84)",
  "Otros trastornos mentales no contenidos en las categorías anteriores"
)

urgencias_valpo <- urgencias_valpo %>%
  mutate(
    causa_respiratoria   = GlosaCausa %in% causas_respiratorias,
    causa_cardiovascular = GlosaCausa %in% causas_cardiovasculares,
    causa_salud_mental   = GlosaCausa %in% causas_salud_mental
  )

## Se va a trabajar mejor con algunas comunas seleccionadas: Valparaíso, Viña del mar, Quilpué, Villa Alemana, Limache, Placilla de Peñuelas.
selected_comunas <- c("Valparaíso", "Viña del Mar","Quilpué","Villa Alemana","Limache","Concón")
urgencias_subset <- filter(urgencias_valpo, NombreComuna %in% selected_comunas)

## Guardar datos en formato "unificado"
urgencias_subset <- urgencias_subset %>%
  group_by(fecha, NombreComuna, NEstablecimiento, IdEstablecimiento, 
           GLOSATIPOESTABLECIMIENTO) %>%
  summarise(
    # Conteos totales y por grupos de edad (suma sobre todas las causas)
    Total_conteo                      = sum(Total, na.rm = TRUE),
    Conteo_pediatricos                = sum(Menores_1 + De_1_a_4 + De_5_a_14, na.rm = TRUE),
    Conteo_adultos                    = sum(De_15_a_64, na.rm = TRUE),
    Conteo_adultos_mayores            = sum(De_65_y_mas, na.rm = TRUE),
    # Conteos por causa específica (suma de Total solo cuando la causa es TRUE)
    Conteo_respiratorios   = sum(Total[causa_respiratoria], na.rm = TRUE),
    Conteo_cardiovasculares = sum(Total[causa_cardiovascular], na.rm = TRUE),
    Conteo_salud_mental    = sum(Total[causa_salud_mental], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  # Renombrar columnas
  rename(
    comuna        = NombreComuna,
    centro        = NEstablecimiento,
    id_centro     = IdEstablecimiento,
    Tipo_Urgencia = GLOSATIPOESTABLECIMIENTO
  )
urgencias_subset <- urgencias_subset %>%
  mutate(
    Complejidad = case_when(
      grepl("Hospital|Clínica", Tipo_Urgencia, ignore.case = TRUE) ~ "Hospital",
      grepl("SAR|SAPU|SUR|PSR|Posta|Consultorio|Centro de Salud", 
            Tipo_Urgencia, ignore.case = TRUE)                     ~ "APS",
      TRUE                                                         ~ "Sin clasificar"
    )
  )
# reordenar columnas
urgencias_subset <- urgencias_subset %>%
  select(
    fecha, comuna, centro, id_centro, Tipo_Urgencia, Complejidad,
    Total_conteo, Conteo_pediatricos, Conteo_adultos, Conteo_adultos_mayores,
    Conteo_respiratorios, Conteo_cardiovasculares,
    Conteo_salud_mental
  )

# GUARDAR DATOS FILTRADOS
save(urgencias_valpo, file = "Datos/Processed/Urgencias_valpo_limpio.RData") # toda la data
save(urgencias_subset, file = "Datos/Processed/Urgencias_valpo_selected.RData") #subset comunas seleccionadas
write.csv(urgencias_subset, file = "Datos/Processed/Urgencias_VALPO_DEIS.csv", row.names = FALSE) 
