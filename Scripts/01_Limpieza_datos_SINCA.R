# =============================================================================
# TESIS: Mega incendio Viña del Mar-Valparaíso 2024 y consultas de urgencia
# Diseño: Series de tiempo interrumpidas (ITS) con regresión Binomial Negativa
# Repositorio: [GitHub - https://github.com/FranVasEstay/Urgencias_Incendios_Vina.git]
# =============================================================================

# -------- LIMPIEZA DE DATOS SINCA --------- #

mp25_raw <- read.csv2("Datos/Raw/SINCA/pm25vina.csv", 
                       fileEncoding = "UTF-8")

mp25_diario <- mp25_raw %>%
  # 1. Eliminar columna X (vacía) y renombrar
  select(-X) %>%
  rename(fecha_yyyymmdd = FECHA..YYMMDD.,
         hora_hhmm = HORA..HHMM.,
         mp25_validado = Registros.validados,
         mp25_preliminar = Registros.preliminares) %>%
  
  # 2. Convertir fecha y hora a formatos Date
  mutate(
    fecha = ymd(paste0("20", as.character(fecha_yyyymmdd))),
    hora_str = sprintf("%04d", hora_hhmm),
    hora = hm(paste0(substr(hora_str, 1, 2), ":", substr(hora_str, 3, 4))),
    datetime = ymd_hm(paste(fecha, hora_str))
  ) %>%
  
  # 3. Filtrar período de estudio
  filter(fecha >= as.Date("2023-02-01") & 
         fecha <= as.Date("2025-02-28")) %>%
  
  # 4. Combinar validado + preliminar: prioriza validado, luego preliminar
  # Anotar esto en la metodología: "Se prioriza el registro validado, y en su ausencia se utiliza el preliminar"
  mutate(mp25 = coalesce(mp25_validado, mp25_preliminar))

# Gráfico rápido de control
ggplot(mp25_diario, aes(x = fecha, y = mp25)) +
  geom_line(na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2024-02-02"), 
             linetype = "dashed", color = "darkred") +
  labs(title = "MP2.5 diario - Estación Viña del Mar",
       y = expression(MP[2.5] ~ (mu*g/m^3))) +
  theme_minimal()

save(mp25_diario, file = "Datos/Processed/MP25.RData")
