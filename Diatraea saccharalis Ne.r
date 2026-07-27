# Diatraea saccharalis (Lepidoptera: Crambidae) is a major pest of corn
# The following R code demonstrates how to analyze its population dynamics

# Load necessary libraries
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(dplyr)
library(knitr)

# Read original data: Progreso de Actividades Insectario
data <- read_excel("LepidopteranData-2024-7-2026.xlsx")

    # Rename columns and filter for Diatraea saccharalis in the "Cría" stage.
    # The "Status" column is used to filter out pending activities,
    # and new columns for year and week are created.
datos_diatraea <- data %>%
  rename(
    Actividad              = Activity,
    Especie               = Especie,
    Cantidad              = Cantidad,
    Target                = Target,
    Lote_produccion       = `Lote de Producción`,
    Notas                 = Notas,
    Status                = Status,
    Fecha_programada      = `Fecha programada`,
    Responsable           = Responsable,
    Fecha_realizacion     = `Fecha de realización`,
    Codigo_barras_lote    = `Valor de Código de Barras del Lote`,
    Etapa                 = Etapa
  ) %>%
  filter(
    Especie == "D. saccharalis",
    Etapa == "Cría",
    Status != "Pendiente"
  ) %>%
  mutate(
    year   = year(Fecha_realizacion),
    week   = isoweek(Fecha_realizacion)
  )

# Convert 'Cantidad' and 'Target' columns to numeric
datos_diatraea <- datos_diatraea %>%
  mutate(
    Cantidad = as.numeric(Cantidad),
    Target   = as.numeric(Target),
    Actividad = trimws(Actividad)
  )

# Clean data by removing rows with NA in 'Cantidad' and 'Activity'
# and filter out any rows where Viabilidad de huevos or Pupae Recovery are <0 and > 100
datos_diatraea <- datos_diatraea %>%
  filter(!is.na(Cantidad), !is.na(Actividad))
  # no me funcionó el filtro de 0-100% para las variables porcentuales %>%
  #filter(!(Activity == "Viabilidad de huevos" & Cantidad < 0 | Cantidad > 100), 
  #!(Activity == "Pupae Recovery" & Cantidad < 0 | Cantidad > 100))

# Summary statistics by activity, using the column Cantidad
resumen_diatraea <- bind_rows(
 datos_diatraea %>%
    filter(Actividad == "Inoculación") %>%
    summarise(
      Variable = "Huevos Inoculados",
      Media = mean(Cantidad, na.rm = TRUE),
      Desvio_estandar = sd(Cantidad, na.rm = TRUE),
      N = sum(!is.na(Cantidad))
    ),
 datos_diatraea %>%
    filter(Actividad == "Viabilidad de huevos") %>%
    summarise(
      Variable = "Viabilidad de huevos (%)",
      Media = mean(Cantidad, na.rm = TRUE),
      Desvio_estandar = sd(Cantidad, na.rm = TRUE),
      N = sum(!is.na(Cantidad))
    ),
  datos_diatraea %>%
    filter(Actividad == "Transferencia") %>%
    summarise(
      Variable = "Larvas Transferidas",
      Media = mean(Cantidad, na.rm = TRUE),
      Desvio_estandar = sd(Cantidad, na.rm = TRUE),
      N = sum(!is.na(Cantidad))
    ), 
 datos_diatraea %>%
    filter(Actividad == "Colecta de pupas") %>%
    summarise(
      Variable = "Pupas Colectadas",
      Media = mean(Cantidad, na.rm = TRUE),
      Desvio_estandar = sd(Cantidad, na.rm = TRUE),
      N = sum(!is.na(Cantidad))
    ),
 datos_diatraea %>%
    filter(Actividad == "Pupae Recovery") %>%
    summarise(
      Variable = "Pupae Recovery (%)",
      Media = mean(Cantidad, na.rm = TRUE),
      Desvio_estandar = sd(Cantidad, na.rm = TRUE),
      N = sum(!is.na(Cantidad))
    )
)

# View the results
resumen_diatraea

kable(resumen_diatraea, digits = 2, 
col.names = c("Variable", "Media", "Desvío estándar", "N"))

#Cálculo de número de adultos resultante

# Usamos un 60% de emergencia promedio de pupas a adultos, 
#basado en nuestros análisis (Biología de Lepidópteros, 2019)
adultos_por_lote <- datos_diatraea %>%
  filter(Actividad == "Colecta de pupas") %>%
  group_by(Lote_produccion) %>%
  summarise(
    # Se multiplica el número de pupas colectadas por 0.6 para estimar el número de adultos emergidos
    N_adultos = sum(Cantidad, na.rm = TRUE) * 0.6,
    .groups   = "drop"
  )
N_adultos_total <- sum(adultos_por_lote$N_adultos, na.rm = TRUE)
N_adultos_total
adultos_por_lote
# 2) Aproximación NeProducción(i) ≈ N_adultos(i)
ne_produccion_lote <- adultos_por_lote %>%
  mutate(
    Ne_produccion_i = N_adultos  # por ahora igual a N_adultos
  )

ne_produccion_lote

# Como no tengo Colecta de pupas 2° pero sí tengo el porcentaje de
#pupas recuperadas, puedo estimar el número de pupas recuperadas por 
#lote según larvas transferidas y luego multiplicarlo por 0.6 
#para obtener el número de adultos emergidos
Pupae_Recovery <- datos_diatraea %>%
  filter(Actividad == "Pupae Recovery") %>%
  group_by(Lote_produccion) %>%
  summarise(
    Pupae_Recovery = sum(Cantidad, na.rm = TRUE),
    .groups = "drop"
  )
# pupas = larvas transferidas * pupae recovery / 100
N_pupas_estimadas <- datos_diatraea %>%
  filter(Actividad == "Transferencia") %>%
  group_by(Lote_produccion) %>%
  summarise(
    N_larvas_transferidas = sum(Cantidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  left_join(N_pupas_recuperadas, by = "Lote_produccion") %>%
  mutate(
    N_pupas_estimadas = N_larvas_transferidas * (Pupae_Recovery / 100)
  )
