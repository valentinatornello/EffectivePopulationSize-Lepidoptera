# Diatraea saccharalis (Lepidoptera: Crambidae) is a major pest of corn
# The following R code demonstrates how to analyze its population dynamics

library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(purrr)
library(readxl)
library(dplyr)
library(knitr)
library(ggplot2)

# Read new data: Progreso de Actividades Insectario from 2024
# Read original data: Cria de Lep from 2022
#data <- read_excel("LepidopteranData-2024-7-2026.xlsx")
data <- read_excel("CriadeLep-7-2026.xlsx")

head(data)

    # Rename columns and filter for Diatraea saccharalis in the "Cría" stage.
    # The "Status" column is used to filter out pending activities,
    # and new columns for year and week are created.
datos_diatraea <- data |>
  rename(Lote_produccion = `Lote de Produccion`) |>
  filter(
    Especie == "D. saccharalis",
    Etapa == "Cría",
    Fecha >= as.Date("2024-01-01")
  ) |>
  mutate(
    year = year(Fecha),
    week = isoweek(Fecha)
  )
datos_diatraea

# Convert 'Cantidad' 
datos_diatraea <- datos_diatraea %>%
  mutate(
    Cantidad = as.numeric(Cantidad),
    Actividad = trimws(Actividad),
    Lote_produccion = as.numeric(Lote_produccion)
  )

# Clean data by removing rows with NA in 'Cantidad' and 'Activity'
# and filter out any rows where Viabilidad de huevos or Pupae Recovery are <0 and > 100
datos_diatraea <- datos_diatraea %>%
  filter(!is.na(Cantidad), !is.na(Actividad))
  # no me funcionó el filtro de 0-100% para las variables porcentuales %>%
  #filter(!(Activity == "Viabilidad de huevos" & Cantidad < 0 | Cantidad > 100), 
  #!(Activity == "Pupae Recovery" & Cantidad < 0 | Cantidad > 100))

# Hacemos un gráfico con el QC de datos para ver cuántos descartamos
qc_plot <- datos_diatraea %>%
  group_by(Actividad) %>%
  summarise(
    total = n(),
    na_count = sum(is.na(Cantidad)),
    .groups = "drop"
  ) %>%
  mutate(
    na_percentage = (na_count / total) * 100
  )
qc_plot
ggplot(qc_plot, aes(x = Actividad, y = na_percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Porcentaje de datos faltantes por actividad",
    x = "Actividad",
    y = "Porcentaje de datos faltantes (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Calculamos el Pupae Recovery por lote de producción pupas/larvas*100
# Creamos la nueva variable Pupae_Recovery en el dataframe datos_diatraea
pupae_recovery <- datos_diatraea %>%
  filter(Actividad %in% c("Transferencia", "Colecta de pupas")) %>%
  group_by(Lote_produccion) %>%
  summarise(
    larvas_transferidas = sum(Cantidad[Actividad == "Transferencia"], na.rm = TRUE),
    pupas_colectadas = sum(Cantidad[Actividad == "Colecta de pupas"], na.rm = TRUE),
    Pupae_Recovery = ifelse(larvas_transferidas > 0, (pupas_colectadas / larvas_transferidas) * 100, NA_real_),
    .groups = "drop"
  )
pupae_recovery

# Diagnóstico: lotes con pupas pero sin larvas transferidas registradas
lotes_sin_transferencia <- pupae_recovery %>%
  filter(larvas_transferidas == 0, pupas_colectadas > 0)
message("Lotes con pupas pero larvas_transferidas = 0 
(revisar registro de Transferencia):")
print(lotes_sin_transferencia)
message("Lotes con Pupae_Recovery NA: ", 
sum(is.na(pupae_recovery$Pupae_Recovery)))

# Eliminamos los lotes que tengan NA pupae recovery
pupae_recovery <- pupae_recovery %>%
  filter(!is.na(Pupae_Recovery))
pupae_recovery

#Eliminamos también los lotes que tengan pupas colectadas pero 
#larvas transferidas = 0
datos_diatraea <- datos_diatraea %>%
  filter(!(Lote_produccion %in% lotes_sin_transferencia$Lote_produccion))

datos_diatraea

  # Calculamos la cantidad de lotes de producción que evaluamos
  lotes_evaluados <- datos_diatraea %>% group_by(Lote_produccion) %>% 
  summarise(n = n(), .groups = "drop") %>% nrow()
  lotes_evaluados


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
    # Se multiplica el número de pupas colectadas por 0.6 
    #para estimar el número de adultos emergidos
    N_adultos = sum(Cantidad, na.rm = TRUE) * 0.6,
    .groups   = "drop"
  )
adultos_por_lote

# Agregar una columna con el Pupae Recovery(%) por lote y el número total de adultos emergidos
datos_diatraea <- datos_diatraea %>%
  left_join(pupae_recovery, by = "Lote_produccion") %>%
  left_join(adultos_por_lote, by = "Lote_produccion")
datos_diatraea

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


# ============================================================
# Asume que ya tienes `datos_diatraea` creado y filtrado
# (Especie D. saccharalis, Etapa Cría, Status != Pendiente)
# ============================================================

# 1) Estandarizar tipos
df <- datos_diatraea %>%
  mutate(
    Fecha_realizacion = suppressWarnings(dmy(Fecha_realizacion)),
    Cantidad_num = parse_number(
      as.character(Cantidad),
      locale = locale(decimal_mark = ",", grouping_mark = ".")
    )
  )

# 2) Una fila por lote y actividad (último valor válido por fecha)
resumen_lote <- df %>%
  arrange(Lote_produccion, Actividad, Fecha_realizacion) %>%
  group_by(Lote_produccion, Actividad) %>%
  summarise(valor = dplyr::last(na.omit(Cantidad_num)), .groups = "drop") %>%
  pivot_wider(names_from = Actividad, values_from = valor) %>%
  rename(
    inoculacion        = `Inoculación`,
    viabilidad_huevos  = `Viabilidad de huevos`,
    larvas_transferidas = `Transferencia`,
    pupas_colectadas   = `Colecta de pupas`,
    pupae_recovery_obs = `Pupae Recovery`
  ) %>%
  mutate(
    # Pupae Recovery recalculado desde flujo real
    pupae_recovery_calc = if_else(
      !is.na(larvas_transferidas) & larvas_transferidas > 0 & !is.na(pupas_colectadas),
      (pupas_colectadas / larvas_transferidas) * 100,
      NA_real_
    ),
    # Adultos estimados (60% emergencia)
    adultos_estimados = if_else(
      !is.na(pupas_colectadas),
      pupas_colectadas * 0.60,
      NA_real_
    ),
    # Definimos NeProduccion como adultos estimados
    NeProduccion = adultos_estimados
  )

# 3) Integración armónica entre lotes (solo Ne > 0)
ne_validos <- resumen_lote %>%
  filter(!is.na(NeProduccion), NeProduccion > 0) %>%
  pull(NeProduccion)

if (length(ne_validos) == 0) {
  stop("No hay valores válidos de NeProduccion (>0). Revisa Colecta de pupas.")
}

t <- length(ne_validos)
NeProduccion_integrado <- t / sum(1 / ne_validos)

# 4) Resumen
resumen_integracion <- tibble(
  n_lotes_totales = n_distinct(resumen_lote$Lote_produccion),
  n_lotes_validos = t,
  Ne_media_aritmetica = mean(ne_validos),
  Ne_media_armonica = NeProduccion_integrado,
  penalizacion_por_lotes_bajos = mean(ne_validos) - NeProduccion_integrado
)

print(resumen_integracion)

# 5) Vista de control por lote
print(
  resumen_lote %>%
    select(
      Lote_produccion,
      inoculacion, viabilidad_huevos, larvas_transferidas,
      pupas_colectadas, pupae_recovery_obs, pupae_recovery_calc,
      adultos_estimados, NeProduccion
    ) %>%
    arrange(Lote_produccion)
)
# Imprimir todas las columnas porque faltan ℹ 5 more variables: pupas_colectadas <dbl>, pupae_recovery_obs <dbl>
print(
  resumen_lote %>%
    select(
      Lote_produccion,
      pupas_colectadas,
      adultos_estimados, NeProduccion
    ) %>%
    arrange(Lote_produccion)
)
library(dplyr)
library(ggplot2)
library(scales)
library(patchwork)


# Solo lotes con Ne válido
plot_df <- resumen_lote %>%
  filter(!is.na(NeProduccion), NeProduccion > 0) %>%
  arrange(Lote_produccion) %>%
  mutate(
    tipo = if_else(
      NeProduccion <= resumen_integracion$Ne_media_armonica[1],
      "≤ media armónica",
      "> media armónica"
    )
  )

ne_h <- resumen_integracion$Ne_media_armonica[1]
ne_a <- resumen_integracion$Ne_media_aritmetica[1]

# 1) Serie por lote
g1 <- ggplot(plot_df, aes(x = Lote_produccion, y = NeProduccion, color = tipo)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_line(alpha = 0.35) +
  geom_hline(yintercept = ne_h, linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = ne_a, linetype = "dotted", linewidth = 1) +
  scale_color_manual(values = c("≤ media armónica" = "#d73027", "> media armónica" = "#1a9850")) +
  labs(
    title = "NeProducción por lote (adultos estimados = pupas × 0.60)",
    subtitle = paste0("Media armónica = ", round(ne_h, 1),
                      " | Media aritmética = ", round(ne_a, 1)),
    x = "Lote de producción",
    y = "NeProducción",
    color = "Clasificación"
  ) +
  theme_minimal(base_size = 12)

# 2) Distribución
g2 <- ggplot(plot_df, aes(x = NeProduccion)) +
  geom_histogram(bins = 30, fill = "#2c7fb8", color = "white", alpha = 0.9) +
  geom_vline(xintercept = ne_h, linetype = "dashed", linewidth = 1) +
  geom_vline(xintercept = ne_a, linetype = "dotted", linewidth = 1) +
  labs(
    title = "Distribución de NeProducción entre lotes",
    x = "NeProducción",
    y = "Frecuencia"
  ) +
  theme_minimal(base_size = 12)

# 3) Boxplot
g3 <- ggplot(plot_df, aes(y = NeProduccion)) +
  geom_boxplot(fill = "#74add1", alpha = 0.8, outlier.alpha = 0.6) +
  geom_hline(yintercept = ne_h, linetype = "dashed", linewidth = 1) +
  geom_hline(yintercept = ne_a, linetype = "dotted", linewidth = 1) +
  labs(
    title = "Resumen robusto de NeProducción",
    y = "NeProducción",
    x = NULL
  ) +
  theme_minimal(base_size = 12)

# Panel final
panel <- (g1 / (g2 | g3)) +
  plot_annotation(
    title = "Diagnóstico visual de NeProducción integrada",
    subtitle = paste0(
      "Lotes válidos: ", nrow(plot_df),
      " de ", resumen_integracion$n_lotes_totales[1],
      " | Penalización (aritmética - armónica): ",
      round(resumen_integracion$penalizacion_por_lotes_bajos[1], 1)
    )
  )

print(panel)

# Guardar imagen
ggsave(
  filename = "neproduccion_diatraea_panel.png",
  plot = panel,
  width = 14, height = 9, dpi = 320
)