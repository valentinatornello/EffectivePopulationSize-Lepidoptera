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
install.packages("tinytex")
# Read new data: Progreso de Actividades Insectario from 2024
# Read original data: Cria de Lep from 2022
#data <- read_excel("LepidopteranData-2024-7-2026.xlsx")
data <- read_excel("CriadeLep-7-2026.xlsx")

head(data)
rmarkdown::render("Diatraea saccharalis Ne.Rmd")
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

# Convert numeric columns as numeric 
datos_diatraea <- datos_diatraea %>%
  mutate(
    Cantidad = as.numeric(Cantidad),
    Actividad = trimws(Actividad),
    Lote_produccion = as.numeric(Lote_produccion)
  )
  library(knitr)
  rmarkdown::render("Diatraea saccharalis Ne.Rmd")

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

  #Eliminamos los lotes que tengan pupae recovery>100%
  datos_diatraea <- datos_diatraea %>%
  filter(!(Lote_produccion %in% pupae_recovery$Lote_produccion[pupae_recovery$Pupae_Recovery > 100]))

datos_diatraea

  # Calculamos la cantidad de lotes de producción que evaluamos
  lotes_evaluados <- datos_diatraea %>% group_by(Lote_produccion) %>% 
  summarise(n = n(), .groups = "drop") %>% nrow()
  lotes_evaluados


# Summary statistics by activity, using the column Cantidad
resumen_diatraea <- bind_rows(
  datos_diatraea %>%
    filter(Actividad == "Lavado y número de huevos") %>%
    summarise(
      Variable = "Huevos Lavados",
      Media = mean(Cantidad, na.rm = TRUE),
      Desvio_estandar = sd(Cantidad, na.rm = TRUE),
      N = sum(!is.na(Cantidad))
    ),
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

# Borrar las columnas Armado de jaulas y Cambio de jaulas, pues no aportan información relevante para el cálculo de Ne
datos_diatraea <- datos_diatraea %>%
  filter(!Actividad %in% c("Armado de jaulas", "Cambio de jaulas"))

# Tabla ancha: una fila por lote, columnas por actividad (suma de Cantidad), más Pupae Recovery y Adultos emergidos
datos_diatraea_transpose <- datos_diatraea %>%
  select(Lote_produccion, Actividad, Cantidad) %>%
  group_by(Lote_produccion, Actividad) %>%
  summarise(Cantidad = sum(Cantidad, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = Actividad, values_from = Cantidad) %>%
  mutate(
    `Pupae Recovery` = if_else(
      !is.na(Transferencia) & Transferencia > 0 & !is.na(`Colecta de pupas`),
      (`Colecta de pupas` / Transferencia) * 100,
      NA_real_
    ),
    `Adultos emergidos` = if_else(
      !is.na(`Colecta de pupas`),
      `Colecta de pupas` * 0.60,
      NA_real_
    )
  )
datos_diatraea_transpose

# Suponiendo que el sex ratio es 1:1, podemos estimar Ne con un estimado N hembras y N machos
# Creamos columna N hembras, y N machos, y luego Ne = 4*Nf*Nm/(Nf+Nm)
datos_diatraea_transpose <- datos_diatraea_transpose %>%
  mutate(
    N_hembras = `Adultos emergidos` / 2,
    N_machos = `Adultos emergidos` / 2,
    Ne = if_else(
      !is.na(N_hembras) & !is.na(N_machos) & (N_hembras + N_machos) > 0,
      (4 * N_hembras * N_machos) / (N_hembras + N_machos),
      NA_real_
    )
  )
  # Resumen media y DE de datos_diatraea_transpose
resumen_diatraea_transpose <- datos_diatraea_transpose %>%
  summarise(
    across(
      c(Transferencia, `Colecta de pupas`, `Pupae Recovery`, `Adultos emergidos`, N_hembras, N_machos, Ne),
      list(Media = ~mean(.x, na.rm = TRUE), DE = ~sd(.x, na.rm = TRUE)),
      .names = "{.col}_{.fn}"
    )
  )
resumen_diatraea_transpose

# Mostrar toda la tabla completa de resumen_diatraea_transpose
kable(resumen_diatraea_transpose, digits = 2,
      col.names = c(
        "Transferencia Media", "Transferencia DE",
        "Colecta de pupas Media", "Colecta de pupas DE",
        "Pupae Recovery Media", "Pupae Recovery DE",
        "Adultos emergidos Media", "Adultos emergidos DE",
        "N hembras Media", "N hembras DE",
        "N machos Media", "N machos DE",
        "Ne Media", "Ne DE"
      ))

 # Agregar columna año de la base original a datos_diatraea_transpose, para poder hacer gráficos de tendencia a lo largo de los años
datos_diatraea_transpose <- datos_diatraea_transpose %>%
  left_join(datos_diatraea %>% select(Lote_produccion, year) %>% distinct(), by = "Lote_produccion")

datos_diatraea_transpose

# Obtener el n muestral para realizar una investigación de cada variable
# de una muestra para D. saccharalis, con un nivel de confianza del 90%
 n_muestral <- function(sd, error, z_score = 1.96) {
  n <- (z_score^2 * sd^2) / (error^2)
  return(ceiling(n))
}

#Obtener el n muestral para realizar una investigación de cada variable
#NC 93%, Z = 1.81
n_muestral_93 <- function(sd, error, z_score = 1.81) {
  n <- (z_score^2 * sd^2) / (error^2)
  return(ceiling(n))
}
#NC 90%, Z = 1.645
n_muestral_90 <- function(sd, error, z_score = 1.645) {
  n <- (z_score^2 * sd^2) / (error^2)
  return(ceiling(n))
}
# NC 85%, Z = 1.44
n_muestral_85 <- function(sd, error, z_score = 1.44) {
  n <- (z_score^2 * sd^2) / (error^2)
  return(ceiling(n))
}

#Para evaluar la viabilidad de huevos, con un error del 5%
sd_viabilidad_huevos <- resumen_diatraea %>%
  filter(Variable == "Viabilidad de huevos (%)") %>%
  pull(Desvio_estandar)
n_viabilidad_huevos <- n_muestral(sd_viabilidad_huevos, error = 5)

message("Tamaño de muestra necesario para evaluar la viabilidad de huevos (error 5%): ", n_viabilidad_huevos)
# Para evaluar la viabilidad de huevos, con un error del 7%
n_viabilidad_huevos93 <- n_muestral_93(sd_viabilidad_huevos, error = 7)
message("Tamaño de muestra necesario para evaluar la viabilidad de huevos (error 7%): ", 
n_viabilidad_huevos) 

# Para evaluar la transferencia de larvas, con un error del 5%, 95% IC
sd_transferencia <- resumen_diatraea %>%
  filter(Variable == "Larvas Transferidas") %>%
  pull(Desvio_estandar)

n_transferencia <- n_muestral(sd_transferencia, error = 5)
message("Tamaño de muestra necesario para evaluar la transferencia de larvas (error 5%): ", 
n_transferencia) 

# Para evaluar la transferencia de larvas, con un error del 7%, 93% IC
n_transferencia_93 <- n_muestral_93(sd_transferencia, error = 7)
message("Tamaño de muestra necesario para evaluar la transferencia de larvas (error 7%): ", 
n_transferencia_93)

# Para evaluar la transferencia de larvas, con un error del 10%, 90% IC
n_transferencia_90 <- n_muestral_90(sd_transferencia, error = 10)
message("Tamaño de muestra necesario para evaluar la transferencia de larvas (error 10%): ", 
n_transferencia_90)

# Para evaluar la transferencia de larvas, con un error del 15%, 85% IC
n_transferencia_85 <- n_muestral_85(sd_transferencia, error = 15)
message("Tamaño de muestra necesario para evaluar la transferencia de larvas (error 15% ): ", 
n_transferencia_85)

# Para evaluar la colecta de pupas, con un error del 5%, 95% IC
sd_colecta_pupas <- resumen_diatraea %>%
  filter(Variable == "Pupas Colectadas") %>%
  pull(Desvio_estandar)
n_colecta_pupas <- n_muestral(sd_colecta_pupas, error = 5)
message("Tamaño de muestra necesario para evaluar la colecta de pupas (error 5%): ", n_colecta_pupas)

# Para evaluar la colecta de pupas, con un error del 7%, 93% IC
n_colecta_pupas_93 <- n_muestral_93(sd_colecta_pupas, error = 7)
message("Tamaño de muestra necesario para evaluar la colecta de pupas (error 7%): ", n_colecta_pupas_93)

# Para evaluar el número de adultos emergidos, con un error del 5%, 95% IC
sd_adultos_emergidos <- resumen_diatraea_transpose %>%
  pull(`Adultos emergidos_DE`)
n_adultos_emergidos <- n_muestral(sd_adultos_emergidos, error = 5)
message("Tamaño de muestra necesario para evaluar el número de adultos emergidos (error 5%): ", 
n_adultos_emergidos)

# Para evaluar el número de adultos emergidos, con un error del 7%, 93% IC
n_adultos_emergidos_93 <- n_muestral_93(sd_adultos_emergidos, error = 7)
message("Tamaño de muestra necesario para evaluar el número de adultos emergidos (error 7%): ", n_adultos_emergidos_93)

# Hacer una tabla resumen de los tamaños de muestra necesarios para cada variable, con NC 95%, 93% y 90%
tamanos_muestra <- tibble(
  Variable = c(
    "Viabilidad de huevos (%)",
    "Larvas Transferidas",
    "Pupas Colectadas",
    "Adultos emergidos"
  ),
  SD = c(
    sd_viabilidad_huevos,
    sd_transferencia,
    sd_colecta_pupas,
    sd_adultos_emergidos
  ),
  Error = c(7, 5, 5, 5),
  Tamaño_muestra_95_IC = c(
    n_viabilidad_huevos,
    n_transferencia,
    n_colecta_pupas,
    n_adultos_emergidos
  ),
  Tamaño_muestra_93_IC = c(
    n_viabilidad_huevos93,
    n_transferencia_93,
    n_colecta_pupas_93,
    n_adultos_emergidos_93
  ),
  Tamaño_muestra_90_IC = c(
    NA,
    n_transferencia_90,
    NA,
    NA
  )
)
# Mostrar la tabla resumen de los tamaños de muestra COMPLETA
kable(tamanos_muestra, digits = 2,
      col.names = c(
        "Variable", "Desvío estándar", "Error (%)",
        "Tamaño de muestra (95% IC)", "Tamaño de muestra (93% IC)", "Tamaño de muestra (90% IC)"
      ))

# 1) Dispersión: Larvas transferidas vs Pupas colectadas
p1 <- ggplot(datos_diatraea_transpose, aes(x = Transferencia, y = `Colecta de pupas`)) +
  geom_point(color = "#2c7fb8", size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d73027") +
  stat_regline_equation(aes(label = after_stat(eq.label)), label.x.npc = "left", label.y.npc = "top") +
  stat_cor(aes(label = after_stat(rr.label)), label.x.npc = "left", label.y.npc = 0.88) +
  labs(title = "Larvas transferidas vs Pupas colectadas",
       x = "Larvas transferidas", y = "Pupas colectadas") +
  theme_minimal(base_size = 12)
p1
# 2) Dispersión: Pupas colectadas vs Adultos emergidos
p2 <- ggplot(datos_diatraea_transpose, aes(x = `Colecta de pupas`, y = `Adultos emergidos`)) +
  geom_point(color = "#1a9850", size = 2.5, alpha = 0.8) +
  geom_smooth(method = "lm", se = TRUE, color = "#d73027") +
  stat_regline_equation(aes(label = after_stat(eq.label)), label.x.npc = "left", label.y.npc = "top") +
  stat_cor(aes(label = after_stat(rr.label)), label.x.npc = "left", label.y.npc = 0.88) +
  labs(title = "Pupas colectadas vs Adultos emergidos",
       x = "Pupas colectadas", y = "Adultos emergidos") +
  theme_minimal(base_size = 12)
p2
# 3) Distribución de Pupae Recovery (%)
p3 <- ggplot(datos_diatraea_transpose, aes(x = `Pupae Recovery`)) +
  geom_histogram(bins = 20, fill = "#74add1", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean(datos_diatraea_transpose$`Pupae Recovery`, na.rm = TRUE),
             linetype = "dashed", color = "#d73027", linewidth = 1) +
  labs(title = "Distribución de Pupae Recovery (%)",
       x = "Pupae Recovery (%)", y = "Frecuencia") +
  theme_minimal(base_size = 12)
p3
# 4) Ne por lote con media armónica superpuesta
ne_harm <- datos_diatraea_transpose %>%
  filter(!is.na(Ne), Ne > 0) %>%
  { nrow(.) / sum(1 / .$Ne) }
ne_harm

# 5) Distribución de Viabilidad de huevos (%) (NORMAL)
p4 <- datos_diatraea %>%
  filter(Actividad == "Viabilidad de huevos") %>%
  ggplot(aes(x = Cantidad)) +
  geom_histogram(bins = 20, fill = "#fdae61", color = "white", alpha = 0.9) +
  geom_vline(xintercept = mean(datos_diatraea$Cantidad[datos_diatraea$Actividad == "Viabilidad de huevos"],
                               na.rm = TRUE),
             linetype = "dashed", color = "#d73027", linewidth = 1) +
  labs(title = "Distribución de Viabilidad de huevos (%)",
       x = "Viabilidad (%)", y = "Frecuencia") +
  theme_minimal(base_size = 12)
p4
# 6) Boxplots comparativos de variables clave
p6 <- datos_diatraea_transpose %>%
  select(Lote_produccion, Transferencia, `Colecta de pupas`, `Adultos emergidos`) %>%
  tidyr::pivot_longer(-Lote_produccion, names_to = "Variable", values_to = "Valor") %>%
  ggplot(aes(x = Variable, y = Valor, fill = Variable)) +
  geom_boxplot(alpha = 0.8, outlier.alpha = 0.5) +
  scale_fill_manual(values = c("#2c7fb8", "#1a9850", "#fdae61")) +
  labs(title = "Distribución de variables clave entre lotes",
       x = NULL, y = "Cantidad") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))
p6

# Reducción del tamaño poblacional a través del ciclo de cría de D. saccharalis, considerando el número efectivo de cada variable
# Gráfico de barras de cada variable con su Ne promedio y la media armónica superpuesta
ne_promedio <- resumen_diatraea_transpose %>%
  summarise(
    Transferencia = `Transferencia_Media`,
    `Colecta de pupas` = `Colecta de pupas_Media`,
    `Adultos emergidos` = `Adultos emergidos_Media`,
    Ne = `Ne_Media`
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Ne") 
ne_promedio

#Gráfico agregando las etiquetas de valores de Ne promedio encima de cada barra y el Ne armónica como línea punteada
grafico_ne <- ggplot(ne_promedio, aes(x = Variable, y = Ne, fill = Variable)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Ne, 1)), vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = ne_harm, linetype = "dashed", color = "#d73027", linewidth = 1) +
  annotate("text", x = Inf, y = ne_harm, label = paste0("Ne arm. = ", round(ne_harm, 1)),
           hjust = 1.1, vjust = -0.4, color = "#d73027", size = 3.5) +
  labs(title = "Reducción del tamaño poblacional a través del ciclo de cría de D. saccharalis",
       x = "Variable", y = "Ne promedio") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

grafico_ne


# Repetimos el gráfico para incluir huevos que tienen valores muy altos
#calcular la media de huevos lavados
Huevos_Lavados_Media <- resumen_diatraea %>%
  filter(Variable == "Huevos Lavados") %>%
  pull(Media)
  Huevos_Inoculados_Media <- resumen_diatraea %>%
  filter(Variable == "Huevos Inoculados") %>% 
  pull(Media)

  #Calculamos el número de larvas eclosionadas según el porcentaje de viabilidad de huevos por lote de producción, luego la media
  larvas_eclosionadas <- datos_diatraea_transpose %>%
    mutate(
      larvas_eclosionadas = Transferencia * (`Viabilidad de huevos (%)` / 100)
    ) %>%
    summarise(larvas_eclosionadas_media = mean(larvas_eclosionadas, na.rm = TRUE)) %>%
    pull(larvas_eclosionadas_media)

# Gráfico de barras de cada variable con su Ne promedio y la media armónica superpuesta
ne_promedio <- resumen_diatraea_transpose %>%
  summarise(
    Huevos = Huevos_Lavados_Media,
   `Larvas Eclosionadas` = larvas_eclosionadas_media,
    `Huevos Inoculados` = Huevos_Inoculados_Media,
    Ne = `Ne_Media`
  ) %>%
  pivot_longer(everything(), names_to = "Variable", values_to = "Ne") 
ne_promedio


#Gráfico agregando las etiquetas de valores de Ne promedio encima de cada barra y el Ne armónica como línea punteada
grafico_ne <- ggplot(ne_promedio, aes(x = Variable, y = Ne, fill = Variable)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = round(Ne, 1)), vjust = -0.5, size = 3.5) +
  geom_hline(yintercept = ne_harm, linetype = "dashed", color = "#d73027", linewidth = 1) +
  annotate("text", x = Inf, y = ne_harm, label = paste0("Ne arm. = ", round(ne_harm, 1)),
           hjust = 1.1, vjust = -0.4, color = "#d73027", size = 3.5) +
  labs(title = "Reducción del tamaño poblacional a través del ciclo de cría de D. saccharalis",
       x = "Variable", y = "Ne promedio") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 20, hjust = 1))

grafico_ne

# Gráfico a lo largo de los años, para ver la tendencia de Huevos Lavados, Transferencia, Coleta de pupas y Adultos emergidos 
#(siempre agrupando por lote de producción y sumando la cantidad de cada actividad)
# FILTRAR SOLO ESAS 4 VARIABLES, Y AGRUPAR POR AÑO, LOTE DE PRODUCCIÓN Y ACTIVIDAD, SUMANDO LA CANTIDAD
years_trend <- datos_diatraea_transpose %>%
  select(Lote_produccion, year, Transferencia, `Colecta de pupas`, `Adultos emergidos`) %>%
  pivot_longer(cols = c(Transferencia, `Colecta de pupas`, `Adultos emergidos`), 
               names_to = "Actividad", values_to = "Cantidad") %>%
  group_by(year, Actividad) %>%
  summarise(Cantidad = sum(Cantidad, na.rm = TRUE), .groups = "drop")

# ¿Qué sucede con la población si yo mantengo el Ne de adultos propuesto por varias generaciones?
# ¿Cuál es mi endocría?
#Finalmente, podemos calcular la endogamia esperada (F) después de varias generaciones usando la fórmula F = 1 - (1 - 1/(2*Ne))^t, donde t es el número de generaciones. Esto nos permitirá evaluar cómo la reducción del tamaño efectivo de la población afecta la diversidad genética a lo largo del tiempo.
endogamia_esperada <- function(Ne, t) {
  F <- 1 - (1 - 1/(2*Ne))^t
  return(F)
}
endogamia_esperada(Ne = ne_harm, t = 10)  # Por ejemplo, después de 10 generaciones

# 0.02895238 significa que después de 10 generaciones, la endogamia esperada es de aproximadamente 2.9%, lo que indica una pérdida moderada de diversidad genética.

# Tabla de endogamia esperada para diferentes valores de Ne y número de generaciones
ne_values <- c(10, 20, 50, 100, 200)

generations <- c(1, 5, 10, 20, 50)

endogamia_table <- expand.grid(Ne = ne_values, Generations = generations) %>%
  mutate(F = endogamia_esperada(Ne, Generations))

endogamia_table

tinytex::install_tinytex()
rmarkdown::render("Diatraea saccharalis Ne.Rmd")