# app.R

library(readr)
library(dplyr)
library(stringr)

# Procesar el archivo CSV de afiliaciones
procesar_afiliaciones <- function(file_path) {
  df <- read_csv(file_path, col_types = cols(
    id = col_character(),
    nombre = col_character(),
    sexo = col_character(),
    edad = col_integer(),
    clave_elector = col_character(),
    email = col_character(),
    telefono = col_character(),
    cve_estado = col_character(),
    estado = col_character(),
    cve_distrito = col_character(),
    distrito = col_character(),
    cve_municipio = col_character(),
    municipio = col_character(),
    seccion = col_character(),
    estatus = col_character(),
    notas = col_character()
  ))
  
  # Limpieza
  df <- df %>%
    mutate(
      cve_estado = trimws(cve_estado),
      cve_distrito = trimws(cve_distrito),
      distrito_num = as.numeric(str_sub(cve_distrito, 3, 4)),  # Extrae últimos 2 dígitos
      estado = str_to_title(estado),  # Normaliza "cdmx" → "Cdmx"
      estatus = trimws(estatus)
    ) %>%
    filter(!is.na(cve_estado) & !is.na(distrito_num))
  
  return(df)
}

# Agregaciones por nivel
generar_resumen <- function(df) {
  resumen <- df %>%
    group_by(cve_estado, distrito_num, estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(nivel = "distrital")
  
  resumen_estatal <- df %>%
    group_by(cve_estado, estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(nivel = "estatal")
  
  resumen_nacional <- df %>%
    group_by(estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(nivel = "nacional")
  
  resumen_completo <- bind_rows(resumen_nacional, resumen_estatal, resumen)
  return(resumen_completo)
}