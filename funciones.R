# funciones.R
library(readxl)
library(dplyr)
library(stringr)
library(purrr)

cargar_afiliaciones_todos <- function() {
  csv_path <- "data/afiliaciones/"
  if (!dir.exists(csv_path)) {
    stop("❌ Carpeta 'data/afiliaciones/' no existe. Debe contener 32 archivos .xlsx, uno por estado.")
  }
  
  archivos <- list.files(csv_path, pattern = "\\.xlsx$", full.names = TRUE)
  
  if (length(archivos) == 0) {
    stop("❌ No se encontraron archivos .xlsx en 'data/afiliaciones/'.")
  }
  
  cat("📂 Cargando", length(archivos), "archivos de afiliaciones...\n")
  
  dfs <- map(archivos, ~ {
    nombre_archivo <- basename(.x)
    cve_estado <- str_sub(nombre_archivo, 1, 2)
    cve_estado <- as.character(as.numeric(cve_estado))
    
    estado_nombre_raw <- str_remove(nombre_archivo, "\\.xlsx$") %>% 
      str_remove("^\\d+_")
    
    estado_nombre_clean <- ifelse(
      estado_nombre_raw == "CDMX",
      "CDMX",
      normalizar_estado(estado_nombre_raw)
    )
    
    cat("  → Cargando:", estado_nombre_clean, " (cve_estado:", cve_estado, ") \n")
    
    df_excel <- read_excel(.x, sheet = 1, col_names = TRUE, skip = 0)
    
    # ✅ LIMPIAR NOMBRES DE COLUMNAS DE ESPACIOS
    names(df_excel) <- trimws(names(df_excel))
    
    expected_cols <- c(
      "id", "nombre", "sexo", "edad", "clave_elector", "email", "telefono",
      "cve_estado", "estado", "cve_distrito", "distrito", "cve_municipio",
      "municipio", "seccion", "estatus", "notas"
    )
    
    if (!all(expected_cols %in% names(df_excel))) {
      cat("❌ El archivo", basename(.x), "no tiene todas las columnas esperadas. Saltando.\n")
      return(NULL)
    }
    
    df_excel$cve_estado <- cve_estado
    df_excel$distrito_num <- df_excel$cve_distrito
    df_excel$estado <- estado_nombre_clean
    
    df_clean <- df_excel %>%
      mutate(
        across(all_of(c("id", "nombre", "sexo", "clave_elector", "email", "telefono", "distrito", "municipio", "notas")), ~ trimws(as.character(.))),
        estatus = trimws(estatus),
        edad = case_when(
          is.na(edad) ~ NA_integer_,
          trimws(edad) == "" ~ NA_integer_,
          str_detect(edad, "^\\d+$") ~ as.integer(trimws(edad)),
          TRUE ~ NA_integer_
        ),
        cve_distrito = as.character(cve_distrito),
        distrito_num = as.character(distrito_num)
      ) %>%
      select(id, nombre, sexo, edad, clave_elector, email, telefono, cve_estado, estado, cve_distrito, distrito_num, distrito, estatus, notas)
    
    return(df_clean)
  })
  
  dfs <- compact(dfs)
  if (length(dfs) == 0) {
    stop("❌ Ningún archivo de afiliaciones se cargó correctamente.")
  }
  
  df_total <- bind_rows(dfs)
  cat("✅ Total de afiliados cargados:", nrow(df_total), "\n")
  return(df_total)
}