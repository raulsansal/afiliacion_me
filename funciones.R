# funciones.R
library(readr)
library(dplyr)
library(stringr)

# ✅ NUEVA FUNCIÓN: Cargar TODOS los CSV de afiliaciones por estado
cargar_afiliaciones_todos <- function() {
  csv_path <- "data/afiliaciones/"
  if (!dir.exists(csv_path)) {
    stop("❌ Carpeta 'data/afiliaciones/' no existe. Debe contener 32 archivos .csv, uno por estado.")
  }
  
  archivos <- list.files(csv_path, pattern = "\\.csv$", full.names = TRUE)
  
  if (length(archivos) == 0) {
    stop("❌ No se encontraron archivos .csv en 'data/afiliaciones/'.")
  }
  
  cat("📂 Cargando", length(archivos), "archivos de afiliaciones...\n")
  
  dfs <- map(archivos, ~ {
    # Extraer nombre del estado desde el nombre del archivo (ej: "09_CDMX.csv")
    estado_nombre_raw <- basename(.x) %>% 
      str_remove("\\.csv$") %>% 
      str_remove("^\\d+_")
    
    # Normalizar nombre del estado, pero con excepción para CDMX
    estado_nombre_clean <- ifelse(
      estado_nombre_raw == "CDMX",
      "CDMX",
      normalizar_estado(estado_nombre_raw)
    )
    
    cat("  → Cargando:", estado_nombre_clean, "\n")
    
    # 🔥 LECTURA ROBUSTA: Intenta leer con col_types, si falla, repara automáticamente
    df <- tryCatch({
      read_csv(
        .x,
        col_types = cols(
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
        ),
        locale = locale(encoding = "UTF-8"),
        show_col_types = FALSE
      )
    }, error = function(e) {
      # Si falla, intentamos leerlo como texto y forzar las columnas
      cat("⚠️  Error al leer con col_types. Intentando reparar archivo manualmente...\n")
      
      lines <- readLines(.x, warn = FALSE)
      header <- str_split_fixed(lines[1], ",", n = 16)
      data_lines <- lapply(lines[-1], function(x) str_split_fixed(x, ",", n = 16))
      data_matrix <- do.call(rbind, data_lines)
      df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
      names(df) <- as.character(header)
      return(df)
    })
    
    # Asegurar que tiene las 16 columnas correctas
    expected_cols <- c(
      "id", "nombre", "sexo", "edad", "clave_elector", "email", "telefono",
      "cve_estado", "estado", "cve_distrito", "distrito", "cve_municipio",
      "municipio", "seccion", "estatus", "notas"
    )
    
    if (ncol(df) != length(expected_cols)) {
      cat("❌ El archivo", basename(.x), "tiene", ncol(df), "columnas, se esperaban", length(expected_cols), "\n")
      return(NULL)
    }
    
    # Renombrar columnas si están desordenadas o mal nombradas
    names(df) <- expected_cols
    
    # Limpieza y normalización
    df <- df %>%
      mutate(
        across(all_of(c("id", "nombre", "sexo", "clave_elector", "email", "telefono", "estado", "distrito", "municipio", "notas")), ~ trimws(as.character(.))),
        cve_estado = trimws(cve_estado),
        cve_distrito = trimws(cve_distrito),
        distrito_num = str_sub(cve_distrito, -2, -1),  # ✅ Últimos 2 caracteres (funciona con "922", "1501", etc.)
        estado = estado_nombre_clean,
        estatus = trimws(estatus),
        edad = as.integer(edad)
      ) %>%
      select(id, nombre, sexo, edad, clave_elector, email, telefono, cve_estado, estado, cve_distrito, distrito_num, estatus, notas)
    
    return(df)
  })
  
  # Filtrar solo los que se cargaron bien
  dfs <- compact(dfs)
  
  if (length(dfs) == 0) {
    stop("❌ Ningún archivo de afiliaciones se cargó correctamente.")
  }
  
  # Combinar todos los dataframes
  df_total <- bind_rows(dfs)
  cat("✅ Total de afiliados cargados:", nrow(df_total), "\n")
  return(df_total)
}

# ✅ Función de agregación (sin cambios, ya funcionaba perfectamente)
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