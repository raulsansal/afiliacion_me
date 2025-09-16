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
    
    # 🔥 Intentar leer con col_types (forma ideal)
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
      # Si falla, intentar leer como texto y reconstruir
      cat("⚠️  Error al leer con col_types. Intentando reparar archivo manualmente...\n")
      
      lines <- readLines(.x, warn = FALSE)
      
      if (length(lines) == 0) return(NULL)
      
      header_line <- lines[1]
      if (!grepl(",", header_line)) {
        cat("❌ El archivo", basename(.x), "no tiene formato CSV válido.\n")
        return(NULL)
      }
      
      header_cols <- str_split_fixed(header_line, ",", n = 16)
      if (length(header_cols) != 16) {
        cat("❌ Encabezado del archivo", basename(.x), "tiene", length(header_cols), "columnas, se esperaban 16.\n")
        return(NULL)
      }
      
      data_lines <- lapply(lines[-1], function(x) {
        if (x == "") return(rep(NA, 16))
        str_split_fixed(x, ",", n = 16)
      })
      
      data_matrix <- do.call(rbind, data_lines)
      df <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
      names(df) <- as.character(header_cols)
      return(df)
    })
    
    # Validar número de columnas
    expected_cols <- c(
      "id", "nombre", "sexo", "edad", "clave_elector", "email", "telefono",
      "cve_estado", "estado", "cve_distrito", "distrito", "cve_municipio",
      "municipio", "seccion", "estatus", "notas"
    )
    
    if (is.null(df) || ncol(df) != length(expected_cols)) {
      cat("❌ El archivo", basename(.x), "no tiene 16 columnas válidas. Saltando.\n")
      return(NULL)
    }
    
    # Renombrar columnas si están mal
    names(df) <- expected_cols
    
    # Limpieza y normalización
    df <- df %>%
      mutate(
        across(all_of(c("id", "nombre", "sexo", "clave_elector", "email", "telefono", "estado", "distrito", "municipio", "notas")), ~ trimws(as.character(.))),
        cve_estado = trimws(cve_estado),
        cve_distrito = trimws(cve_distrito),
        distrito_num = str_sub(cve_distrito, -2, -1),  # ✅ Últimos 2 caracteres
        estado = estado_nombre_clean,  # ← Aquí asignamos el nombre limpio
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

# ✅ FUNCION CORREGIDA: generar_resumen() — AHORA INCLUYE estado_nombre
generar_resumen <- function(df) {
  resumen <- df %>%
    group_by(cve_estado, distrito_num, estado, estatus) %>%  # ✅ AÑADIMOS "estado" aquí
    summarise(total = n(), .groups = 'drop') %>%
    mutate(
      nivel = "distrital",
      estado_nombre = estado  # ✅ AÑADIMOS ESTA COLUMNA EXPLÍCITA
    ) %>%
    select(-estado)  # Opcional: eliminar "estado" si no quieres duplicación
  
  resumen_estatal <- df %>%
    group_by(cve_estado, estado, estatus) %>%  # ✅ AÑADIMOS "estado" aquí
    summarise(total = n(), .groups = 'drop') %>%
    mutate(
      nivel = "estatal",
      estado_nombre = estado  # ✅ AÑADIMOS ESTA COLUMNA EXPLÍCITA
    ) %>%
    select(-estado)
  
  resumen_nacional <- df %>%
    group_by(estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(
      nivel = "nacional",
      estado_nombre = "Nacional"  # ✅ Para que tenga sentido en filtro
    )
  
  resumen_completo <- bind_rows(resumen_nacional, resumen_estatal, resumen)
  return(resumen_completo)
}