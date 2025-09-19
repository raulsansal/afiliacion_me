# funciones.R
library(readxl)    # ← ¡NUEVO PAQUETE PARA LEER EXCEL!
library(dplyr)
library(stringr)
library(purrr)

# ✅ NUEVA FUNCIÓN: Cargar TODOS los archivos .xlsx de afiliaciones por estado
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
    # Extraer nombre del archivo (ej: "09_CDMX.xlsx")
    nombre_archivo <- basename(.x)
    
    # ✅ EXTRAER cve_estado DE LOS PRIMEROS 2 CARACTERES (ej: "09_CDMX.xlsx" → "09" → "9")
    cve_estado <- str_sub(nombre_archivo, 1, 2)
    cve_estado <- as.character(as.numeric(cve_estado))  # "09" → 9 → "9"
    
    # Extraer nombre del estado (ej: "09_CDMX" → "CDMX")
    estado_nombre_raw <- str_remove(nombre_archivo, "\\.xlsx$") %>% 
      str_remove("^\\d+_")
    
    # Normalizar nombre del estado, pero con excepción para CDMX
    estado_nombre_clean <- ifelse(
      estado_nombre_raw == "CDMX",
      "CDMX",
      normalizar_estado(estado_nombre_raw)
    )
    
    cat("  → Cargando:", estado_nombre_clean, " (cve_estado:", cve_estado, ") \n")
    
    # 🔥 LEER EL ARCHIVO EXCEL (primera hoja, sin encabezado)
    df_excel <- read_excel(.x, sheet = 1, col_names = FALSE, skip = 0)
    
    # Verificar que tenga al menos 16 columnas
    if (ncol(df_excel) < 16) {
      cat("❌ El archivo", basename(.x), "tiene menos de 16 columnas. Saltando.\n")
      return(NULL)
    }
    
    # Renombrar columnas por posición (igual que antes)
    names(df_excel) <- c(
      "id", "nombre", "sexo", "edad", "clave_elector", "email", "telefono",
      "cve_estado", "estado", "cve_distrito", "distrito", "cve_municipio",
      "municipio", "seccion", "estatus", "notas"
    )
    
    # ✅ ¡FORZAMOS cve_estado DESDE EL NOMBRE DEL ARCHIVO! 🚨
    df_excel$cve_estado <- cve_estado
    
    # ✅ EXTRAER distrito_num desde cve_distrito (últimos 2 dígitos)
    df_excel$cve_distrito <- trimws(df_excel$cve_distrito)
    df_excel$distrito_num <- as.character(str_sub(df_excel$cve_distrito, -2, -1))
    
    # ✅ FORZAR estado_nombre_clean (no confiar en el Excel)
    df_excel$estado <- estado_nombre_clean
    
    # ✅ LIMPIEZA DE LOS DEMÁS CAMPOS — ¡CON VALIDACIÓN DE EDAD!
    df_clean <- df_excel %>%
      mutate(
        across(all_of(c("id", "nombre", "sexo", "clave_elector", "email", "telefono", "distrito", "municipio", "notas")), ~ trimws(as.character(.))),
        estatus = trimws(estatus),
        
        # ✅ ¡NUEVO: LIMPIAR Y VALIDAR EDAD SIN WARNINGS!
        edad = case_when(
          is.na(edad) ~ NA_integer_,
          trimws(edad) == "" ~ NA_integer_,
          str_detect(edad, "^\\d+$") ~ as.integer(trimws(edad)),   # Solo si es puro número
          TRUE ~ NA_integer_  # Cualquier otra cosa → NA
        )
      ) %>%
      select(id, nombre, sexo, edad, clave_elector, email, telefono, cve_estado, estado, cve_distrito, distrito_num, estatus, notas)
    
    return(df_clean)
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

# ✅ FUNCION CORREGIDA: generar_resumen() — AHORA USA SOLO cve_estado PARA ASIGNAR estado_nombre
generar_resumen <- function(df) {
  resumen <- df %>%
    group_by(cve_estado, distrito_num, estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(
      nivel = "distrital",
      # ✅ ASIGNAMOS estado_nombre DESDE cve_estado, IGUAL QUE EN global.R
      estado_nombre = case_when(
        cve_estado == "1" ~ "Aguascalientes",
        cve_estado == "2" ~ "Baja California",
        cve_estado == "3" ~ "Baja California Sur",
        cve_estado == "4" ~ "Campeche",
        cve_estado == "5" ~ "Coahuila",
        cve_estado == "6" ~ "Colima",
        cve_estado == "7" ~ "Chiapas",
        cve_estado == "8" ~ "Chihuahua",
        cve_estado == "9" ~ "CDMX",
        cve_estado == "10" ~ "Durango",
        cve_estado == "11" ~ "Guanajuato",
        cve_estado == "12" ~ "Guerrero",
        cve_estado == "13" ~ "Hidalgo",
        cve_estado == "14" ~ "Jalisco",
        cve_estado == "15" ~ "Mexico",
        cve_estado == "16" ~ "Michoacan",
        cve_estado == "17" ~ "Morelos",
        cve_estado == "18" ~ "Nayarit",
        cve_estado == "19" ~ "Nuevo Leon",
        cve_estado == "20" ~ "Oaxaca",
        cve_estado == "21" ~ "Puebla",
        cve_estado == "22" ~ "Queretaro",
        cve_estado == "23" ~ "Quintana Roo",
        cve_estado == "24" ~ "San Luis Potosi",
        cve_estado == "25" ~ "Sinaloa",
        cve_estado == "26" ~ "Sonora",
        cve_estado == "27" ~ "Tabasco",
        cve_estado == "28" ~ "Tamaulipas",
        cve_estado == "29" ~ "Tlaxcala",
        cve_estado == "30" ~ "Veracruz",
        cve_estado == "31" ~ "Yucatan",
        cve_estado == "32" ~ "Zacatecas",
        TRUE ~ "Desconocido"
      )
    ) %>%
    select(cve_estado, distrito_num, estado_nombre, estatus, total, nivel)
  
  resumen_estatal <- df %>%
    group_by(cve_estado, estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(
      nivel = "estatal",
      estado_nombre = case_when(
        cve_estado == "1" ~ "Aguascalientes",
        cve_estado == "2" ~ "Baja California",
        cve_estado == "3" ~ "Baja California Sur",
        cve_estado == "4" ~ "Campeche",
        cve_estado == "5" ~ "Coahuila",
        cve_estado == "6" ~ "Colima",
        cve_estado == "7" ~ "Chiapas",
        cve_estado == "8" ~ "Chihuahua",
        cve_estado == "9" ~ "CDMX",
        cve_estado == "10" ~ "Durango",
        cve_estado == "11" ~ "Guanajuato",
        cve_estado == "12" ~ "Guerrero",
        cve_estado == "13" ~ "Hidalgo",
        cve_estado == "14" ~ "Jalisco",
        cve_estado == "15" ~ "Mexico",
        cve_estado == "16" ~ "Michoacan",
        cve_estado == "17" ~ "Morelos",
        cve_estado == "18" ~ "Nayarit",
        cve_estado == "19" ~ "Nuevo Leon",
        cve_estado == "20" ~ "Oaxaca",
        cve_estado == "21" ~ "Puebla",
        cve_estado == "22" ~ "Queretaro",
        cve_estado == "23" ~ "Quintana Roo",
        cve_estado == "24" ~ "San Luis Potosi",
        cve_estado == "25" ~ "Sinaloa",
        cve_estado == "26" ~ "Sonora",
        cve_estado == "27" ~ "Tabasco",
        cve_estado == "28" ~ "Tamaulipas",
        cve_estado == "29" ~ "Tlaxcala",
        cve_estado == "30" ~ "Veracruz",
        cve_estado == "31" ~ "Yucatan",
        cve_estado == "32" ~ "Zacatecas",
        TRUE ~ "Desconocido"
      )
    ) %>%
    select(cve_estado, estado_nombre, estatus, total, nivel)
  
  resumen_nacional <- df %>%
    group_by(estatus) %>%
    summarise(total = n(), .groups = 'drop') %>%
    mutate(
      nivel = "nacional",
      estado_nombre = "Nacional"
    ) %>%
    select(estado_nombre, estatus, total, nivel)
  
  resumen_completo <- bind_rows(resumen_nacional, resumen_estatal, resumen)
  return(resumen_completo)
}