# global.R
# 📦 CARGAR PAQUETES NECESARIOS (¡EXPLÍCITAMENTE!)
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(foreign)
library(readxl)  # ← ¡NUEVO PAQUETE PARA LEER EXCEL!

# Configuración
shape_path <- "afiliacion_shp"
CRS_OBJ <- st_crs("EPSG:4326")

# Ruta del archivo de metas estatales
metas_path <- "data/metas_estatales.xlsx"

# 🔧 Función para normalizar nombres de estados (sin acentos, ñ, espacios)
normalizar_estado <- function(nombre) {
  nombre %>%
    str_replace_all("á", "a") %>%
    str_replace_all("é", "e") %>%
    str_replace_all("í", "i") %>%
    str_replace_all("ó", "o") %>%
    str_replace_all("ú", "u") %>%
    str_replace_all("ñ", "n") %>%
    str_replace_all("Á", "A") %>%
    str_replace_all("É", "E") %>%
    str_replace_all("Í", "I") %>%
    str_replace_all("Ó", "O") %>%
    str_replace_all("Ú", "U") %>%
    str_replace_all("Ñ", "N") %>%
    str_replace_all("\\s+", "_") %>%   # Reemplaza espacios múltiples por _
    str_remove("^_+") %>%              # Elimina guiones bajos iniciales
    str_remove("_+$")                  # Elimina guiones bajos finales
}

# 📌 LISTA ESPERADA DE LOS 32 ESTADOS (formato final: sin acentos, CDMX, etc.)
expected_states <- c(
  "Aguascalientes", "Baja_California", "Baja_California_Sur", "Campeche",
  "Coahuila", "Colima", "Chiapas", "Chihuahua", "CDMX",
  "Durango", "Guanajuato", "Guerrero", "Hidalgo", "Jalisco",
  "Mexico", "Michoacan", "Morelos", "Nayarit", "Nuevo_Leon",
  "Oaxaca", "Puebla", "Queretaro", "Quintana_Roo", "San_Luis_Potosi",
  "Sinaloa", "Sonora", "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz",
  "Yucatan", "Zacatecas"
)

# Cargar metas estatales (si existe)
if (file.exists(metas_path)) {
  cat("✅ Intentando cargar metas estatales desde:", metas_path, "\n")
  
  # Leer Excel sin encabezado (asumimos que la primera fila es datos)
  metas_raw <- read_excel(metas_path, sheet = 1, col_names = FALSE, skip = 0)
  
  # Verificar que tiene al menos 2 columnas
  if (ncol(metas_raw) < 2) {
    stop("❌ El archivo 'metas_estatales.xlsx' debe tener al menos 2 columnas: estado_nombre y meta_estatal")
  }
  
  # Renombrar columnas por posición
  names(metas_raw) <- c("estado_nombre", "meta_estatal")
  
  # Convertir meta_estatal a numérico (maneja NA automáticamente)
  metas_raw$meta_estatal <- as.numeric(metas_raw$meta_estatal)
  
  # Crear dataframe limpio con normalización
  metas_estatales <- metas_raw %>%
    select(estado_nombre, meta_estatal) %>%
    mutate(
      estado_nombre = case_when(
        estado_nombre == "Ciudad de México" ~ "CDMX",
        estado_nombre == "México" ~ "Mexico",
        estado_nombre == "Nuevo León" ~ "Nuevo_Leon",
        estado_nombre == "San Luis Potosí" ~ "San_Luis_Potosi",
        TRUE ~ normalizar_estado(estado_nombre)
      ),
      meta_estatal = ifelse(is.na(meta_estatal), NA_real_, meta_estatal)
    ) %>%
    filter(!is.na(estado_nombre)) %>%
    # ✅ FORZAR COHERENCIA ABSOLUTA CON expected_states
    mutate(
      estado_nombre = factor(estado_nombre, levels = expected_states, ordered = FALSE)
    ) %>%
    as.data.frame()
  
  # Validar que no hay filas vacías
  if (nrow(metas_estatales) == 0) {
    stop("❌ El archivo 'metas_estatales.xlsx' está vacío o no tiene datos válidos.")
  }
  
  # Verificar que todos los estados esperados están presentes
  loaded_states <- unique(metas_estatales$estado_nombre)
  missing <- setdiff(expected_states, loaded_states)
  extra <- setdiff(loaded_states, expected_states)
  
  if (length(missing) > 0) {
    cat("🚨 ADVERTENCIA: Los siguientes estados esperados NO se cargaron:\n")
    print(missing)
  }
  if (length(extra) > 0) {
    cat("🚨 ADVERTENCIA: Se encontraron estados inesperados en metas_estatales.xlsx:\n")
    print(extra)
  }
  
  cat("✅ Metas estatales cargadas correctamente. Total de registros:", nrow(metas_estatales), "\n")
} else {
  metas_estatales <- data.frame(
    estado_nombre = expected_states,
    meta_estatal = 14000
  )
  cat("⚠️ Archivo de metas no encontrado. Usando meta homogénea de 14,000 para todos los estados.\n")
}

# Cargar distritos federales de los 32 estados (¡ADAPTADO A TU ESTRUCTURA!)
cargar_distritos_federales <- function() {
  estados_carpetas <- list.dirs(shape_path, full.names = FALSE, recursive = FALSE)
  
  distritos_list <- list()  # ← ¡LISTA PARA ACUMULAR TODOS LOS ESTADOS!
  
  for (estado in estados_carpetas) {
    # Buscar archivo DISTRITO_FEDERAL.shp en cada carpeta (insensible a mayúsculas)
    shp_files <- list.files(
      file.path(shape_path, estado),
      pattern = "^DISTRITO_FEDERAL\\.",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    # Filtrar solo archivos .shp (no carpetas)
    if (length(shp_files) > 0) {
      shp_files <- shp_files[file.exists(shp_files) & !file.info(shp_files)$isdir]
    }
    
    if (length(shp_files) == 0) {
      cat("⚠️  No se encontró ningún archivo DISTRITO_FEDERAL.shp en:", estado, "\n")
      next
    }
    
    ruta_shape <- shp_files[1]  # Usa el primero que encuentre
    cat("✅ Cargando:", estado, "... ")
    
    tryCatch({
      df <- st_read(ruta_shape, quiet = TRUE)
      
      # Verificar columnas reales
      cat("Columnas encontradas:", paste(names(df), collapse = ", "), "\n")
      
      # Detectar columna geométrica
      geom_col <- st_geometry(df)
      if (is.null(geom_col)) {
        cat("❌ No se encontró ninguna columna geométrica válida. Saltando.\n")
        return(NULL)
      }
      
      geom_col_name <- names(df)[sapply(df, function(x) inherits(x, "sfc"))][1]
      if (is.na(geom_col_name)) {
        cat("❌ No se pudo identificar el nombre de la columna geométrica. Saltando.\n")
        return(NULL)
      }
      
      # Verificar que tenga las columnas necesarias
      if (!all(c("ENTIDAD", "DISTRITO") %in% names(df))) {
        cat("❌ Columnas faltantes (ENTIDAD, DISTRITO). Saltando.\n")
        return(NULL)
      }
      
      # Extraer nombre del estado del nombre de la carpeta (ej: "09_CDMX" → "CDMX")
      estado_nombre_raw <- estado %>% str_remove("^\\d+_")
      estado_nombre_raw <- ifelse(estado_nombre_raw == "CDMX", "CDMX", str_to_title(estado_nombre_raw))
      
      # Normalizar nombre del estado
      estado_nombre_clean <- ifelse(
        estado_nombre_raw == "Ciudad de México",
        "CDMX",
        normalizar_estado(estado_nombre_raw)
      )
      
      # Validar que sea un estado esperado
      if (!(estado_nombre_clean %in% expected_states)) {
        cat("⚠️ Estado inesperado:", estado_nombre_raw, ". Saltando.\n")
        return(NULL)
      }
      
      # Transformar CRS si es necesario
      if (!st_is_longlat(df)) {
        df <- st_transform(df, CRS_OBJ)
        cat("(transformado a WGS84) ")
      }
      
      # Crear columna de nombre de estado
      df_processed <- df %>%
        select(ENTIDAD, DISTRITO, !!sym(geom_col_name)) %>%
        mutate(
          cve_estado = as.character(ENTIDAD),
          distrito_num = as.character(DISTRITO),
          estado_nombre = estado_nombre_clean,
          meta_estatal = metas_estatales$meta_estatal[match(estado_nombre_clean, metas_estatales$estado_nombre)],
          monitoreado = !is.na(meta_estatal)
        ) %>%
        select(cve_estado, distrito_num, estado_nombre, monitoreado, meta_estatal, !!sym(geom_col_name)) %>%
        rename(geometry = !!sym(geom_col_name)) %>%
        st_make_valid() %>%
        filter(!is.na(st_is_valid(.)))
      
      cat("✔️ OK\n")
      
      # ✅ ¡CLAVE: ACUMULAR EN LA LISTA, NO DEVOLVER!
      distritos_list[[length(distritos_list) + 1]] <- df_processed
      
    }, error = function(e) {
      cat("❌ Error al leer:", e$message, "\n")
      return(NULL)
    })
  }
  
  # 👇 ¡UNIR TODOS LOS ESTADOS AL FINAL!
  distritos_validos <- bind_rows(distritos_list)
  
  if (nrow(distritos_validos) == 0) {
    stop("🛑 FATAL: No se cargó ningún distrito federal. Verifica que cada estado tenga DISTRITO_FEDERAL.shp + sus archivos auxiliares (.shx, .dbf, .prj)")
  }
  
  # Validar que todos los estados esperados están presentes
  loaded_states <- unique(distritos_validos$estado_nombre)
  missing <- setdiff(expected_states, loaded_states)
  extra <- setdiff(loaded_states, expected_states)
  
  if (length(missing) > 0) {
    cat("🚨 ADVERTENCIA: Los siguientes estados esperados NO se cargaron:\n")
    print(missing)
  }
  if (length(extra) > 0) {
    cat("🚨 ADVERTENCIA: Se encontraron estados inesperados:\n")
    print(extra)
  }
  
  cat("🎉 Cargados", nrow(distritos_validos), "distritos federales de", length(unique(distritos_validos$estado_nombre)), "estados.\n")
  return(distritos_validos)  # ← ¡DEVUELVE TODO JUNTO!
}

# 👇 CARGAR LOS DATOS ESPACIALES (SOLO UNA VEZ)
distritos_federales <- cargar_distritos_federales()

# 📌 LISTA DE ESTADOS PARA EL FILTRO — SIEMPRE LOS 32 ESPERADOS
estados_lista <- sort(expected_states)