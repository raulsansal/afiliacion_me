# global.R
# 📦 CARGAR PAQUETES NECESARIOS (¡EXPLÍCITAMENTE!)
library(sf)
library(dplyr)
library(purrr)
library(stringr)
library(readr)
library(foreign)

# Configuración
shape_path <- "afiliacion_shp"
CRS_OBJ <- st_crs("EPSG:4326")

# Ruta del archivo de metas estatales
metas_path <- "data/metas_estatales.csv"

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
  
  lines <- readLines(metas_path, warn = FALSE)
  cat("🔍 Primeras 3 líneas del archivo:\n")
  cat(lines[1:3], sep = "\n")
  
  metas_estatales <- read_csv(
    metas_path,
    col_names = c("estado_nombre", "meta_estatal"),
    col_types = cols(
      estado_nombre = col_character(),
      meta_estatal = col_double()
    ),
    locale = locale(encoding = "UTF-8"),
    skip_empty_rows = TRUE,
    quote = ""
  ) %>%
    select(estado_nombre, meta_estatal) %>%
    mutate(
      estado_nombre = case_when(
        estado_nombre == "Ciudad de México" ~ "CDMX",
        TRUE ~ normalizar_estado(estado_nombre)
      ),
      meta_estatal = ifelse(is.na(meta_estatal), NA_real_, meta_estatal)
    )
  
  cat("✅ Metas estatales cargadas correctamente.\n")
} else {
  metas_estatales <- data.frame(
    estado_nombre = expected_states,
    meta_estatal = 14000
  )
  cat("⚠️ Archivo de metas no encontrado. Usando meta homogénea de 14,000 para todos los estados.\n")
}

# Cargar distritos federales de los 32 estados
cargar_distritos_federales <- function() {
  estados_carpetas <- list.dirs(shape_path, full.names = FALSE, recursive = FALSE)
  
  # ✅ CREAR LA LISTA DENTRO DE LA FUNCIÓN
  distritos_list <- list()
  
  for (estado in estados_carpetas) {
    # 🔥 BUSCAR ARCHIVO DISTRITO_FEDERAL.SH(P) INSENSIBLE A MAYÚSCULAS/MINÚSCULAS
    shp_files <- list.files(
      file.path(shape_path, estado),
      pattern = "^DISTRITO_FEDERAL\\.",
      full.names = TRUE,
      ignore.case = TRUE
    )
    
    # Filtrar solo archivos (no carpetas) — compatible con R < 4.1
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
      
      # ✅ DETECTAR COLUMNAS GEOMÉTRICAS — COMPATIBLE CON TODAS LAS VERSIONES DE sf
      geom_col <- st_geometry(df)
      if (is.null(geom_col)) {
        cat("❌ No se encontró ninguna columna geométrica válida. Saltando.\n")
        return(NULL)
      }
      
      # Obtener el nombre de la columna geométrica (compatible con cualquier sf)
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
      
      # Extraer nombre del estado del nombre de la carpeta
      estado_nombre_raw <- estado %>% str_remove("^\\d+_")
      estado_nombre_raw <- ifelse(estado_nombre_raw == "CDMX", "CDMX", str_to_title(estado_nombre_raw))
      
      # Normalizar nombre del estado, pero con excepción para CDMX
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
      
      # ⚠️ TRANSFORMACIÓN REAL DE CRS — ¡CORREGIDA!
      original_crs <- st_crs(df)
      if (is.na(original_crs$epsg) || is.na(original_crs$proj4string)) {
        cat("⚠️ CRS no reconocido. Asumiendo UTM Zona 14N (EPSG:32614)...\n")
        df <- st_set_crs(df, 32614)  # UTM Zona 14N (común en INE)
      }
      
      if (!st_is_longlat(df)) {
        df <- st_transform(df, CRS_OBJ)
        cat("(transformado a WGS84) ")
      } else {
        cat("(ya está en WGS84) ")
      }
      
      # ✅ ¡CLAVE: SELECCIONAR PRIMERO LAS COLUMNAS BASE, LUEGO MUTATE!
      df_processed <- df %>%
        select(ENTIDAD, DISTRITO, !!sym(geom_col_name)) %>%  # ← ¡USAMOS LA COLUMNA QUE SEA!
        mutate(
          cve_estado = as.character(ENTIDAD),
          distrito_num = as.numeric(DISTRITO),  # ← ¡CAMBIO CLAVE: A NÚMERO!
          estado_nombre = estado_nombre_clean,
          meta_estatal = metas_estatales$meta_estatal[match(estado_nombre_clean, metas_estatales$estado_nombre)],
          monitoreado = !is.na(meta_estatal)
        ) %>%
        select(cve_estado, distrito_num, estado_nombre, monitoreado, meta_estatal, !!sym(geom_col_name)) %>%
        rename(geometry = !!sym(geom_col_name))  # ← La renombramos como "geometry" para consistencia
      
      # ✅ VALIDACIÓN GEOMÉTRICA
      df_processed <- df_processed %>%
        st_make_valid() %>%
        filter(!is.na(st_is_valid(.)))
      
      # ✅ ¡ELIMINAMOS EL RECORTADO! Dejamos todas las geometrías completas
      # mexico_bbox <- st_bbox(c(xmin = -118.5, xmax = -86.5, ymin = 14.5, ymax = 32.7))
      # df_processed <- df_processed %>% st_crop(mexico_bbox)
      
      cat("✔️ OK\n")
      
      # ✅ ¡ACUMULAR EN LA LISTA LOCAL!
      distritos_list[[length(distritos_list) + 1]] <- df_processed
      
      # ✅ ¡DEVOLVER NULL SOLO SI HAY ERROR — PERO SI TODO VA BIEN, NO DEVUELVA NADA!
      # La función sigue ejecutándose — no se necesita return aquí.
      
    }, error = function(e) {
      cat("❌ Error al leer:", e$message, "\n")
      return(NULL)
    })
  }
  
  # 👇 ¡UNIR TODOS LOS ESTADOS EN UN SOLO SF OBJECT!
  distritos_validos <- bind_rows(distritos_list)
  
  if (nrow(distritos_validos) == 0) {
    stop("🛑 FATAL: No se cargó ningún distrito federal. Verifica que todos los estados tengan DISTRITO_FEDERAL.shp + sus archivos auxiliares (.shx, .dbf, .prj)")
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
  
  # ✅ ¡ESTA ES LA LÍNEA CLAVE: DEVUELVE EL OBJETO ACUMULADO!
  return(distritos_validos)
}

# 👇 CARGAR LOS DATOS ESPACIALES (SOLO UNA VEZ)
distritos_federales <- cargar_distritos_federales()

# 📌 LISTA DE ESTADOS PARA EL FILTRO — SIEMPRE LOS 32 ESPERADOS
estados_lista <- sort(expected_states)