# Cargar distritos federales de los 32 estados
cargar_distritos_federales <- function() {
  # ✅ RUTA ÚNICA AL ARCHIVO COMPLETO DE DISTRICTOS FEDERALES
  ruta_shape <- file.path("afiliacion_shp", "DISTRITO_FEDERAL.shp")
  
  if (!file.exists(ruta_shape)) {
    stop("🛑 FATAL: No se encontró el archivo 'afiliacion_shp/DISTRITO_FEDERAL.shp'.\n",
         "Descarga el shapefile oficial de distritos federales de INE 2024 y colócalo aquí.")
  }
  
  cat("✅ Cargando distritos federales de México desde:", ruta_shape, "\n")
  
  df <- st_read(ruta_shape, quiet = TRUE)
  
  # Verificar columnas necesarias
  if (!all(c("ENTIDAD", "DISTRITO") %in% names(df))) {
    stop("❌ El shapefile no tiene las columnas necesarias: ENTIDAD, DISTRITO")
  }
  
  cat("Columnas encontradas:", paste(names(df), collapse = ", "), "\n")
  
  # Normalizar CRS
  if (!st_is_longlat(df)) {
    df <- st_transform(df, CRS_OBJ)
    cat("(transformado a WGS84) ")
  }
  
  # Crear columna de nombre de estado
  df <- df %>%
    mutate(
      cve_estado = as.character(ENTIDAD),
      distrito_num = as.character(DISTRITO),  # ✅ FORZADO A CHARACTER
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
      ),
      meta_estatal = metas_estatales$meta_estatal[match(estado_nombre, metas_estatales$estado_nombre)],
      monitoreado = !is.na(meta_estatal)
    ) %>%
    select(cve_estado, distrito_num, estado_nombre, monitoreado, meta_estatal, geometry) %>%
    st_make_valid() %>%
    filter(!is.na(st_is_valid(.)))
  
  cat("🎉 Cargados", nrow(df), "distritos federales de", length(unique(df$estado_nombre)), "estados.\n")
  return(df)
}