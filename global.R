# global.R

library(sf)
library(dplyr)
library(purrr)

# Ruta base de los shapefiles
shape_path <- "afiliacion_shp"

# Cargar todos los distritos federales de todos los estados
cargar_distritos_federales <- function() {
  estados <- list.dirs(shape_path, full.names = FALSE, recursive = FALSE)
  
  distritos_list <- map(estados, ~ {
    ruta <- file.path(shape_path, .x, "DISTRITO_FEDERAL.shp")
    if (file.exists(ruta)) {
      sf::st_read(ruta, quiet = TRUE) %>%
        mutate(
          cve_estado = as.character(ENTIDAD),
          distrito_num = as.numeric(DISTRITO),
          estado_nombre = .x %>% str_remove("^\\d+_") %>% str_to_title()
        ) %>%
        select(cve_estado, distrito_num, estado_nombre, geometry)
    }
  }) %>%
    compact() %>%
    bind_rows()
  
  return(distritos_list)
}

# Cargar una sola vez al inicio
distritos_federales <- cargar_distritos_federales()

# Lista de estados para filtros
estados_lista <- sort(unique(distritos_federales$estado_nombre))