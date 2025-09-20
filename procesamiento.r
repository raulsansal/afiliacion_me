# ==================================================================
# 📌 SCRIPT: Eliminar duplicados en Excel → 1 fila por ID_ENTIDAD + ID_DISTRITO_FEDERAL
# ==================================================================

library(readxl)
library(writexl)
library(dplyr)

# =====================
# 1. CARGAR EL ARCHIVO EXCEL
# =====================

archivo_entrada <- "data/fuente_distritos.xlsx"  # ← ¡CAMBIA ESTA RUTA!
hoja <- "Hoja1"  # ← Cambia esto si tu hoja tiene otro nombre

cat("📥 Cargando archivo:", archivo_entrada, "\n")
datos_brutos <- read_excel(archivo_entrada, sheet = hoja)

# Verificar columnas esperadas
expected_cols <- c("ID_ENTIDAD", "ESTADO", "ID_DISTRITO_FEDERAL")
if (!all(expected_cols %in% names(datos_brutos))) {
  stop("❌ El archivo debe contener las columnas: ", paste(expected_cols, collapse = ", "))
}

cat("✅ Columnas cargadas correctamente.\n")

# =====================
# 2. LIMPIAR NOMBRES DE COLUMNAS (eliminar espacios, mayúsculas, etc.)
# =====================
datos_brutos <- datos_brutos %>%
  rename_with(~ tolower(.x)) %>%                    # Convertir a minúsculas
  rename(
    cve_estado = id_entidad,
    estado_nombre = estado,
    distrito_num = id_distrito_federal
  ) %>%
  mutate(
    across(c(cve_estado, distrito_num), ~ as.numeric(.)),  # Asegurar numérico
    estado_nombre = trimws(as.character(estado_nombre))     # Limpiar espacios
  )

cat("✅ Columnas normalizadas: cve_estado, estado_nombre, distrito_num\n")

# =====================
# 3. ELIMINAR DUPLICADOS: MANTENER UNA FILA POR COMBINACIÓN ÚNICA
# =====================
# Usamos distinct() de dplyr → mantiene solo la primera ocurrencia de cada grupo único
datos_limpios <- datos_brutos %>%
  distinct(cve_estado, distrito_num, .keep_all = TRUE) %>%
  select(cve_estado, estado_nombre, distrito_num) %>%
  arrange(cve_estado, distrito_num)  # Ordenar para legibilidad

cat("✅ Duplicados eliminados. Total de filas únicas:", nrow(datos_limpios), "\n")

# =====================
# 4. GUARDAR EL ARCHIVO LIMPIO EN EXCEL
# =====================
archivo_salida <- "data/distritos_limpio.xlsx"

cat("💾 Guardando archivo limpio en:", archivo_salida, "\n")
write_xlsx(datos_limpios, archivo_salida)

# =====================
# 5. VERIFICACIÓN FINAL (OPCIONAL)
# =====================
print(head(datos_limpios))
cat("\n📊 Resumen de estados únicos:\n")
print(table(datos_limpios$cve_estado))

cat("\n🎉 ¡Limpieza completada exitosamente!\n")