#' @title Encontrar directorios de datos de la Lista Nominal Electoral
#' @description Busca las rutas de los directorios de datos históricos y semanales de la LNE.
#' @return Lista con rutas a los directorios 'historico' y 'semanal'
find_lne_data_dirs <- function() {
  base_path <- here::here("data", "pdln")
  
  rutas <- list(
    historico = file.path(base_path, "historico"),
    semanal = file.path(base_path, "semanal")
  )
  
  for (nombre in names(rutas)) {
    if (!dir.exists(rutas[[nombre]])) {
      warning(paste("⚠️ Directorio no encontrado:", rutas[[nombre]]))
    } else {
      message("✅ Directorio LNE encontrado:", nombre, "→", rutas[[nombre]])
    }
  }
  
  return(rutas)
}

#' @title Extraer fecha de nombre de archivo LNE
#' @param archivo Nombre del archivo (ej. derfe_pdln_20241031_base.csv)
#' @return Fecha en formato Date o NULL si no coincide
extract_fecha_lne <- function(archivo) {
  # Patrón: derfe_pdln_YYYYMMDD_...
  m <- regexec("derfe_pdln_(\\d{8})_", archivo)
  match <- regmatches(archivo, m)[[1]]
  
  if (length(match) < 2) return(NULL)
  
  fecha_str <- match[2]
  tryCatch({
    as.Date(fecha_str, format = "%Y%m%d")
  }, error = function(e) {
    message("⚠️ Fecha inválida en archivo: ", archivo)
    return(NULL)
  })
}

#' @title Generar catálogo de fechas disponibles
#' @return Lista con fechas históricas y semanales
build_lne_catalog <- function() {
  dirs <- find_lne_data_dirs()
  
  catalog <- list(
    historico = character(0),
    semanal = list(edad = character(0), origen = character(0), sexo = character(0))
  )
  
  # --- Histórico ---
  if (dir.exists(dirs$historico)) {
    files_hist <- list.files(dirs$historico, pattern = "^derfe_pdln_\\d{8}_base\\.csv$", full.names = FALSE)
    fechas_hist <- sapply(files_hist, extract_fecha_lne, USE.NAMES = FALSE)
    fechas_hist <- fechas_hist[!sapply(fechas_hist, is.null)]
    catalog$historico <- sort(unique(fechas_hist), decreasing = TRUE)
  }
  
  # --- Semanal ---
  if (dir.exists(dirs$semanal)) {
    for (tipo in c("edad", "origen", "sexo")) {
      pattern <- paste0("^derfe_pdln_\\d{8}_", tipo, "\\.csv$")
      files_sem <- list.files(dirs$semanal, pattern = pattern, full.names = FALSE)
      fechas_sem <- sapply(files_sem, extract_fecha_lne, USE.NAMES = FALSE)
      fechas_sem <- fechas_sem[!sapply(fechas_sem, is.null)]
      catalog$semanal[[tipo]] <- sort(unique(fechas_sem), decreasing = TRUE)
    }
  }
  
  # Intersección de fechas semanales (solo fechas con los 3 archivos)
  fechas_comunes <- Reduce(intersect, catalog$semanal)
  catalog$semanal_comun <- sort(fechas_comunes, decreasing = TRUE)
  
  return(catalog)
}