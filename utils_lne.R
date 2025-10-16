# utils_lne.R
# Funciones utilitarias para manejo de datos de Lista Nominal Electoral (LNE)

library(data.table)
library(here)

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
#' @description Escanea directorios y genera listas de fechas disponibles por tipo
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
    catalog$historico <- sort(unique(as.Date(unlist(fechas_hist), origin = "1970-01-01")), decreasing = TRUE)
    message("📅 Fechas históricas encontradas: ", length(catalog$historico))
  }
  
  # --- Semanal ---
  if (dir.exists(dirs$semanal)) {
    for (tipo in c("edad", "origen", "sexo")) {
      pattern <- paste0("^derfe_pdln_\\d{8}_", tipo, "\\.csv$")
      files_sem <- list.files(dirs$semanal, pattern = pattern, full.names = FALSE)
      fechas_sem <- sapply(files_sem, extract_fecha_lne, USE.NAMES = FALSE)
      fechas_sem <- fechas_sem[!sapply(fechas_sem, is.null)]
      catalog$semanal[[tipo]] <- sort(unique(as.Date(unlist(fechas_sem), origin = "1970-01-01")), decreasing = TRUE)
    }
    message("📅 Fechas semanales (edad): ", length(catalog$semanal$edad))
    message("📅 Fechas semanales (origen): ", length(catalog$semanal$origen))
    message("📅 Fechas semanales (sexo): ", length(catalog$semanal$sexo))
  }
  
  # Intersección de fechas semanales (solo fechas con los 3 archivos)
  fechas_comunes <- Reduce(intersect, catalog$semanal)
  catalog$semanal_comun <- sort(fechas_comunes, decreasing = TRUE)
  message("📅 Fechas semanales completas (edad+origen+sexo): ", length(catalog$semanal_comun))
  
  return(catalog)
}

#' @title Normalizar nombres de columnas de archivo histórico
#' @param dt data.table con columnas originales
#' @return data.table con columnas normalizadas
normalizar_columnas_historico <- function(dt) {
  
  cols_originales <- colnames(dt)
  message("🔍 Columnas originales del CSV: ", paste(head(cols_originales, 10), collapse = " | "))
  
  tryCatch({
    # Limpiar nombres de columnas (quitar saltos de línea y espacios extra)
    cols_limpios <- gsub("\n", " ", cols_originales)
    cols_limpios <- gsub("\\s+", " ", cols_limpios)
    cols_limpios <- trimws(cols_limpios)
    setnames(dt, old = cols_originales, new = cols_limpios)
    
    # Renombrar claves geográficas
    if ("CLAVE ENTIDAD" %in% colnames(dt)) {
      setnames(dt, "CLAVE ENTIDAD", "clave_entidad", skip_absent = TRUE)
    }
    
    if ("CLAVE DISTRITO" %in% colnames(dt)) {
      setnames(dt, "CLAVE DISTRITO", "clave_distrito", skip_absent = TRUE)
    }
    
    if ("CLAVE MUNICIPIO" %in% colnames(dt)) {
      setnames(dt, "CLAVE MUNICIPIO", "clave_municipio", skip_absent = TRUE)
    }
    
    if ("SECCION" %in% colnames(dt)) {
      setnames(dt, "SECCION", "seccion", skip_absent = TRUE)
    }
    
    message("✅ Columnas geográficas normalizadas")
    
  }, error = function(e) {
    message("⚠️ Error al normalizar columnas históricas: ", e$message)
  })
  
  return(dt)
}

#' @title Normalizar nombres de columnas de archivo semanal
#' @param dt data.table con columnas originales
#' @return data.table con columnas normalizadas
normalizar_columnas_semanal <- function(dt) {
  # CRÍTICO: Si solo hay 1 columna, el archivo no se parseó correctamente
  if (ncol(dt) == 1) {
    message("⚠️ Solo 1 columna detectada, intentando re-parsear con separador de tabulador")
    
    # Obtener el texto crudo de la única columna
    primera_col <- names(dt)[1]
    
    # Si la primera columna contiene tabuladores, necesitamos re-parsear
    if (grepl("\t", primera_col)) {
      message("✅ Detectados tabuladores en el encabezado, re-parseando...")
      
      # Separar el encabezado por tabulador
      headers <- strsplit(primera_col, "\t")[[1]]
      headers <- trimws(headers)
      
      # Separar cada fila por tabulador
      filas_parseadas <- lapply(1:nrow(dt), function(i) {
        valores <- strsplit(as.character(dt[[primera_col]][i]), "\t")[[1]]
        valores <- trimws(valores)
        
        # Asegurarse de que tenga la misma longitud que los headers
        if (length(valores) < length(headers)) {
          valores <- c(valores, rep(NA, length(headers) - length(valores)))
        } else if (length(valores) > length(headers)) {
          valores <- valores[1:length(headers)]
        }
        
        return(valores)
      })
      
      # Crear nuevo data.table correctamente parseado
      dt_nuevo <- data.table::data.table(do.call(rbind, filas_parseadas))
      colnames(dt_nuevo) <- headers
      
      message("✅ Re-parseado exitoso: ", ncol(dt_nuevo), " columnas")
      dt <- dt_nuevo
    }
  }
  
  # Ahora normalizar nombres de columnas
  cols_originales <- colnames(dt)
  
  # Limpiar espacios y saltos de línea
  cols_nuevas <- gsub("\n", " ", cols_originales, fixed = TRUE)
  cols_nuevas <- gsub("\r", "", cols_nuevas, fixed = TRUE)
  cols_nuevas <- gsub("\\s+", " ", cols_nuevas)
  cols_nuevas <- trimws(cols_nuevas)
  
  setnames(dt, cols_originales, cols_nuevas, skip_absent = TRUE)
  
  # Renombrar columnas geográficas estándar
  mapeo_nombres <- c(
    "CLAVE ENTIDAD" = "clave_entidad",
    "NOMBRE ENTIDAD" = "nombre_entidad",
    "CLAVE DISTRITO" = "clave_distrito",
    "CABECERA DISTRITAL" = "cabecera_distrital",
    "CLAVE MUNICIPIO" = "clave_municipio",
    "NOMBRE MUNICIPIO" = "nombre_municipio",
    "SECCION" = "seccion",
    "PADRON HOMBRES" = "padron_hombres",
    "PADRON MUJERES" = "padron_mujeres",
    "PADRON NO BINARIO" = "padron_no_binario",
    "PADRON ELECTORAL" = "padron_electoral",
    "LISTA HOMBRES" = "lista_hombres",
    "LISTA MUJERES" = "lista_mujeres",
    "LISTA NO BINARIO" = "lista_no_binario",
    "LISTA NOMINAL" = "lista_nominal"
  )
  
  for (viejo in names(mapeo_nombres)) {
    if (viejo %in% colnames(dt)) {
      setnames(dt, viejo, mapeo_nombres[[viejo]], skip_absent = TRUE)
      message("✅ Renombrado: ", viejo, " → ", mapeo_nombres[[viejo]])
    }
  }
  
  message("✅ Normalización completada: ", ncol(dt), " columnas")
  message("📋 Columnas finales: ", paste(head(colnames(dt), 10), collapse = ", "))
  
  return(dt)
}

#' @title Catálogo de entidades federativas
#' @return Named vector con clave como names y nombre como values
catalogo_entidades <- function() {
  c(
    "1" = "AGUASCALIENTES",
    "2" = "BAJA CALIFORNIA",
    "3" = "BAJA CALIFORNIA SUR",
    "4" = "CAMPECHE",
    "5" = "COAHUILA",
    "6" = "COLIMA",
    "7" = "CHIAPAS",
    "8" = "CHIHUAHUA",
    "9" = "CIUDAD DE MEXICO",
    "10" = "DURANGO",
    "11" = "GUANAJUATO",
    "12" = "GUERRERO",
    "13" = "HIDALGO",
    "14" = "JALISCO",
    "15" = "MEXICO",
    "16" = "MICHOACAN",
    "17" = "MORELOS",
    "18" = "NAYARIT",
    "19" = "NUEVO LEON",
    "20" = "OAXACA",
    "21" = "PUEBLA",
    "22" = "QUERETARO",
    "23" = "QUINTANA ROO",
    "24" = "SAN LUIS POTOSI",
    "25" = "SINALOA",
    "26" = "SONORA",
    "27" = "TABASCO",
    "28" = "TAMAULIPAS",
    "29" = "TLAXCALA",
    "30" = "VERACRUZ",
    "31" = "YUCATAN",
    "32" = "ZACATECAS",
    "0" = "RESIDENTES EXTRANJERO"
  )
}

#' @title Limpiar datos especiales (TOTALES y RESIDENTES EXTRANJERO)
#' @param dt data.table con datos LNE
#' @param incluir_extranjero Lógico, si incluir residentes en el extranjero
#' @param incluir_totales Lógico, si incluir fila de totales
#' @return data.table limpio
limpiar_filas_especiales <- function(dt, incluir_extranjero = TRUE, incluir_totales = FALSE) {
  
  # Identificar fila de totales (última fila usualmente, o donde clave_entidad == "TOTALES")
  if (!incluir_totales) {
    # Detectar por contenido de texto "TOTALES" en cualquier columna de texto
    cols_texto <- sapply(dt, is.character)
    if (any(cols_texto)) {
      filas_totales <- apply(dt[, ..cols_texto], 1, function(row) any(grepl("TOTALES", row, ignore.case = TRUE)))
      if (any(filas_totales)) {
        dt <- dt[!filas_totales]
        message("🧹 Fila de TOTALES removida")
      }
    }
  }
  
  # Identificar residentes extranjero (clave_entidad == "1" o "0" y distrito == "0")
  if (!incluir_extranjero) {
    if ("clave_entidad" %in% colnames(dt) && "clave_distrito" %in% colnames(dt)) {
      dt <- dt[!(clave_entidad %in% c("0", "1") & clave_distrito == "0")]
      message("🧹 Residentes EXTRANJERO removidos")
    }
  }
  
  return(dt)
}

#' @title Validar integridad de datos cargados
#' @param dt data.table con datos LNE
#' @param tipo "historico" o "semanal"
#' @return Lista con resultado de validación (valido = TRUE/FALSE, mensajes = character vector)
validar_datos_lne <- function(dt, tipo = "historico") {
  resultado <- list(valido = TRUE, mensajes = character(0))
  
  # Verificar que no esté vacío
  if (nrow(dt) == 0) {
    resultado$valido <- FALSE
    resultado$mensajes <- c(resultado$mensajes, "❌ Data.table vacío")
    return(resultado)
  }
  
  # Verificar columnas geográficas
  cols_geograficas <- c("clave_entidad", "clave_distrito", "clave_municipio", "seccion")
  faltantes <- setdiff(cols_geograficas, colnames(dt))
  if (length(faltantes) > 0) {
    resultado$valido <- FALSE
    resultado$mensajes <- c(resultado$mensajes, paste("❌ Faltan columnas geográficas:", paste(faltantes, collapse = ", ")))
  }
  
  # Verificar columnas numéricas clave
  if (tipo == "historico") {
    cols_numericas <- c("padron_electoral", "lista_nominal")
  } else {
    cols_numericas <- c("padron_electoral", "lista_nominal")
  }
  
  for (col in cols_numericas) {
    if (col %in% colnames(dt)) {
      if (!is.numeric(dt[[col]])) {
        resultado$valido <- FALSE
        resultado$mensajes <- c(resultado$mensajes, paste("❌ Columna", col, "no es numérica"))
      }
    }
  }
  
  if (resultado$valido) {
    resultado$mensajes <- c(resultado$mensajes, "✅ Validación exitosa")
  }
  
  return(resultado)
}