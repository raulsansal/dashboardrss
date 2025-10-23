# server/datos_lne.R
# Sistema de carga de datos de Lista Nominal Electoral (LNE)

library(data.table)
library(dplyr)

# ========== FUNCIÓN: ESCANEAR ARCHIVOS DISPONIBLES ==========

escanear_archivos_lne <- function() {
  
  # Rutas de carpetas
  ruta_historico <- file.path("data", "pdln", "historico")
  ruta_semanal <- file.path("data", "pdln", "semanal")
  
  # Inicializar catálogo
  catalogo <- list(
    historico = character(0),
    historico_fechas = as.Date(character(0)),
    semanal_comun = character(0),
    semanal_comun_fechas = as.Date(character(0)),
    semanal_sexo = character(0),
    semanal_edad = character(0),
    semanal_origen = character(0)
  )
  
  # ========== ESCANEAR ARCHIVOS HISTÓRICOS ==========
  if (dir.exists(ruta_historico)) {
    archivos_historico <- list.files(ruta_historico, pattern = "\\.csv$", full.names = FALSE)
    
    if (length(archivos_historico) > 0) {
      # Extraer fechas de nombres como: derfe_pdln_20250731_base.csv
      fechas_extraidas <- gsub(".*_(\\d{8})_.*\\.csv", "\\1", archivos_historico)
      
      # Convertir a formato Date
      fechas_date <- as.Date(fechas_extraidas, format = "%Y%m%d")
      
      # Filtrar fechas válidas
      fechas_validas <- !is.na(fechas_date)
      
      catalogo$historico <- archivos_historico[fechas_validas]
      catalogo$historico_fechas <- fechas_date[fechas_validas]
      
      message("📂 Archivos históricos encontrados: ", length(catalogo$historico))
      if (length(catalogo$historico) > 0) {
        message("   Última fecha: ", max(catalogo$historico_fechas))
      }
    } else {
      message("⚠️ No se encontraron archivos en ", ruta_historico)
    }
  } else {
    warning("❌ Carpeta no encontrada: ", ruta_historico)
  }
  
  # ========== ESCANEAR ARCHIVOS SEMANALES ==========
  if (dir.exists(ruta_semanal)) {
    archivos_semanal <- list.files(ruta_semanal, pattern = "\\.csv$", full.names = FALSE)
    
    if (length(archivos_semanal) > 0) {
      # Separar por tipo
      catalogo$semanal_comun <- grep("comun|completo", archivos_semanal, value = TRUE, ignore.case = TRUE)
      catalogo$semanal_sexo <- grep("sexo", archivos_semanal, value = TRUE, ignore.case = TRUE)
      catalogo$semanal_edad <- grep("edad", archivos_semanal, value = TRUE, ignore.case = TRUE)
      catalogo$semanal_origen <- grep("origen", archivos_semanal, value = TRUE, ignore.case = TRUE)
      
      # Extraer fechas de archivos comunes
      if (length(catalogo$semanal_comun) > 0) {
        fechas_semanal <- gsub(".*_(\\d{8})_.*\\.csv", "\\1", catalogo$semanal_comun)
        fechas_semanal_date <- as.Date(fechas_semanal, format = "%Y%m%d")
        catalogo$semanal_comun_fechas <- fechas_semanal_date[!is.na(fechas_semanal_date)]
      }
      
      message("📂 Archivos semanales encontrados: ", 
              length(catalogo$semanal_comun), " comunes, ",
              length(catalogo$semanal_sexo), " sexo, ",
              length(catalogo$semanal_edad), " edad, ",
              length(catalogo$semanal_origen), " origen")
    }
  } else {
    message("⚠️ Carpeta no encontrada: ", ruta_semanal)
  }
  
  return(catalogo)
}

# ========== CATÁLOGO GLOBAL DE ARCHIVOS DISPONIBLES ==========

if (!exists("LNE_CATALOG")) {
  message("🔍 Escaneando archivos LNE disponibles...")
  LNE_CATALOG <- escanear_archivos_lne()
  
  # Crear versión simplificada para compatibilidad
  LNE_CATALOG$historico <- LNE_CATALOG$historico_fechas
  LNE_CATALOG$semanal_comun <- LNE_CATALOG$semanal_comun_fechas
  
  message("✅ Catálogo LNE inicializado: ", 
          length(LNE_CATALOG$historico), " históricos, ",
          length(LNE_CATALOG$semanal_comun), " semanales")
}

# ========== MAPEO DE ENTIDADES ==========

entidades <- c(
  "01" = "AGUASCALIENTES", "02" = "BAJA CALIFORNIA", "03" = "BAJA CALIFORNIA SUR",
  "04" = "CAMPECHE", "05" = "COAHUILA", "06" = "COLIMA", "07" = "CHIAPAS",
  "08" = "CHIHUAHUA", "09" = "CIUDAD DE MEXICO", "10" = "DURANGO",
  "11" = "GUANAJUATO", "12" = "GUERRERO", "13" = "HIDALGO", "14" = "JALISCO",
  "15" = "MEXICO", "16" = "MICHOACAN", "17" = "MORELOS", "18" = "NAYARIT",
  "19" = "NUEVO LEON", "20" = "OAXACA", "21" = "PUEBLA", "22" = "QUERETARO",
  "23" = "QUINTANA ROO", "24" = "SAN LUIS POTOSI", "25" = "SINALOA",
  "26" = "SONORA", "27" = "TABASCO", "28" = "TAMAULIPAS", "29" = "TLAXCALA",
  "30" = "VERACRUZ", "31" = "YUCATAN", "32" = "ZACATECAS", "33" = "EXTRANJERO"
)

# ========== FUNCIÓN AUXILIAR: ENCONTRAR ARCHIVO POR FECHA ==========

encontrar_archivo_lne <- function(tipo_corte, fecha, dimension = "completo") {
  
  fecha <- as.Date(fecha)
  fecha_str <- format(fecha, "%Y%m%d")
  
  if (tipo_corte == "historico") {
    ruta_carpeta <- file.path("data", "pdln", "historico")
    
    # Buscar archivo que contenga la fecha
    archivos <- list.files(ruta_carpeta, pattern = paste0(".*", fecha_str, ".*\\.csv$"), full.names = TRUE)
    
    if (length(archivos) > 0) {
      message("✅ [encontrar_archivo_lne] Archivo encontrado: ", basename(archivos[1]))
      return(archivos[1])
    } else {
      message("❌ [encontrar_archivo_lne] No se encontró archivo para fecha: ", fecha)
      return(NULL)
    }
    
  } else if (tipo_corte == "semanal") {
    ruta_carpeta <- file.path("data", "pdln", "semanal")
    
    # Determinar patrón según dimensión
    patron_dimension <- switch(dimension,
                               "completo" = "(comun|completo)",
                               "sexo" = "sexo",
                               "edad" = "edad",
                               "origen" = "origen",
                               "comun")
    
    # Buscar archivo que contenga la fecha y dimensión
    archivos <- list.files(ruta_carpeta, 
                           pattern = paste0(".*", patron_dimension, ".*", fecha_str, ".*\\.csv$"), 
                           full.names = TRUE, ignore.case = TRUE)
    
    if (length(archivos) > 0) {
      message("✅ [encontrar_archivo_lne] Archivo encontrado: ", basename(archivos[1]))
      return(archivos[1])
    } else {
      message("❌ [encontrar_archivo_lne] No se encontró archivo para fecha: ", fecha, ", dimensión: ", dimension)
      return(NULL)
    }
    
  } else {
    stop("Tipo de corte no válido: ", tipo_corte)
  }
}

# ========== FUNCIÓN PRINCIPAL: CARGAR LNE ==========

cargar_lne <- function(tipo_corte, fecha, dimension = "completo", 
                       estado = "Nacional", distrito = "Todos", 
                       municipio = "Todos", seccion = "Todas",
                       incluir_extranjero = TRUE) {
  
  inicio_total <- Sys.time()
  
  # Validar fecha
  fecha <- as.Date(fecha)
  if (is.na(fecha)) {
    stop("Fecha inválida: ", fecha)
  }
  
  # Encontrar archivo por fecha
  ruta_archivo <- encontrar_archivo_lne(tipo_corte, fecha, dimension)
  
  if (is.null(ruta_archivo)) {
    message("❌ [cargar_lne] No se pudo encontrar archivo para fecha: ", fecha)
    return(NULL)
  }
  
  message("📂 [cargar_lne] Cargando: ", ruta_archivo)
  
  # ========== LEER ARCHIVO CSV (OPTIMIZADO CON fread) ==========
  inicio_lectura <- Sys.time()
  
  dt <- tryCatch({
    fread(ruta_archivo, encoding = "UTF-8", stringsAsFactors = FALSE)
  }, error = function(e) {
    message("❌ [cargar_lne] Error leyendo CSV: ", e$message)
    return(NULL)
  })
  
  if (is.null(dt)) {
    message("❌ [cargar_lne] fread() retornó NULL")
    return(NULL)
  }
  
  tiempo_lectura <- round(difftime(Sys.time(), inicio_lectura, units = "secs"), 2)
  message("⏱️ Lectura CSV: ", tiempo_lectura, " seg (", nrow(dt), " filas)")
  
  if (nrow(dt) == 0) {
    message("⚠️ [cargar_lne] CSV vacío")
    return(NULL)
  }
  
  # ========== NORMALIZAR COLUMNAS ==========
  inicio_proceso <- Sys.time()
  
  # Normalizar nombres de columnas
  colnames(dt) <- tolower(colnames(dt))
  colnames(dt) <- gsub("\\s+", "_", colnames(dt))
  colnames(dt) <- gsub("[áàäâ]", "a", colnames(dt))
  colnames(dt) <- gsub("[éèëê]", "e", colnames(dt))
  colnames(dt) <- gsub("[íìïî]", "i", colnames(dt))
  colnames(dt) <- gsub("[óòöô]", "o", colnames(dt))
  colnames(dt) <- gsub("[úùüû]", "u", colnames(dt))
  colnames(dt) <- gsub("ñ", "n", colnames(dt))
  
  message("📋 [cargar_lne] Columnas originales: ", paste(head(colnames(dt), 10), collapse = ", "))
  
  # Renombrar columnas clave
  col_map <- c(
    "clave_entidad" = "clave_entidad",
    "entidad" = "clave_entidad",
    "cve_entidad" = "clave_entidad",
    "id_entidad" = "clave_entidad",
    "clave_distrito" = "clave_distrito",
    "distrito" = "clave_distrito",
    "cve_distrito" = "clave_distrito",
    "id_distrito" = "clave_distrito",
    "clave_municipio" = "clave_municipio",
    "municipio" = "clave_municipio",
    "cve_municipio" = "clave_municipio",
    "id_municipio" = "clave_municipio",
    "seccion" = "seccion",
    "cve_seccion" = "seccion",
    "id_seccion" = "seccion"
  )
  
  for (nuevo_nombre in names(col_map)) {
    patron <- col_map[nuevo_nombre]
    cols_match <- grep(paste0("^", patron, "$"), colnames(dt), value = TRUE)
    if (length(cols_match) > 0) {
      setnames(dt, cols_match[1], nuevo_nombre, skip_absent = TRUE)
    }
  }
  
  # ========== AGREGAR MAPEOS ==========
  
  # Nombre de entidad
  if ("clave_entidad" %in% colnames(dt)) {
    dt[, nombre_entidad := entidades[sprintf("%02d", as.integer(clave_entidad))]]
    message("✅ [cargar_lne] nombre_entidad agregado")
  }
  
  # Nombre de distrito (cabecera distrital)
  if (all(c("clave_entidad", "clave_distrito") %in% colnames(dt))) {
    dt[, cabecera_distrital := sprintf("%02d", as.integer(clave_distrito))]
    message("✅ [cargar_lne] cabecera_distrital agregado")
  }
  
  # Nombre de municipio
  if (all(c("clave_entidad", "clave_municipio") %in% colnames(dt))) {
    dt[, nombre_municipio := paste0(
      sprintf("%02d", as.integer(clave_entidad)), "-",
      sprintf("%03d", as.integer(clave_municipio))
    )]
    message("✅ [cargar_lne] nombre_municipio agregado")
  }
  
  # ========== ELIMINAR FILA DE TOTALES ==========
  if ("seccion" %in% colnames(dt)) {
    filas_antes <- nrow(dt)
    dt <- dt[!grepl("^TOTAL", toupper(as.character(seccion)), ignore.case = TRUE)]
    filas_despues <- nrow(dt)
    if (filas_antes != filas_despues) {
      message("🗑️ [cargar_lne] Eliminadas ", filas_antes - filas_despues, " filas de TOTALES")
    }
  }
  
  # ========== PROCESAR COLUMNAS NUMÉRICAS ==========
  
  # Identificar columnas numéricas (excluyendo claves)
  cols_numericas <- setdiff(
    colnames(dt),
    c("clave_entidad", "clave_distrito", "clave_municipio", "seccion",
      "nombre_entidad", "cabecera_distrital", "nombre_municipio")
  )
  
  # Convertir a numéricas
  for (col in cols_numericas) {
    if (col %in% colnames(dt)) {
      dt[[col]] <- suppressWarnings(as.numeric(dt[[col]]))
    }
  }
  
  # ========== DETECTAR Y RENOMBRAR COLUMNAS PRINCIPALES ==========
  
  # Padrón Electoral
  candidatos_padron <- grep("padron.*electoral|padron_electoral", 
                            colnames(dt), value = TRUE, ignore.case = TRUE)
  if (length(candidatos_padron) > 0) {
    col_padron <- candidatos_padron[1]
    setnames(dt, col_padron, "padron_electoral", skip_absent = TRUE)
    message("✅ [cargar_lne] padron_electoral: ", col_padron, " → padron_electoral")
  }
  
  # Lista Nominal
  candidatos_lista <- grep("lista.*nominal|lista_nominal", 
                           colnames(dt), value = TRUE, ignore.case = TRUE)
  if (length(candidatos_lista) > 0) {
    col_lista <- candidatos_lista[1]
    setnames(dt, col_lista, "lista_nominal", skip_absent = TRUE)
    message("✅ [cargar_lne] lista_nominal: ", col_lista, " → lista_nominal")
  }
  
  # Calcular tasa de inclusión
  if (all(c("padron_electoral", "lista_nominal") %in% colnames(dt))) {
    dt[, tasa_inclusion := round((lista_nominal / padron_electoral) * 100, 2)]
    dt[is.nan(tasa_inclusion) | is.infinite(tasa_inclusion), tasa_inclusion := NA]
    message("✅ [cargar_lne] tasa_inclusion calculada")
  }
  
  # ========== DETECTAR COLUMNAS DE SEXO ==========
  if (dimension == "completo" || dimension == "sexo") {
    # Padrón Hombres
    cols_padron_h <- grep("padron.*hombres|padron_hombres", colnames(dt), 
                          value = TRUE, ignore.case = TRUE)
    if (length(cols_padron_h) > 0) {
      setnames(dt, cols_padron_h[1], "padron_hombres", skip_absent = TRUE)
      message("✅ [cargar_lne] padron_hombres detectado")
    }
    
    # Padrón Mujeres
    cols_padron_m <- grep("padron.*mujeres|padron_mujeres", colnames(dt), 
                          value = TRUE, ignore.case = TRUE)
    if (length(cols_padron_m) > 0) {
      setnames(dt, cols_padron_m[1], "padron_mujeres", skip_absent = TRUE)
      message("✅ [cargar_lne] padron_mujeres detectado")
    }
    
    # Lista Hombres
    cols_lista_h <- grep("lista.*hombres|lista_hombres", colnames(dt), 
                         value = TRUE, ignore.case = TRUE)
    if (length(cols_lista_h) > 0) {
      setnames(dt, cols_lista_h[1], "lista_hombres", skip_absent = TRUE)
      message("✅ [cargar_lne] lista_hombres detectado")
    }
    
    # Lista Mujeres
    cols_lista_m <- grep("lista.*mujeres|lista_mujeres", colnames(dt), 
                         value = TRUE, ignore.case = TRUE)
    if (length(cols_lista_m) > 0) {
      setnames(dt, cols_lista_m[1], "lista_mujeres", skip_absent = TRUE)
      message("✅ [cargar_lne] lista_mujeres detectado")
    }
  }
  
  tiempo_proceso <- round(difftime(Sys.time(), inicio_proceso, units = "secs"), 2)
  message("⏱️ Procesamiento: ", tiempo_proceso, " seg")
  
  # ========== APLICAR FILTROS ==========
  inicio_filtros <- Sys.time()
  
  # Filtro de estado
  if (estado != "Nacional") {
    if ("nombre_entidad" %in% colnames(dt)) {
      dt <- dt[toupper(nombre_entidad) == toupper(estado)]
      message("🔍 Filtro estado: ", estado, " → ", nrow(dt), " filas")
    }
  }
  
  # Filtro de distrito
  if (distrito != "Todos" && "cabecera_distrital" %in% colnames(dt)) {
    dt <- dt[cabecera_distrital == distrito]
    message("🔍 Filtro distrito: ", distrito, " → ", nrow(dt), " filas")
  }
  
  # Filtro de municipio
  if (municipio != "Todos" && "nombre_municipio" %in% colnames(dt)) {
    dt <- dt[nombre_municipio == municipio]
    message("🔍 Filtro municipio: ", municipio, " → ", nrow(dt), " filas")
  }
  
  # Filtro de sección
  if (!is.null(seccion) && length(seccion) > 0 && !("Todas" %in% seccion)) {
    if ("seccion" %in% colnames(dt)) {
      dt <- dt[seccion %in% seccion]
      message("🔍 Filtro secciones: ", length(seccion), " seleccionadas → ", nrow(dt), " filas")
    }
  }
  
  # Filtro de extranjero
  if (!incluir_extranjero && "nombre_entidad" %in% colnames(dt)) {
    dt <- dt[nombre_entidad != "EXTRANJERO"]
    message("🔍 Filtro: Excluir extranjero → ", nrow(dt), " filas")
  }
  
  tiempo_filtros <- round(difftime(Sys.time(), inicio_filtros, units = "secs"), 2)
  message("⏱️ Filtros: ", tiempo_filtros, " seg")
  
  if (nrow(dt) == 0) {
    message("⚠️ [cargar_lne] Sin datos tras aplicar filtros")
    return(NULL)
  }
  
  # ========== PREPARAR RESULTADO ==========
  
  # Convertir a data.frame
  df <- as.data.frame(dt)
  
  # Obtener listas únicas para filtros
  todos_estados <- if ("nombre_entidad" %in% colnames(df)) {
    sort(unique(df$nombre_entidad[df$nombre_entidad != "EXTRANJERO"]))
  } else character(0)
  
  todos_distritos <- if ("cabecera_distrital" %in% colnames(df)) {
    sort(unique(df$cabecera_distrital))
  } else character(0)
  
  todos_municipios <- if ("nombre_municipio" %in% colnames(df)) {
    sort(unique(df$nombre_municipio))
  } else character(0)
  
  todas_secciones <- if ("seccion" %in% colnames(df)) {
    sort(unique(df$seccion))
  } else character(0)
  
  tiempo_total <- round(difftime(Sys.time(), inicio_total, units = "secs"), 2)
  message("✅ [cargar_lne] Datos cargados: ", nrow(df), " filas, ", ncol(df), " columnas (Tiempo total: ", tiempo_total, " seg)")
  
  # Retornar estructura con datos y metadatos
  resultado <- list(
    datos = df,
    todos_estados = todos_estados,
    todos_distritos = todos_distritos,
    todos_municipios = todos_municipios,
    todas_secciones = todas_secciones
  )
  
  return(resultado)
}

message("✅ datos_lne.R cargado")
