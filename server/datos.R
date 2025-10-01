# server/datos.R
library(here)
library(data.table)

cargar_datos <- function(anio, cargo, tipo_eleccion, estado, cabecera, municipio, seccion) {
  
  message("🔧 Cargando datos: anio=", anio, ", cargo=", cargo,
          ", tipo_eleccion=", tipo_eleccion %||% "AMBAS",
          ", estado=", estado %||% "Nacional",
          ", cabecera=", cabecera %||% "Todos",
          ", municipio=", municipio %||% "Todos",
          ", seccion=", if (length(seccion) > 0) paste(seccion, collapse = ", ") else "Todas")
  
  # Normalizar entradas
  anio <- as.character(anio)
  tipo_eleccion <- tipo_eleccion %||% "AMBAS"
  estado <- toupper(trimws(estado %||% "Nacional"))
  cabecera <- cabecera %||% "Todos"
  municipio <- municipio %||% "Todos"
  seccion <- if (is.null(seccion) || length(seccion) == 0 || "Todas" %in% seccion) "Todas" else seccion
  
  # Validar combinación año-cargo
  valid_combinations <- list(
    "2024" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
    "2023" = c("SENADURIA"),
    "2021" = c("DIPUTACION FEDERAL", "SENADURIA"),
    "2018" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
    "2015" = c("DIPUTACION FEDERAL"),
    "2012" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
    "2009" = c("DIPUTACION FEDERAL"),
    "2006" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA")
  )
  
  if (!anio %in% names(valid_combinations) || !cargo %in% valid_combinations[[anio]]) {
    message("❌ Combinación no válida: anio=", anio, ", cargo=", cargo)
    return(list(
      datos = data.frame(),
      columnas = NULL,
      tiene_ordinaria = FALSE,
      tiene_extraordinaria = FALSE,
      tiene_mayoria_relativa = FALSE,
      tiene_representacion_proporcional = FALSE,
      todos_estados = character(0),
      todos_cabeceras = character(0),
      todos_municipios = character(0),
      todas_secciones = character(0)
    ))
  }
  
  # Mapear cargo a nombre de archivo
  cargo_map <- c("DIPUTACION FEDERAL" = "dip", "SENADURIA" = "sen", "PRESIDENCIA" = "pdte")
  cargo_name <- cargo_map[cargo]
  
  if (is.na(cargo_name)) {
    message("⚠️ Cargo no mapeado: ", cargo)
    return(list(
      datos = data.frame(),
      columnas = NULL,
      tiene_ordinaria = FALSE,
      tiene_extraordinaria = FALSE,
      tiene_mayoria_relativa = FALSE,
      tiene_representacion_proporcional = FALSE,
      todos_estados = character(0),
      todos_cabeceras = character(0),
      todos_municipios = character(0),
      todas_secciones = character(0)
    ))
  }
  
  # Construir nombre del archivo
  archivo <- paste0("pef_", cargo_name, "_", anio, ".csv")
  ruta_archivo <- here::here("data/results/federals", archivo)
  
  message("📁 Ruta del archivo: ", ruta_archivo)
  
  # Verificar si existe el archivo
  if (!file.exists(ruta_archivo)) {
    message("❌ Archivo no encontrado: ", ruta_archivo)
    return(list(
      datos = data.frame(),
      columnas = NULL,
      tiene_ordinaria = FALSE,
      tiene_extraordinaria = FALSE,
      tiene_mayoria_relativa = FALSE,
      tiene_representacion_proporcional = FALSE,
      todos_estados = character(0),
      todos_cabeceras = character(0),
      todos_municipios = character(0),
      todas_secciones = character(0)
    ))
  }
  
  # Leer el archivo CSV con data.table
  datos <- tryCatch({
    dt <- fread(ruta_archivo, showProgress = FALSE)
    message("📊 Filas totales en CSV antes de filtrar: ", nrow(dt))
    
    # # Convertir columnas clave a numéricas, incluso si vienen como texto
    if ("part_ciud" %in% colnames(dt)) {
      dt[, part_ciud := as.numeric(as.character(part_ciud))]
    }
    if ("lne" %in% colnames(dt)) {
      dt[, lne := as.numeric(as.character(lne))]
    }
    if ("total_votos" %in% colnames(dt)) {
      dt[, total_votos := as.numeric(as.character(total_votos))]
    }
    
    # Eliminar filas con part_ciud NA si es necesario
    dt <- dt[!is.na(part_ciud)]
    
    # Verificar y normalizar columna 'estado'
    if ("estado" %in% colnames(dt)) {
      dt[, estado := toupper(trimws(estado))]
      message("🔍 Valores únicos en 'estado': ", paste(sort(unique(dt$estado)), collapse = ", "))
    } else {
      message("❌ Columna 'estado' no encontrada en el CSV")
      return(data.frame())
    }
    
    # Verificar y normalizar columna 'tipo'
    if ("tipo" %in% colnames(dt)) {
      dt[, tipo := toupper(trimws(tipo))]
      tipos_unicos <- unique(dt$tipo[!is.na(dt$tipo)])
      message("🔍 Valores únicos en 'tipo': ", paste(tipos_unicos, collapse = ", "))
    } else {
      message("⚠️ Columna 'tipo' no encontrada. Se asignará 'ORDINARIA' por defecto.")
      dt[, tipo := "ORDINARIA"]
    }
    
    # Combinar filtros
    filtros <- TRUE
    if (estado != "NACIONAL") {
      filtros <- filtros & dt$estado == estado
    }
    if (cabecera != "Todos" && "cabecera" %in% colnames(dt)) {
      filtros <- filtros & dt$cabecera == cabecera
    }
    if (municipio != "Todos" && "municipio" %in% colnames(dt)) {
      filtros <- filtros & dt$municipio == municipio
    }
    if (!identical(seccion, "Todas") && "seccion" %in% colnames(dt)) {
      filtros <- filtros & dt$seccion %in% seccion
    }
    if (!is.null(tipo_eleccion) && tipo_eleccion != "AMBAS" && "tipo" %in% colnames(dt)) {
      filtros <- filtros & dt$tipo == tipo_eleccion
    }
    
    dt <- dt[filtros]
    message("📊 Filas tras aplicar filtros: ", nrow(dt))
    
    # Normalizar columna principio
    if ("principio" %in% colnames(dt)) {
      dt[, principio := toupper(trimws(principio))]
      dt[, principio := fifelse(principio == "MAYORIA RELATIVA", "MAYORÍA RELATIVA", 
                                fifelse(principio == "REPRESENTACION PROPORCIONAL", 
                                        "REPRESENTACIÓN PROPORCIONAL", principio))]
      message("🔍 Principios únicos: ", paste(unique(dt$principio[!is.na(dt$principio)]), collapse = ", "))
    } else {
      message("⚠️ Columna 'principio' no encontrada en el CSV")
    }
    
    as.data.frame(dt)
  }, error = function(e) {
    message("💥 Error al leer archivo: ", e$message)
    return(NULL)
  })
  
  if (is.null(datos) || nrow(datos) == 0) {
    message("⚠️ Datos es NULL o vacíos tras intentar leer el archivo")
    return(list(
      datos = data.frame(),
      columnas = NULL,
      tiene_ordinaria = FALSE,
      tiene_extraordinaria = FALSE,
      tiene_mayoria_relativa = FALSE,
      tiene_representacion_proporcional = FALSE,
      todos_estados = character(0),
      todos_cabeceras = character(0),
      todos_municipios = character(0),
      todas_secciones = character(0)
    ))
  }
  
  # ✅ Asegurar que los nombres de distrito estén completos y correctos
  if ("cabecera" %in% colnames(datos)) {
    # Normalizar formato: asegurar que tenga el nombre completo
    datos$cabecera <- gsub("^0101.*", "0101 JESUS MARIA", datos$cabecera)
    datos$cabecera <- gsub("^0102.*", "0102 AGUASCALIENTES", datos$cabecera)
    datos$cabecera <- gsub("^0103.*", "0103 SAN FRANCISCO DE LOS ROMO", datos$cabecera)
  }
  
  # Obtener listas únicas
  todos_estados <- sort(unique(datos$estado[!is.na(datos$estado)])) %||% character(0)
  todos_cabeceras <- sort(unique(datos$cabecera[!is.na(datos$cabecera)])) %||% character(0)
  todos_municipios <- sort(unique(datos$municipio[!is.na(datos$municipio)])) %||% character(0)
  todas_secciones <- sort(unique(datos$seccion[!is.na(datos$seccion)])) %||% character(0)
  
  # Evaluar tipos de elección
  tiene_ordinaria <- FALSE
  tiene_extraordinaria <- FALSE
  
  if ("tipo" %in% colnames(datos)) {
    tiene_ordinaria <- any(datos$tipo == "ORDINARIA", na.rm = TRUE)
    tiene_extraordinaria <- any(datos$tipo == "EXTRAORDINARIA", na.rm = TRUE)
    message("🔍 tiene_ordinaria: ", tiene_ordinaria, ", tiene_extraordinaria: ", tiene_extraordinaria)
  } else {
    message("⚠️ Columna 'tipo' no disponible. Asumiendo solo elección ordinaria.")
    tiene_ordinaria <- TRUE
  }
  
  tiene_mayoria_relativa <- any(datos$principio == "MAYORÍA RELATIVA", na.rm = TRUE)
  tiene_representacion_proporcional <- any(datos$principio == "REPRESENTACIÓN PROPORCIONAL", na.rm = TRUE)
  message("🔍 tiene_mayoria_relativa: ", tiene_mayoria_relativa, 
          ", tiene_representacion_proporcional: ", tiene_representacion_proporcional)
  
  # Retorno
  list(
    datos = datos,
    columnas = colnames(datos) %||% character(0),
    tiene_ordinaria = tiene_ordinaria,
    tiene_extraordinaria = tiene_extraordinaria,
    tiene_mayoria_relativa = tiene_mayoria_relativa,
    tiene_representacion_proporcional = tiene_representacion_proporcional,
    todos_estados = todos_estados,
    todos_cabeceras = todos_cabeceras,
    todos_municipios = todos_municipios,
    todas_secciones = todas_secciones
  )
}