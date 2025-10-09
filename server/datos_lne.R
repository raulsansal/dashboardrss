# server/datos_lne.R
# Función principal para cargar datos de Lista Nominal Electoral

source("utils_lne.R")
library(data.table)

# Cargar catálogo global una vez al iniciar
LNE_CATALOG <- build_lne_catalog()
LNE_DIRS <- find_lne_data_dirs()

#' @title Cargar datos de Lista Nominal Electoral
#' @description Función unificada para cargar datos históricos o semanales con filtros geográficos
#' @param tipo_corte "historico" o "semanal"
#' @param fecha Fecha de corte (Date object)
#' @param dimension Solo para semanal: "edad", "origen", "sexo" o "completo"
#' @param estado Nombre de estado o "Nacional" (default)
#' @param distrito Código de distrito o "Todos" (default)
#' @param municipio Nombre de municipio o "Todos" (default)
#' @param seccion Vector de códigos de sección o "Todas" (default)
#' @param incluir_extranjero Lógico, incluir residentes en el extranjero (default: TRUE)
#' @return Lista con: datos (data.frame), metadatos, listas únicas de valores geográficos
cargar_lne <- function(tipo_corte, fecha, dimension = "completo",
                       estado = "Nacional", distrito = "Todos", 
                       municipio = "Todos", seccion = "Todas",
                       incluir_extranjero = TRUE) {
  
  # Validar fecha
  if (is.null(fecha) || is.na(fecha)) {
    message("❌ Fecha inválida en cargar_lne()")
    return(list(
      datos = data.frame(),
      metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
      todos_estados = character(0),
      todos_distritos = character(0),
      todos_municipios = character(0),
      todas_secciones = character(0)
    ))
  }
  
  fecha_str <- format(fecha, "%Y%m%d")
  message("📂 Cargando datos LNE: tipo=", tipo_corte, ", fecha=", fecha_str, ", dimension=", dimension)
  
  # ========== CARGA DE DATOS HISTÓRICOS ==========
  if (tipo_corte == "historico") {
    archivo <- file.path(LNE_DIRS$historico, paste0("derfe_pdln_", fecha_str, "_base.csv"))
    
    if (!file.exists(archivo)) {
      message("❌ Archivo histórico no encontrado: ", archivo)
      return(list(
        datos = data.frame(),
        metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
        todos_estados = character(0),
        todos_distritos = character(0),
        todos_municipios = character(0),
        todas_secciones = character(0)
      ))
    }
    
    # Leer archivo con data.table
    dt <- fread(archivo, sep = "\t", header = TRUE, encoding = "UTF-8", showProgress = FALSE)
    message("📊 Filas cargadas (histórico): ", nrow(dt))
    
    # Normalizar nombres de columnas
    dt <- normalizar_columnas_historico(dt)
    
    # Asegurar que claves sean character
    if ("clave_entidad" %in% colnames(dt)) dt[, clave_entidad := as.character(clave_entidad)]
    if ("clave_distrito" %in% colnames(dt)) dt[, clave_distrito := as.character(clave_distrito)]
    if ("clave_municipio" %in% colnames(dt)) dt[, clave_municipio := as.character(clave_municipio)]
    if ("seccion" %in% colnames(dt)) dt[, seccion := as.character(seccion)]
    
    # Agregar nombres de entidades usando catálogo
    entidades <- catalogo_entidades()
    if ("clave_entidad" %in% colnames(dt)) {
      dt[, nombre_entidad := entidades[as.character(clave_entidad)]]
      dt[is.na(nombre_entidad), nombre_entidad := "DESCONOCIDO"]
    }
    
    # Limpiar filas especiales (TOTALES y opcionalmente RESIDENTES EXTRANJERO)
    dt <- limpiar_filas_especiales(dt, incluir_extranjero = incluir_extranjero, incluir_totales = FALSE)
    
    # CRÍTICO: Detectar estructura de columnas del archivo histórico
    # Según la muestra, las columnas vienen agrupadas:
    # 1. Padrón Nacional: HOMBRES, MUJERES, NO BINARIO, TOTAL (índices 5-8)
    # 2. Padrón Extranjero: HOMBRES, MUJERES, TOTAL (índices 9-11)
    # 3. Lista Nominal Nacional: HOMBRES, MUJERES, NO BINARIO, TOTAL (índices 12-15)
    # 4. Lista Nominal Extranjero: HOMBRES, MUJERES, TOTAL (índices 16-18)
    
    # Renombrar columnas por índice (más robusto que por nombre con espacios)
    col_names <- colnames(dt)
    if (length(col_names) >= 15) {
      # Suponiendo estructura estándar
      setnames(dt, old = col_names[5:8], 
               new = c("padron_nac_hombres", "padron_nac_mujeres", "padron_nac_nobinario", "padron_nac_total"),
               skip_absent = TRUE)
      
      if (length(col_names) >= 11) {
        setnames(dt, old = col_names[9:11], 
                 new = c("padron_ext_hombres", "padron_ext_mujeres", "padron_ext_total"),
                 skip_absent = TRUE)
      }
      
      setnames(dt, old = col_names[12:15], 
               new = c("lista_nac_hombres", "lista_nac_mujeres", "lista_nac_nobinario", "lista_nac_total"),
               skip_absent = TRUE)
      
      if (length(col_names) >= 18) {
        setnames(dt, old = col_names[16:18], 
                 new = c("lista_ext_hombres", "lista_ext_mujeres", "lista_ext_total"),
                 skip_absent = TRUE)
      }
      
      # Crear columnas agregadas (nacional + extranjero)
      dt[, padron_electoral := padron_nac_total + ifelse(is.na(padron_ext_total), 0, padron_ext_total)]
      dt[, lista_nominal := lista_nac_total + ifelse(is.na(lista_ext_total), 0, lista_ext_total)]
      dt[, padron_hombres := padron_nac_hombres + ifelse(is.na(padron_ext_hombres), 0, padron_ext_hombres)]
      dt[, padron_mujeres := padron_nac_mujeres + ifelse(is.na(padron_ext_mujeres), 0, padron_ext_mujeres)]
      dt[, lista_hombres := lista_nac_hombres + ifelse(is.na(lista_ext_hombres), 0, lista_ext_hombres)]
      dt[, lista_mujeres := lista_nac_mujeres + ifelse(is.na(lista_ext_mujeres), 0, lista_ext_mujeres)]
      
      # Calcular tasa de inclusión
      dt[, tasa_inclusion := round((lista_nominal / padron_electoral) * 100, 2)]
    } else {
      message("⚠️ Estructura de columnas histórico no coincide con esperado")
    }
    
    # Aplicar filtros geográficos
    dt_filtrado <- dt
    
    if (estado != "Nacional") {
      dt_filtrado <- dt_filtrado[nombre_entidad == estado]
      message("🔍 Filtro estado: ", estado, " → ", nrow(dt_filtrado), " filas")
    }
    
    if (distrito != "Todos" && "clave_distrito" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[clave_distrito == distrito]
      message("🔍 Filtro distrito: ", distrito, " → ", nrow(dt_filtrado), " filas")
    }
    
    if (municipio != "Todos" && "clave_municipio" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[clave_municipio == municipio]
      message("🔍 Filtro municipio: ", municipio, " → ", nrow(dt_filtrado), " filas")
    }
    
    if (!identical(seccion, "Todas") && !"Todas" %in% seccion && "seccion" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[seccion %in% seccion]
      message("🔍 Filtro sección: ", length(seccion), " secciones → ", nrow(dt_filtrado), " filas")
    }
    
    # Generar listas únicas para filtros dinámicos
    todos_estados <- sort(unique(dt[nombre_entidad != "RESIDENTES EXTRANJERO" & !is.na(nombre_entidad), nombre_entidad]))
    todos_distritos <- sort(unique(dt_filtrado[!is.na(clave_distrito), clave_distrito]))
    todos_municipios <- sort(unique(dt_filtrado[!is.na(clave_municipio), clave_municipio]))
    todas_secciones <- sort(unique(dt_filtrado[!is.na(seccion), seccion]))
    
    # Validar datos cargados
    validacion <- validar_datos_lne(dt_filtrado, tipo = "historico")
    if (!validacion$valido) {
      message("⚠️ Advertencia en validación: ", paste(validacion$mensajes, collapse = "; "))
    }
    
    return(list(
      datos = as.data.frame(dt_filtrado),
      metadatos = list(
        tipo_corte = tipo_corte,
        fecha = fecha,
        dimension = dimension,
        archivo = basename(archivo),
        filas_originales = nrow(dt),
        filas_filtradas = nrow(dt_filtrado)
      ),
      todos_estados = todos_estados,
      todos_distritos = todos_distritos,
      todos_municipios = todos_municipios,
      todas_secciones = todas_secciones
    ))
  }
  
  # ========== CARGA DE DATOS SEMANALES ==========
  if (tipo_corte == "semanal") {
    # Verificar que la fecha tenga los 3 archivos (edad, origen, sexo)
    if (!fecha %in% LNE_CATALOG$semanal_comun) {
      message("❌ Fecha semanal incompleta (faltan archivos): ", fecha_str)
      return(list(
        datos = data.frame(),
        metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
        todos_estados = character(0),
        todos_distritos = character(0),
        todos_municipios = character(0),
        todas_secciones = character(0)
      ))
    }
    
    # Cargar archivos según dimensión solicitada
    archivos <- list()
    
    if (dimension %in% c("edad", "completo")) {
      archivos$edad <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_edad.csv"))
    }
    
    if (dimension %in% c("origen", "completo")) {
      archivos$origen <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_origen.csv"))
    }
    
    if (dimension %in% c("sexo", "completo")) {
      archivos$sexo <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_sexo.csv"))
    }
    
    # Verificar que todos los archivos existan
    archivos_faltantes <- sapply(archivos, function(a) !file.exists(a))
    if (any(archivos_faltantes)) {
      message("❌ Faltan archivos semanales: ", paste(names(archivos)[archivos_faltantes], collapse = ", "))
      return(list(
        datos = data.frame(),
        metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
        todos_estados = character(0),
        todos_distritos = character(0),
        todos_municipios = character(0),
        todas_secciones = character(0)
      ))
    }
    
    # Cargar archivos
    dts <- list()
    for (tipo in names(archivos)) {
      dts[[tipo]] <- fread(archivos[[tipo]], sep = "\t", header = TRUE, encoding = "UTF-8", showProgress = FALSE)
      dts[[tipo]] <- normalizar_columnas_semanal(dts[[tipo]])
      message("📊 Filas cargadas (", tipo, "): ", nrow(dts[[tipo]]))
    }
    
    # Unir datasets por claves geográficas
    # Usar el archivo base (edad si está disponible, sino sexo)
    if ("edad" %in% names(dts)) {
      dt_base <- dts$edad
    } else if ("sexo" %in% names(dts)) {
      dt_base <- dts$sexo
    } else {
      dt_base <- dts[[1]]
    }
    
    # Identificar columnas geográficas para el merge
    cols_geo <- c("clave_entidad", "nombre_entidad", "clave_distrito", "cabecera_distrital", 
                  "clave_municipio", "nombre_municipio", "seccion")
    
    # Si hay más de un dataset, hacer merge
    if (length(dts) > 1) {
      dt_completo <- dt_base
      
      for (tipo in setdiff(names(dts), names(dts)[1])) {
        # Identificar columnas a traer (no geográficas ni duplicadas)
        cols_existentes <- intersect(colnames(dt_completo), colnames(dts[[tipo]]))
        cols_nuevas <- setdiff(colnames(dts[[tipo]]), cols_existentes)
        cols_merge <- c(cols_geo[cols_geo %in% colnames(dts[[tipo]])], cols_nuevas)
        
        dt_completo <- merge(dt_completo, dts[[tipo]][, ..cols_merge], 
                             by = cols_geo[cols_geo %in% colnames(dts[[tipo]])],
                             all.x = TRUE, all.y = FALSE)
        message("🔗 Merge con ", tipo, " → ", nrow(dt_completo), " filas")
      }
    } else {
      dt_completo <- dt_base
    }
    
    # Limpiar filas especiales
    dt_completo <- limpiar_filas_especiales(dt_completo, incluir_extranjero = incluir_extranjero, incluir_totales = FALSE)
    
    # Calcular tasa de inclusión si no existe
    if ("padron_electoral" %in% colnames(dt_completo) && "lista_nominal" %in% colnames(dt_completo)) {
      if (!"tasa_inclusion" %in% colnames(dt_completo)) {
        dt_completo[, tasa_inclusion := round((lista_nominal / padron_electoral) * 100, 2)]
      }
    }
    
    # Aplicar filtros geográficos
    dt_filtrado <- dt_completo
    
    if (estado != "Nacional" && "nombre_entidad" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[nombre_entidad == estado]
      message("🔍 Filtro estado: ", estado, " → ", nrow(dt_filtrado), " filas")
    }
    
    if (distrito != "Todos" && "cabecera_distrital" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[cabecera_distrital == distrito]
      message("🔍 Filtro distrito: ", distrito, " → ", nrow(dt_filtrado), " filas")
    }
    
    if (municipio != "Todos" && "nombre_municipio" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[nombre_municipio == municipio]
      message("🔍 Filtro municipio: ", municipio, " → ", nrow(dt_filtrado), " filas")
    }
    
    if (!identical(seccion, "Todas") && !"Todas" %in% seccion && "seccion" %in% colnames(dt_filtrado)) {
      dt_filtrado <- dt_filtrado[seccion %in% seccion]
      message("🔍 Filtro sección: ", length(seccion), " secciones → ", nrow(dt_filtrado), " filas")
    }
    
    # Generar listas únicas para filtros dinámicos
    if ("nombre_entidad" %in% colnames(dt_completo)) {
      todos_estados <- sort(unique(dt_completo[nombre_entidad != "RESIDENTES EXTRANJERO" & !is.na(nombre_entidad), nombre_entidad]))
    } else {
      todos_estados <- character(0)
    }
    
    if ("cabecera_distrital" %in% colnames(dt_filtrado)) {
      todos_distritos <- sort(unique(dt_filtrado[!is.na(cabecera_distrital), cabecera_distrital]))
    } else {
      todos_distritos <- character(0)
    }
    
    if ("nombre_municipio" %in% colnames(dt_filtrado)) {
      todos_municipios <- sort(unique(dt_filtrado[!is.na(nombre_municipio), nombre_municipio]))
    } else {
      todos_municipios <- character(0)
    }
    
    if ("seccion" %in% colnames(dt_filtrado)) {
      todas_secciones <- sort(unique(dt_filtrado[!is.na(seccion), seccion]))
    } else {
      todas_secciones <- character(0)
    }
    
    # Validar datos cargados
    validacion <- validar_datos_lne(dt_filtrado, tipo = "semanal")
    if (!validacion$valido) {
      message("⚠️ Advertencia en validación: ", paste(validacion$mensajes, collapse = "; "))
    }
    
    return(list(
      datos = as.data.frame(dt_filtrado),
      metadatos = list(
        tipo_corte = tipo_corte,
        fecha = fecha,
        dimension = dimension,
        archivos = basename(unlist(archivos)),
        filas_originales = nrow(dt_completo),
        filas_filtradas = nrow(dt_filtrado)
      ),
      todos_estados = todos_estados,
      todos_distritos = todos_distritos,
      todos_municipios = todos_municipios,
      todas_secciones = todas_secciones
    ))
  }
  
  # Si llegamos aquí, tipo_corte no es válido
  message("❌ Tipo de corte no válido: ", tipo_corte)
  return(list(
    datos = data.frame(),
    metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
    todos_estados = character(0),
    todos_distritos = character(0),
    todos_municipios = character(0),
    todas_secciones = character(0)
  ))
}

#' @title Obtener resumen agregado de datos LNE
#' @description Calcula totales y promedios para la selección actual
#' @param datos_lne Output de cargar_lne()
#' @return Lista con indicadores clave
obtener_resumen_lne <- function(datos_lne) {
  
  if (is.null(datos_lne) || nrow(datos_lne$datos) == 0) {
    return(list(
      total_padron = 0,
      total_lista = 0,
      tasa_inclusion_promedio = 0,
      padron_hombres = 0,
      padron_mujeres = 0,
      lista_hombres = 0,
      lista_mujeres = 0,
      num_secciones = 0
    ))
  }
  
  datos <- datos_lne$datos
  
  resumen <- list(
    total_padron = sum(datos$padron_electoral, na.rm = TRUE),
    total_lista = sum(datos$lista_nominal, na.rm = TRUE),
    tasa_inclusion_promedio = round(mean(datos$tasa_inclusion, na.rm = TRUE), 2),
    num_secciones = nrow(datos)
  )
  
  # Agregar desgloses por sexo si están disponibles
  if ("padron_hombres" %in% colnames(datos)) {
    resumen$padron_hombres <- sum(datos$padron_hombres, na.rm = TRUE)
  }
  
  if ("padron_mujeres" %in% colnames(datos)) {
    resumen$padron_mujeres <- sum(datos$padron_mujeres, na.rm = TRUE)
  }
  
  if ("lista_hombres" %in% colnames(datos)) {
    resumen$lista_hombres <- sum(datos$lista_hombres, na.rm = TRUE)
  }
  
  if ("lista_mujeres" %in% colnames(datos)) {
    resumen$lista_mujeres <- sum(datos$lista_mujeres, na.rm = TRUE)
  }
  
  return(resumen)
}

#' @title Comparar dos periodos de LNE
#' @description Calcula diferencias absolutas y relativas entre dos fechas
#' @param datos_fecha1 Output de cargar_lne() para fecha 1
#' @param datos_fecha2 Output de cargar_lne() para fecha 2
#' @return Lista con diferencias calculadas
comparar_periodos_lne <- function(datos_fecha1, datos_fecha2) {
  
  resumen1 <- obtener_resumen_lne(datos_fecha1)
  resumen2 <- obtener_resumen_lne(datos_fecha2)
  
  comparacion <- list(
    diferencia_padron = resumen2$total_padron - resumen1$total_padron,
    diferencia_lista = resumen2$total_lista - resumen1$total_lista,
    crecimiento_padron_pct = round(((resumen2$total_padron - resumen1$total_padron) / resumen1$total_padron) * 100, 2),
    crecimiento_lista_pct = round(((resumen2$total_lista - resumen1$total_lista) / resumen1$total_lista) * 100, 2),
    cambio_tasa_inclusion = resumen2$tasa_inclusion_promedio - resumen1$tasa_inclusion_promedio
  )
  
  return(comparacion)
}

message("✅ Módulo datos_lne.R cargado correctamente")
message("📅 Fechas históricas disponibles: ", length(LNE_CATALOG$historico))
message("📅 Fechas semanales completas: ", length(LNE_CATALOG$semanal_comun))