# server/datos_lne.R
# Función principal para cargar datos de Lista Nominal Electoral

source("utils_lne.R")
library(data.table)

# Configurar locale en español para nombres de meses
Sys.setlocale("LC_TIME", "es_ES.UTF-8")

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
    
    # Leer archivo - usar read.csv porque el separador es COMA, no tabulador
    dt <- tryCatch({
      df <- read.csv(
        archivo,
        header = TRUE,
        stringsAsFactors = FALSE,
        fileEncoding = "UTF-8",
        check.names = FALSE,
        quote = "\"",
        na.strings = c("", "NA"),
        strip.white = TRUE
      )
      
      message("📊 Filas: ", nrow(df), " | Columnas: ", ncol(df))
      
      # Limpiar nombres de columnas (eliminar saltos de línea)
      nombres <- colnames(df)
      nombres <- gsub("\n", " ", nombres, fixed = TRUE)
      nombres <- gsub("\r", "", nombres, fixed = TRUE)
      nombres <- gsub("\\s+", " ", nombres)
      nombres <- trimws(nombres)
      
      # Eliminar columnas vacías (sin nombre)
      idx_validas <- nchar(nombres) > 0
      df <- df[, idx_validas, drop = FALSE]
      nombres <- nombres[idx_validas]
      
      colnames(df) <- nombres
      
      message("✅ Columnas válidas: ", ncol(df))
      message("📋 Primeras 5: ", paste(head(nombres, 5), collapse = " | "))
      
      as.data.table(df)
    }, error = function(e) {
      message("❌ Error al leer archivo histórico: ", e$message)
      return(NULL)
    })
    
    if (is.null(dt) || nrow(dt) == 0) {
      message("❌ No se pudieron cargar datos o archivo vacío")
      return(list(
        datos = data.frame(),
        metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
        todos_estados = character(0),
        todos_distritos = character(0),
        todos_municipios = character(0),
        todas_secciones = character(0)
      ))
    }
    
    message("📊 Filas cargadas (histórico): ", nrow(dt))
    
    # Normalizar nombres de columnas
    dt <- normalizar_columnas_historico(dt)
    message("📋 Después de normalizar (primeras 10): ", paste(head(colnames(dt), 10), collapse = ", "))
    
    # Convertir claves geográficas a character
    for (col in c("clave_entidad", "clave_distrito", "clave_municipio", "seccion")) {
      if (col %in% colnames(dt)) {
        dt[[col]] <- as.character(dt[[col]])
      }
    }
    
    # Agregar nombres de entidades usando catálogo
    entidades <- catalogo_entidades()
    if ("clave_entidad" %in% colnames(dt)) {
      dt[, nombre_entidad := entidades[as.character(clave_entidad)]]
      dt[is.na(nombre_entidad), nombre_entidad := "DESCONOCIDO"]
      message("✅ Entidades encontradas: ", length(unique(dt$nombre_entidad)))
    }
    
    # Limpiar filas especiales ANTES de procesar columnas numéricas
    dt <- limpiar_filas_especiales(dt, incluir_extranjero = incluir_extranjero, incluir_totales = FALSE)
    
    # Procesar columnas numéricas - eliminar comas y comillas
    cols_numericas <- grep("PADRON|PADRÓN|LISTA|HOMBRES|MUJERES|BINARIO|TOTAL|ELECTORAL|NOMINAL", 
                           colnames(dt), value = TRUE, ignore.case = TRUE)
    
    message("🔢 Procesando ", length(cols_numericas), " columnas numéricas")
    if (length(cols_numericas) > 0) {
      message("📋 Columnas numéricas (primeras 5): ", paste(head(cols_numericas, 5), collapse = ", "))
    }
    
    for (col in cols_numericas) {
      if (col %in% colnames(dt)) {
        dt[[col]] <- as.numeric(gsub('[,"]', '', as.character(dt[[col]])))
      }
    }
    
    # Identificar y estandarizar columnas clave
    col_names <- colnames(dt)
    
    # Buscar columnas PADRÓN/PADRON (con y sin acento)
    padron_total_idx <- grep("PADRON|PADRÓN", col_names, ignore.case = TRUE)
    padron_total_idx <- padron_total_idx[grep("TOTAL|ELECTORAL", col_names[padron_total_idx], ignore.case = TRUE)]
    
    # Excluir EXTRANJERO si existe
    if (length(padron_total_idx) > 1) {
      excluir <- grep("EXTRANJERO", col_names[padron_total_idx], ignore.case = TRUE)
      if (length(excluir) > 0) {
        padron_total_idx <- padron_total_idx[-excluir]
      }
    }
    
    lista_total_idx <- grep("LISTA", col_names, ignore.case = TRUE)
    lista_total_idx <- lista_total_idx[grep("TOTAL|NOMINAL", col_names[lista_total_idx], ignore.case = TRUE)]
    
    # Excluir EXTRANJERO
    if (length(lista_total_idx) > 1) {
      excluir <- grep("EXTRANJERO", col_names[lista_total_idx], ignore.case = TRUE)
      if (length(excluir) > 0) {
        lista_total_idx <- lista_total_idx[-excluir]
      }
    }
    
    message("🔍 Candidatos padrón: ", length(padron_total_idx))
    message("🔍 Candidatos lista: ", length(lista_total_idx))
    
    # Crear columnas estandarizadas
    if (length(padron_total_idx) > 0) {
      col_padron <- col_names[padron_total_idx[1]]
      message("✅ Columna padron_electoral: ", col_padron)
      dt[, padron_electoral := get(col_padron)]
      message("📊 Padrón total: ", format(sum(dt$padron_electoral, na.rm = TRUE), big.mark = ","))
    } else {
      message("⚠️ No se encontró columna de padrón electoral total")
      message("📋 Columnas disponibles: ", paste(head(col_names, 20), collapse = ", "))
    }
    
    if (length(lista_total_idx) > 0) {
      col_lista <- col_names[lista_total_idx[1]]
      message("✅ Columna lista_nominal: ", col_lista)
      dt[, lista_nominal := get(col_lista)]
      message("📊 Lista total: ", format(sum(dt$lista_nominal, na.rm = TRUE), big.mark = ","))
    } else {
      message("⚠️ No se encontró columna de lista nominal total")
    }
    
    # Buscar columnas de hombres y mujeres
    padron_h_idx <- grep("PADRON|PADRÓN", col_names, ignore.case = TRUE)
    padron_h_idx <- padron_h_idx[grep("HOMBRES", col_names[padron_h_idx], ignore.case = TRUE)]
    if (length(padron_h_idx) > 1) {
      excluir <- grep("EXTRANJERO", col_names[padron_h_idx], ignore.case = TRUE)
      if (length(excluir) > 0) padron_h_idx <- padron_h_idx[-excluir]
    }
    
    padron_m_idx <- grep("PADRON|PADRÓN", col_names, ignore.case = TRUE)
    padron_m_idx <- padron_m_idx[grep("MUJERES", col_names[padron_m_idx], ignore.case = TRUE)]
    if (length(padron_m_idx) > 1) {
      excluir <- grep("EXTRANJERO", col_names[padron_m_idx], ignore.case = TRUE)
      if (length(excluir) > 0) padron_m_idx <- padron_m_idx[-excluir]
    }
    
    lista_h_idx <- grep("LISTA", col_names, ignore.case = TRUE)
    lista_h_idx <- lista_h_idx[grep("HOMBRES", col_names[lista_h_idx], ignore.case = TRUE)]
    if (length(lista_h_idx) > 1) {
      excluir <- grep("EXTRANJERO", col_names[lista_h_idx], ignore.case = TRUE)
      if (length(excluir) > 0) lista_h_idx <- lista_h_idx[-excluir]
    }
    
    lista_m_idx <- grep("LISTA", col_names, ignore.case = TRUE)
    lista_m_idx <- lista_m_idx[grep("MUJERES", col_names[lista_m_idx], ignore.case = TRUE)]
    if (length(lista_m_idx) > 1) {
      excluir <- grep("EXTRANJERO", col_names[lista_m_idx], ignore.case = TRUE)
      if (length(excluir) > 0) lista_m_idx <- lista_m_idx[-excluir]
    }
    
    if (length(padron_h_idx) > 0) {
      dt[, padron_hombres := get(col_names[padron_h_idx[1]])]
      message("✅ Padrón hombres: ", format(sum(dt$padron_hombres, na.rm = TRUE), big.mark = ","))
    }
    if (length(padron_m_idx) > 0) {
      dt[, padron_mujeres := get(col_names[padron_m_idx[1]])]
      message("✅ Padrón mujeres: ", format(sum(dt$padron_mujeres, na.rm = TRUE), big.mark = ","))
    }
    if (length(lista_h_idx) > 0) {
      dt[, lista_hombres := get(col_names[lista_h_idx[1]])]
      message("✅ Lista hombres: ", format(sum(dt$lista_hombres, na.rm = TRUE), big.mark = ","))
    }
    if (length(lista_m_idx) > 0) {
      dt[, lista_mujeres := get(col_names[lista_m_idx[1]])]
      message("✅ Lista mujeres: ", format(sum(dt$lista_mujeres, na.rm = TRUE), big.mark = ","))
    }
    
    # Calcular tasa de inclusión
    if ("padron_electoral" %in% colnames(dt) && "lista_nominal" %in% colnames(dt)) {
      dt[padron_electoral > 0, tasa_inclusion := round((lista_nominal / padron_electoral) * 100, 2)]
      message("✅ Tasa de inclusión calculada")
    }
    
    message("✅ Columnas numéricas procesadas correctamente")
    
    # Aplicar filtros geográficos
    dt_filtrado <- dt
    
    if (estado != "Nacional" && "nombre_entidad" %in% colnames(dt)) {
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
    if ("nombre_entidad" %in% colnames(dt)) {
      todos_estados <- sort(unique(dt[nombre_entidad != "RESIDENTES EXTRANJERO" & 
                                        !is.na(nombre_entidad), nombre_entidad]))
    } else {
      todos_estados <- character(0)
    }
    
    todos_distritos <- if ("clave_distrito" %in% colnames(dt_filtrado)) {
      sort(unique(dt_filtrado[!is.na(clave_distrito) & clave_distrito != "0", clave_distrito]))
    } else character(0)
    
    todos_municipios <- if ("clave_municipio" %in% colnames(dt_filtrado)) {
      sort(unique(dt_filtrado[!is.na(clave_municipio) & clave_municipio != "0", clave_municipio]))
    } else character(0)
    
    todas_secciones <- if ("seccion" %in% colnames(dt_filtrado)) {
      sort(unique(dt_filtrado[!is.na(seccion) & seccion != "0", seccion]))
    } else character(0)
    
    message("✅ Datos históricos cargados: ", nrow(dt_filtrado), " filas")
    message("📊 Resumen final: padrón=", format(sum(dt_filtrado$padron_electoral, na.rm = TRUE), big.mark = ","), 
            ", lista=", format(sum(dt_filtrado$lista_nominal, na.rm = TRUE), big.mark = ","))
    
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
      message("❌ Fecha semanal incompleta: ", fecha_str)
      return(list(
        datos = data.frame(),
        metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
        todos_estados = character(0),
        todos_distritos = character(0),
        todos_municipios = character(0),
        todas_secciones = character(0)
      ))
    }
    
    # Cargar archivos según dimensión
    archivos <- list()
    
    if (dimension %in% c("sexo", "completo")) {
      archivos$sexo <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_sexo.csv"))
    }
    
    if (dimension %in% c("edad", "completo")) {
      archivos$edad <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_edad.csv"))
    }
    
    if (dimension %in% c("origen", "completo")) {
      archivos$origen <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_origen.csv"))
    }
    
    # Verificar existencia
    archivos_faltantes <- sapply(archivos, function(a) !file.exists(a))
    if (any(archivos_faltantes)) {
      message("❌ Archivos faltantes: ", paste(names(archivos)[archivos_faltantes], collapse = ", "))
      return(list(
        datos = data.frame(),
        metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
        todos_estados = character(0),
        todos_distritos = character(0),
        todos_municipios = character(0),
        todas_secciones = character(0)
      ))
    }
    
    # Cargar archivos - usar read.delim para SEMANALES (usan tabulador)
    dts <- list()
    for (tipo in names(archivos)) {
      dts[[tipo]] <- tryCatch({
        df <- read.delim(
          archivos[[tipo]],
          header = TRUE,
          stringsAsFactors = FALSE,
          fileEncoding = "UTF-8",
          check.names = FALSE,
          quote = "\"",
          na.strings = c("", "NA"),
          strip.white = TRUE
        )
        
        # Limpiar nombres
        nombres <- colnames(df)
        nombres <- gsub("\n", " ", nombres, fixed = TRUE)
        nombres <- gsub("\r", "", nombres, fixed = TRUE)
        nombres <- gsub("\\s+", " ", nombres)
        nombres <- trimws(nombres)
        colnames(df) <- nombres
        
        as.data.table(df)
      }, error = function(e) {
        message("❌ Error leyendo ", tipo, ": ", e$message)
        return(NULL)
      })
      
      if (is.null(dts[[tipo]])) {
        return(list(
          datos = data.frame(),
          metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
          todos_estados = character(0),
          todos_distritos = character(0),
          todos_municipios = character(0),
          todas_secciones = character(0)
        ))
      }
      
      dts[[tipo]] <- normalizar_columnas_semanal(dts[[tipo]])
      message("📊 Cargado (", tipo, "): ", nrow(dts[[tipo]]), " filas")
    }
    
    # Usar sexo como base (o el primero disponible)
    dt_base <- dts[[names(dts)[1]]]
    
    # Columnas geográficas
    cols_geo <- c("clave_entidad", "nombre_entidad", "clave_distrito", "cabecera_distrital", 
                  "clave_municipio", "nombre_municipio", "seccion")
    
    # Convertir columnas numéricas
    for (dt_nombre in names(dts)) {
      cols_num <- grep("padron|padrón|lista|hombres|mujeres|binario|electoral|nominal", 
                       colnames(dts[[dt_nombre]]), value = TRUE, ignore.case = TRUE)
      
      for (col in cols_num) {
        if (col %in% colnames(dts[[dt_nombre]])) {
          dts[[dt_nombre]][[col]] <- as.numeric(gsub('[,"]', '', as.character(dts[[dt_nombre]][[col]])))
        }
      }
    }
    
    dt_completo <- dt_base
    
    # Merge si es completo
    if (length(dts) > 1 && dimension == "completo") {
      for (tipo in setdiff(names(dts), names(dts)[1])) {
        cols_existentes <- intersect(colnames(dt_completo), colnames(dts[[tipo]]))
        cols_nuevas <- setdiff(colnames(dts[[tipo]]), cols_existentes)
        cols_merge <- c(cols_geo[cols_geo %in% colnames(dts[[tipo]])], cols_nuevas)
        
        dt_completo <- merge(dt_completo, dts[[tipo]][, ..cols_merge], 
                             by = cols_geo[cols_geo %in% colnames(dts[[tipo]])],
                             all.x = TRUE, all.y = FALSE)
        message("🔗 Merge ", tipo, " → ", nrow(dt_completo), " filas")
      }
    }
    
    # Limpiar filas especiales
    dt_completo <- limpiar_filas_especiales(dt_completo, incluir_extranjero = incluir_extranjero, 
                                            incluir_totales = FALSE)
    
    # Calcular tasa de inclusión si no existe
    if ("padron_electoral" %in% colnames(dt_completo) && "lista_nominal" %in% colnames(dt_completo)) {
      if (!"tasa_inclusion" %in% colnames(dt_completo)) {
        dt_completo[padron_electoral > 0, tasa_inclusion := round((lista_nominal / padron_electoral) * 100, 2)]
      }
    }
    
    # Aplicar filtros
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
    
    # Listas únicas
    if ("nombre_entidad" %in% colnames(dt_completo)) {
      todos_estados <- sort(unique(dt_completo[nombre_entidad != "RESIDENTES EXTRANJERO" & 
                                                 !is.na(nombre_entidad), nombre_entidad]))
    } else {
      todos_estados <- character(0)
    }
    
    todos_distritos <- if ("cabecera_distrital" %in% colnames(dt_filtrado)) {
      sort(unique(dt_filtrado[!is.na(cabecera_distrital), cabecera_distrital]))
    } else character(0)
    
    todos_municipios <- if ("nombre_municipio" %in% colnames(dt_filtrado)) {
      sort(unique(dt_filtrado[!is.na(nombre_municipio), nombre_municipio]))
    } else character(0)
    
    todas_secciones <- if ("seccion" %in% colnames(dt_filtrado)) {
      sort(unique(dt_filtrado[!is.na(seccion), seccion]))
    } else character(0)
    
    message("✅ Datos semanales cargados: ", nrow(dt_filtrado), " filas")
    
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
  
  message("❌ Tipo no válido: ", tipo_corte)
  return(list(
    datos = data.frame(),
    metadatos = list(tipo_corte = tipo_corte, fecha = fecha, dimension = dimension),
    todos_estados = character(0),
    todos_distritos = character(0),
    todos_municipios = character(0),
    todas_secciones = character(0)
  ))
}

# Funciones auxiliares
obtener_resumen_lne <- function(datos_lne) {
  if (is.null(datos_lne) || nrow(datos_lne$datos) == 0) {
    return(list(
      total_padron = 0, total_lista = 0, tasa_inclusion_promedio = 0,
      padron_hombres = 0, padron_mujeres = 0,
      lista_hombres = 0, lista_mujeres = 0, num_secciones = 0
    ))
  }
  
  datos <- datos_lne$datos
  resumen <- list(
    total_padron = sum(datos$padron_electoral, na.rm = TRUE),
    total_lista = sum(datos$lista_nominal, na.rm = TRUE),
    tasa_inclusion_promedio = round(mean(datos$tasa_inclusion, na.rm = TRUE), 2),
    num_secciones = nrow(datos)
  )
  
  if ("padron_hombres" %in% colnames(datos)) resumen$padron_hombres <- sum(datos$padron_hombres, na.rm = TRUE)
  if ("padron_mujeres" %in% colnames(datos)) resumen$padron_mujeres <- sum(datos$padron_mujeres, na.rm = TRUE)
  if ("lista_hombres" %in% colnames(datos)) resumen$lista_hombres <- sum(datos$lista_hombres, na.rm = TRUE)
  if ("lista_mujeres" %in% colnames(datos)) resumen$lista_mujeres <- sum(datos$lista_mujeres, na.rm = TRUE)
  
  return(resumen)
}

message("✅ Módulo datos_lne.R cargado")
message("📅 Fechas históricas: ", length(LNE_CATALOG$historico))
message("📅 Fechas semanales: ", length(LNE_CATALOG$semanal_comun))