# server/datos_lne.R 

source("utils_lne.R")

# Cargar catálogo global una vez al iniciar
LNE_CATALOG <- build_lne_catalog()
LNE_DIRS <- find_lne_data_dirs()

#' @title Cargar datos de Lista Nominal Electoral
#' @param tipo_corte "historico" o "semanal"
#' @param fecha Fecha de corte (Date)
#' @param dimension Solo para semanal: "edad", "origen", "sexo" o "completo"
#' @param estado, distrito, municipio, seccion Filtros geográficos
#' @return Lista con datos, metadatos y listas únicas
cargar_lne <- function(tipo_corte, fecha, dimension = "completo",
                       estado = "Nacional", distrito = "Todos", 
                       municipio = "Todos", seccion = "Todas") {
  
  if (is.null(fecha) || is.na(fecha)) {
    message("❌ Fecha inválida en cargar_lne()")
    return(list(datos = data.frame(), todos_estados = character(0)))
  }
  
  fecha_str <- format(fecha, "%Y%m%d")
  
  if (tipo_corte == "historico") {
    archivo <- file.path(LNE_DIRS$historico, paste0("derfe_pdln_", fecha_str, "_base.csv"))
    if (!file.exists(archivo)) {
      message("❌ Archivo histórico no encontrado: ", archivo)
      return(list(datos = data.frame(), todos_estados = character(0)))
    }
    
    dt <- data.table::fread(archivo, showProgress = FALSE)
    
    # Renombrar columnas para consistencia
    setnames(dt, 
             old = c("PADRÓN ELECTORAL NACIONAL,,,,HOMBRES", 
                     "PADRÓN ELECTORAL NACIONAL,,,,MUJERES", 
                     "PADRÓN ELECTORAL NACIONAL,,,,NO BINARIO", 
                     "PADRÓN ELECTORAL NACIONAL,,,,TOTAL",
                     "LISTA NOMINAL DE ELECTORES NACIONAL,,,,HOMBRES",
                     "LISTA NOMINAL DE ELECTORES NACIONAL,,,,MUJERES",
                     "LISTA NOMINAL DE ELECTORES NACIONAL,,,,NO BINARIO",
                     "LISTA NOMINAL DE ELECTORES NACIONAL,,,,TOTAL"),
             new = c("padron_hombres", "padron_mujeres", "padron_no_binario", "padron_electoral",
                     "lista_hombres", "lista_mujeres", "lista_no_binario", "lista_nominal"))
    
    # Normalizar claves
    dt[, `CLAVE ENTIDAD` := as.character(`CLAVE ENTIDAD`)]
    dt[, `CLAVE DISTRITO` := as.character(`CLAVE DISTRITO`)]
    dt[, `CLAVE MUNICIPIO` := as.character(`CLAVE MUNICIPIO`)]
    
    # Agregar nombres de entidad (opcional: cargar desde catálogo externo)
    entidades <- c(
      "1" = "AGUASCALIENTES", "2" = "BAJA CALIFORNIA", "3" = "BAJA CALIFORNIA SUR",
      "4" = "CAMPECHE", "5" = "COAHUILA", "6" = "COLIMA", "7" = "CHIAPAS",
      "8" = "CHIHUAHUA", "9" = "CIUDAD DE MEXICO", "10" = "DURANGO",
      "11" = "GUANAJUATO", "12" = "GUERRERO", "13" = "HIDALGO", "14" = "JALISCO",
      "15" = "MEXICO", "16" = "MICHOACAN", "17" = "MORELOS", "18" = "NAYARIT",
      "19" = "NUEVO LEON", "20" = "OAXACA", "21" = "PUEBLA", "22" = "QUERETARO",
      "23" = "QUINTANA ROO", "24" = "SAN LUIS POTOSI", "25" = "SINALOA",
      "26" = "SONORA", "27" = "TABASCO", "28" = "TAMAULIPAS", "29" = "TLAXCALA",
      "30" = "VERACRUZ DE IGNACIO DE LA LLAVE", "31" = "YUCATAN", "32" = "ZACATECAS",
      "0" = "RESIDENTES EXTRANJERO"
    )
    
    dt[, nombre_entidad := entidades[as.character(`CLAVE ENTIDAD`)]]
    dt[is.na(nombre_entidad), nombre_entidad := "DESCONOCIDO"]
    
    # Filtrar por geografía
    dt_filtrado <- dt
    if (estado != "Nacional") {
      dt_filtrado <- dt_filtrado[nombre_entidad == estado, ]
    }
    if (distrito != "Todos") {
      dt_filtrado <- dt_filtrado[`CLAVE DISTRITO` == distrito, ]
    }
    if (municipio != "Todos") {
      dt_filtrado <- dt_filtrado[`CLAVE MUNICIPIO` == municipio, ]
    }
    if (!identical(seccion, "Todas") && !"Todas" %in% seccion) {
      dt_filtrado <- dt_filtrado[SECCION %in% seccion, ]
    }
    
    # Listas únicas
    todos_estados <- sort(unique(dt[nombre_entidad != "RESIDENTES EXTRANJERO", nombre_entidad]))
    todos_distritos <- sort(unique(dt_filtrado$`CLAVE DISTRITO`))
    todos_municipios <- sort(unique(dt_filtrado$`CLAVE MUNICIPIO`))
    todas_secciones <- sort(unique(dt_filtrado$SECCION))
    
    return(list(
      datos = as.data.frame(dt_filtrado),
      todos_estados = todos_estados,
      todos_distritos = todos_distritos,
      todos_municipios = todos_municipios,
      todas_secciones = todas_secciones
    ))
    
  } else if (tipo_corte == "semanal") {
    if (!fecha %in% LNE_CATALOG$semanal_comun) {
      message("❌ Fecha semanal incompleta: ", fecha_str)
      return(list(datos = data.frame(), todos_estados = character(0)))
    }
    
    # Cargar los tres archivos
    archivo_edad <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_edad.csv"))
    archivo_origen <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_origen.csv"))
    archivo_sexo <- file.path(LNE_DIRS$semanal, paste0("derfe_pdln_", fecha_str, "_sexo.csv"))
    
    if (!all(file.exists(archivo_edad, archivo_origen, archivo_sexo))) {
      message("❌ Faltan archivos semanales para: ", fecha_str)
      return(list(datos = data.frame(), todos_estados = character(0)))
    }
    
    dt_edad <- data.table::fread(archivo_edad, showProgress = FALSE)
    dt_origen <- data.table::fread(archivo_origen, showProgress = FALSE)
    dt_sexo <- data.table::fread(archivo_sexo, showProgress = FALSE)
    
    # Unir por clave geográfica
    dt_completo <- merge(dt_sexo, dt_edad, 
                         by = c("CLAVE ENTIDAD", "NOMBRE ENTIDAD", "CLAVE DISTRITO", 
                                "CABECERA DISTRITAL", "CLAVE MUNICIPIO", "NOMBRE MUNICIPIO", "SECCION"),
                         suffixes = c("", "_edad"))
    dt_completo <- merge(dt_completo, dt_origen,
                         by = c("CLAVE ENTIDAD", "NOMBRE ENTIDAD", "CLAVE DISTRITO", 
                                "CABECERA DISTRITAL", "CLAVE MUNICIPIO", "NOMBRE MUNICIPIO", "SECCION"),
                         suffixes = c("", "_origen"))
    
    # Filtrar por geografía
    dt_filtrado <- dt_completo
    if (estado != "Nacional") {
      dt_filtrado <- dt_filtrado[`NOMBRE ENTIDAD` == estado, ]
    }
    if (distrito != "Todos") {
      dt_filtrado <- dt_filtrado[`CABECERA DISTRITAL` == distrito, ]
    }
    if (municipio != "Todos") {
      dt_filtrado <- dt_filtrado[`NOMBRE MUNICIPIO` == municipio, ]
    }
    if (!identical(seccion, "Todas") && !"Todas" %in% seccion) {
      dt_filtrado <- dt_filtrado[SECCION %in% seccion, ]
    }
    
    todos_estados <- sort(unique(dt_completo$`NOMBRE ENTIDAD`))
    todos_distritos <- sort(unique(dt_filtrado$`CABECERA DISTRITAL`))
    todos_municipios <- sort(unique(dt_filtrado$`NOMBRE MUNICIPIO`))
    todas_secciones <- sort(unique(dt_filtrado$SECCION))
    
    return(list(
      datos = as.data.frame(dt_filtrado),
      todos_estados = todos_estados,
      todos_distritos = todos_distritos,
      todos_municipios = todos_municipios,
      todas_secciones = todas_secciones
    ))
  }
  
  return(list(datos = data.frame(), todos_estados = character(0)))
}