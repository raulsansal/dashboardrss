# modules/elecciones_federales_server.R

elecciones_federales_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cargamos los archivos de módulos
    source("modules/elecciones_federales_server_main.R", local = TRUE)
    source("modules/elecciones_federales_server_text_analysis.R", local = TRUE)
    
    # Depurar inputs iniciales
    observe({
      message("🔍 Inputs iniciales: ",
              "year=", input$year %||% "NULL", ", ",
              "cargo=", input$cargo %||% "NULL", ", ",
              "tipo_eleccion=", input$tipo_eleccion %||% "NULL", ", ",
              "estado=", input$estado %||% "NULL", ", ",
              "partidos=", paste(input$partidos %||% "NULL", collapse = ","), ", ",
              "cabecera=", input$cabecera %||% "NULL", ", ",
              "municipio=", input$municipio %||% "NULL", ", ",
              "seccion=", paste(input$seccion %||% "NULL", collapse = ","))
    })
    
    # Definir combinacion_valida
    combinacion_valida <- reactive({
      message("🔍 [COMBINACION_VALIDA] Evaluando para year=", input$year %||% "NULL", 
              ", cargo=", input$cargo %||% "NULL")
      req(input$year, input$cargo)
      valid_combinations <- list(
        "2023" = c("SENADURIA"),
        "2021" = c("DIPUTACION FEDERAL", "SENADURIA"),
        "2018" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
        "2015" = c("DIPUTACION FEDERAL"),
        "2012" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
        "2009" = c("DIPUTACION FEDERAL"),
        "2006" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA")
      )
      year_str <- as.character(input$year)
      if (!year_str %in% names(valid_combinations)) {
        message("⚠️ [COMBINACION_VALIDA] Año inválido: ", year_str)
        return(FALSE)
      }
      valid_cargos <- valid_combinations[[year_str]]
      is_valid <- input$cargo %in% valid_cargos
      message("✅ [COMBINACION_VALIDA] Resultado: ", is_valid, " para year=", year_str, ", cargo=", input$cargo)
      is_valid
    })
    
    # Definir datos_columnas con valores predeterminados
    datos_columnas <- reactive({
      message("🔍 [DATOS_COLUMNAS] Iniciando para year=", input$year %||% "NULL", 
              ", cargo=", input$cargo %||% "NULL", ", tipo_eleccion=", input$tipo_eleccion %||% "NULL", 
              ", estado=", input$estado %||% "NULL", ", cabecera=", input$cabecera %||% "NULL", 
              ", municipio=", input$municipio %||% "NULL", ", seccion=", paste(input$seccion %||% "NULL", collapse = ", "), 
              " en ", Sys.time())
      
      # Usar isolate para evitar reevaluaciones innecesarias
      year <- isolate(input$year %||% "2021")
      cargo <- isolate(input$cargo %||% "DIPUTACION FEDERAL")
      tipo_eleccion <- isolate(input$tipo_eleccion %||% "AMBAS")
      estado <- isolate(input$estado %||% "Nacional")
      cabecera <- isolate(input$cabecera %||% "Todos")
      municipio <- isolate(input$municipio %||% "Todos")
      seccion <- isolate(if (is.null(input$seccion) || length(input$seccion) == 0) "Todas" else input$seccion)
      
      # Validar combinación antes de cargar datos
      if (!combinacion_valida()) {
        message("⚠️ [DATOS_COLUMNAS] Combinación no válida, retornando NULL")
        return(NULL)
      }
      
      # Cargar datos
      datos <- tryCatch({
        cargar_datos(
          anio = year,
          cargo = cargo,
          tipo_eleccion = tipo_eleccion,
          estado = estado,
          cabecera = cabecera,
          municipio = municipio,
          seccion = seccion
        )
      }, error = function(e) {
        message("⚠️ [DATOS_COLUMNAS] Error en cargar_datos: ", e$message)
        return(NULL)
      })
      
      # Depuración detallada
      message("🔍 [DATOS_COLUMNAS] Tipo de resultado: ", class(datos))
      message("🔍 [DATOS_COLUMNAS] Es NULL? ", is.null(datos))
      message("🔍 [DATOS_COLUMNAS] Nombres de resultado: ", 
              if (!is.null(datos) && is.list(datos)) paste(names(datos), collapse = ", ") else "No es lista o es NULL")
      if (!is.null(datos) && is.list(datos) && "datos" %in% names(datos)) {
        message("🔍 [DATOS_COLUMNAS] Es datos un data.frame? ", is.data.frame(datos$datos))
        message("🔍 [DATOS_COLUMNAS] Filas en datos: ", nrow(datos$datos))
        message("🔍 [DATOS_COLUMNAS] Columnas en datos: ", paste(colnames(datos$datos), collapse = ", "))
        # Validar columnas numéricas
        columnas_requeridas <- c("lne", "no_reg", "vot_nul")
        columnas_faltantes <- columnas_requeridas[!columnas_requeridas %in% colnames(datos$datos)]
        if (length(columnas_faltantes) > 0) {
          message("⚠️ [DATOS_COLUMNAS] Faltan columnas: ", paste(columnas_faltantes, collapse = ", "))
        } else {
          columnas_no_numericas <- columnas_requeridas[!sapply(datos$datos[columnas_requeridas], is.numeric)]
          if (length(columnas_no_numericas) > 0) {
            message("⚠️ [DATOS_COLUMNAS] Columnas no numéricas: ", paste(columnas_no_numericas, collapse = ", "))
          }
        }
      }
      
      datos
    })
    
    # Observar cuando se eliminan partidos
    observeEvent(input$partidos_removed, {
      req(input$partidos_removed$value)
      current_values <- input$partidos
      if (is.null(current_values) || length(current_values) == 0) {
        updateSelectizeInput(session, "partidos", selected = NULL)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Observar cuando se eliminan secciones electorales
    observeEvent(input$seccion_removed, {
      req(input$seccion_removed$value)
      current_values <- input$seccion
      if (is.null(current_values) || length(current_values) == 0) {
        updateSelectizeInput(session, "seccion", selected = NULL)
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Llamar directamente a los submódulos
    elecciones_federales_server_main(input, output, session, datos_columnas, combinacion_valida)
    elecciones_federales_server_text_analysis(input, output, session, datos_columnas)
  })
}