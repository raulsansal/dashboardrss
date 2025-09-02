# modules/elecciones_federales_server.R
elecciones_federales_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cargamos los archivos de módulos
    source("modules/elecciones_federales_server_main.R", local = TRUE)
    source("modules/elecciones_federales_server_text_analysis.R", local = TRUE)
    
    # Depurar inputs iniciales
    observe({
      message("Inputs iniciales: ",
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
        message("Año inválido en elecciones_federales_server: ", year_str)
        return(FALSE)
      }
      valid_cargos <- valid_combinations[[year_str]]
      is_valid <- input$cargo %in% valid_cargos
      message("combinacion_valida: ", is_valid, " para year=", year_str, ", cargo=", input$cargo)
      is_valid
    })
    
    # Definir datos_columnas con valores predeterminados
    datos_columnas <- reactive({
      year <- input$year %||% "2021"
      cargo <- input$cargo %||% "DIPUTACION FEDERAL"
      tipo_eleccion <- input$tipo_eleccion %||% "AMBAS"
      estado <- input$estado %||% "Nacional"
      cabecera <- input$cabecera %||% "Todos"
      municipio <- input$municipio %||% "Todos"
      seccion <- if (is.null(input$seccion) || length(input$seccion) == 0) "Todas" else input$seccion
      
      req(combinacion_valida())
      
      datos <- cargar_datos(
        year,
        cargo,
        tipo_eleccion,
        estado,
        cabecera,
        municipio,
        seccion
      )
      message("datos_columnas retornó: ", class(datos), ", nombres: ", 
              if (is.list(datos)) paste(names(datos), collapse = ", ") else "sin nombres")
      message("todos_estados: ", paste(datos$todos_estados %||% "NULL", collapse = ", "))
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