# modules/lista_nominal_server.R

lista_nominal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cargar submódulos
    source("modules/lista_nominal_server_main.R", local = TRUE)
    source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
    
    # Cargar datos_lne si no está ya cargado
    if (!exists("cargar_lne", envir = .GlobalEnv)) {
      source("server/datos_lne.R", local = TRUE)
    }
    
    # ========== INFORMACIÓN TIPO DE CORTE ==========
    
    output$info_tipo_corte <- renderUI({
      req(input$tipo_corte)
      
      if (input$tipo_corte == "historico") {
        HTML(paste0(
          "<div style='background-color: #e8f4f8; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos históricos mensuales</strong><br>",
          "Información agregada por entidad, distrito, municipio y sección.<br>",
          "Periodo: 2017 - presente</small>",
          "</div>"
        ))
      } else {
        HTML(paste0(
          "<div style='background-color: #fff4e6; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos semanales detallados</strong><br>",
          "Desgloses por edad, sexo y origen.<br>",
          "Periodo: enero 2025 - presente</small>",
          "</div>"
        ))
      }
    })
    
    # ========== ACTUALIZAR AÑOS DISPONIBLES ==========
    
    observeEvent(input$tipo_corte, {
      req(input$tipo_corte)
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          # Extraer años únicos de fechas históricas
          if (length(catalog$historico) > 0) {
            años_disponibles <- sort(unique(format(catalog$historico, "%Y")), decreasing = TRUE)
            
            updateSelectInput(session, "year",
                              choices = años_disponibles,
                              selected = años_disponibles[1])
            
            message("📅 Años históricos actualizados: ", paste(años_disponibles, collapse = ", "))
          } else {
            updateSelectInput(session, "year", choices = NULL)
            message("⚠️ No hay fechas históricas disponibles")
          }
          
        } else {  # semanal
          # Extraer años únicos de fechas semanales
          if (length(catalog$semanal_comun) > 0) {
            años_disponibles <- sort(unique(format(catalog$semanal_comun, "%Y")), decreasing = TRUE)
            
            updateSelectInput(session, "year",
                              choices = años_disponibles,
                              selected = años_disponibles[1])
            
            message("📅 Años semanales actualizados: ", paste(años_disponibles, collapse = ", "))
          } else {
            updateSelectInput(session, "year", choices = NULL)
            message("⚠️ No hay fechas semanales disponibles")
          }
        }
      } else {
        message("❌ LNE_CATALOG no está disponible")
      }
    }, priority = 100)  # Alta prioridad para ejecutar primero
    
    # ========== ACTUALIZAR FECHAS DISPONIBLES ==========
    
    observeEvent(list(input$tipo_corte, input$year), {
      req(input$tipo_corte, input$year)
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          # Filtrar fechas históricas por año
          fechas_year <- catalog$historico[format(catalog$historico, "%Y") == input$year]
          
          if (length(fechas_year) > 0) {
            # Ordenar de más reciente a más antiguo
            fechas_year <- sort(fechas_year, decreasing = TRUE)
            
            # Formatear para mostrar
            choices <- setNames(
              as.character(fechas_year),
              format(fechas_year, "%B %Y")  # Mes Año
            )
            
            updateSelectInput(session, "date",
                              choices = choices,
                              selected = choices[1])
            
            message("📅 Fechas históricas actualizadas para ", input$year, ": ", length(fechas_year), " opciones")
          } else {
            updateSelectInput(session, "date", choices = c("Sin datos" = ""))
            message("⚠️ No hay fechas históricas para ", input$year)
          }
          
        } else {  # semanal
          # Filtrar fechas semanales por año
          fechas_year <- catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == input$year]
          
          if (length(fechas_year) > 0) {
            # Ordenar de más reciente a más antiguo
            fechas_year <- sort(fechas_year, decreasing = TRUE)
            
            # Formatear para mostrar
            choices <- setNames(
              as.character(fechas_year),
              format(fechas_year, "%d de %B de %Y")  # Día de Mes de Año
            )
            
            updateSelectInput(session, "date",
                              choices = choices,
                              selected = choices[1])
            
            message("📅 Fechas semanales actualizadas para ", input$year, ": ", length(fechas_year), " opciones")
          } else {
            updateSelectInput(session, "date", choices = c("Sin datos" = ""))
            message("⚠️ No hay fechas semanales para ", input$year)
          }
        }
      }
    }, priority = 90)
    
    # ========== INFORMACIÓN DE FECHA SELECCIONADA ==========
    
    output$info_fecha <- renderUI({
      req(input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(NULL)
      }
      
      fecha <- as.Date(input$date)
      
      HTML(paste0(
        "<div style='background-color: #f0f0f0; padding: 5px; border-radius: 3px; margin-top: 5px;'>",
        "<small><strong>Fecha seleccionada:</strong><br>",
        format(fecha, "%d de %B de %Y"), "</small>",
        "</div>"
      ))
    })
    
    # ========== SELECTOR DE DESGLOSE DINÁMICO ==========
    
    output$selector_desglose <- renderUI({
      req(input$tipo_corte)
      
      if (input$tipo_corte == "historico") {
        # Para histórico, solo sexo está siempre disponible
        selectInput(
          ns("desglose"),
          "Desglose:",
          choices = c("Sexo", "Entidad de Origen"),
          selected = "Sexo"
        )
      } else {
        # Para semanal, están disponibles edad, sexo y origen
        selectInput(
          ns("desglose"),
          "Desglose:",
          choices = c("Sexo", "Rango de Edad", "Entidad de Origen"),
          selected = "Sexo"
        )
      }
    })
    
    # ========== ENCABEZADO PRINCIPAL ==========
    
    output$encabezado_principal <- renderUI({
      req(input$tipo_corte, input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(
          h3("Lista Nominal Electoral", style = "color: #666;")
        )
      }
      
      fecha <- as.Date(input$date)
      tipo_texto <- if (input$tipo_corte == "historico") "Datos Históricos" else "Datos Semanales"
      
      HTML(paste0(
        "<h3>Lista Nominal Electoral - ", tipo_texto, "</h3>",
        "<p style='font-size: 14px; color: #666;'>",
        "Corte: <strong>", format(fecha, "%d de %B de %Y"), "</strong> | ",
        "Ámbito: <strong>", input$entidad, "</strong>",
        "</p>"
      ))
    })
    
    # ========== REACTIVOS PRINCIPALES ==========
    
    combinacion_valida <- reactive({
      req(input$tipo_corte, input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        message("⚠️ Fecha no válida")
        return(FALSE)
      }
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) {
        message("❌ Error al parsear fecha: ", e$message)
        return(NULL)
      })
      
      if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
        message("⚠️ Fecha inválida en combinacion_valida")
        return(FALSE)
      }
      
      # Verificar que la fecha esté en el catálogo
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          fecha_valida <- fecha_seleccionada %in% catalog$historico
        } else {
          fecha_valida <- fecha_seleccionada %in% catalog$semanal_comun
        }
        
        message("✅ Validación de fecha (", input$tipo_corte, "): ", fecha_seleccionada, " → ", fecha_valida)
        return(fecha_valida)
      }
      
      return(TRUE)
    })
    
    datos_columnas <- reactive({
      message("🔍 [DATOS_COLUMNAS_LNE] Iniciando carga para tipo_corte=", input$tipo_corte %||% "NULL", 
              ", year=", input$year %||% "NULL", ", date=", input$date %||% "NULL", 
              ", entidad=", input$entidad %||% "NULL")
      
      # Validar entradas básicas
      req(input$tipo_corte, input$year, input$date, input$entidad)
      
      if (input$date == "" || input$date == "Sin datos") {
        message("❌ Fecha no seleccionada o sin datos")
        return(NULL)
      }
      
      # Convertir fecha de texto a Date
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) {
        message("❌ Error al convertir fecha: ", e$message)
        return(NULL)
      })
      
      if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
        message("❌ Fecha inválida")
        return(NULL)
      }
      
      # Preparar parámetros de filtro
      estado_filtro <- if (input$entidad == "Nacional") "Nacional" else input$entidad
      distrito_filtro <- input$distrito %||% "Todos"
      municipio_filtro <- input$municipio %||% "Todos"
      seccion_filtro <- input$seccion %||% "Todas"
      
      # Determinar dimensión según desglose (solo para semanal)
      dimension <- if (input$tipo_corte == "semanal") {
        "completo"  # Siempre cargar todo para semanal
      } else {
        "completo"
      }
      
      # Cargar datos
      message("📂 Llamando a cargar_lne con: tipo_corte=", input$tipo_corte, 
              ", fecha=", fecha_seleccionada, ", dimension=", dimension,
              ", estado=", estado_filtro)
      
      datos_lne <- tryCatch({
        cargar_lne(
          tipo_corte = input$tipo_corte,
          fecha = fecha_seleccionada,
          dimension = dimension,
          estado = estado_filtro,
          distrito = distrito_filtro,
          municipio = municipio_filtro,
          seccion = seccion_filtro,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("❌ Error al cargar datos LNE: ", e$message)
        message("❌ Stack trace: ", paste(traceback(), collapse = "\n"))
        return(NULL)
      })
      
      if (is.null(datos_lne)) {
        message("❌ cargar_lne retornó NULL")
        return(NULL)
      }
      
      if (!is.list(datos_lne)) {
        message("❌ datos_lne no es una lista: ", class(datos_lne))
        return(NULL)
      }
      
      if (!"datos" %in% names(datos_lne)) {
        message("❌ datos_lne no contiene el elemento 'datos'")
        message("🔍 Elementos disponibles: ", paste(names(datos_lne), collapse = ", "))
        return(NULL)
      }
      
      if (nrow(datos_lne$datos) == 0) {
        message("⚠️ Datos vacíos tras filtros")
        return(NULL)
      }
      
      message("✅ Datos LNE cargados: ", nrow(datos_lne$datos), " filas, ", 
              ncol(datos_lne$datos), " columnas")
      message("🔍 Columnas disponibles: ", paste(head(colnames(datos_lne$datos), 20), collapse = ", "))
      
      return(datos_lne)
    })
    
    # ========== ACTUALIZAR FILTROS GEOGRÁFICOS ==========
    
    observeEvent(datos_columnas(), {
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos)) {
        estados <- c("Nacional", datos$todos_estados)
        
        updateSelectInput(session, "entidad",
                          choices = estados,
                          selected = isolate(input$entidad) %||% "Nacional")
        
        message("🗺️ Estados actualizados: ", length(estados) - 1, " entidades")
      }
    }, priority = 50)
    
    observeEvent(list(datos_columnas(), input$entidad), {
      req(input$entidad)
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos) && input$entidad != "Nacional") {
        distritos <- c("Todos", datos$todos_distritos)
        updateSelectInput(session, "distrito",
                          choices = distritos,
                          selected = "Todos")
        message("🗺️ Distritos actualizados: ", length(distritos) - 1, " opciones")
      }
    }, priority = 40)
    
    observeEvent(list(datos_columnas(), input$distrito), {
      req(input$distrito)
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos)) {
        municipios <- c("Todos", datos$todos_municipios)
        updateSelectInput(session, "municipio",
                          choices = municipios,
                          selected = "Todos")
        message("🗺️ Municipios actualizados: ", length(municipios) - 1, " opciones")
      }
    }, priority = 30)
    
    observeEvent(list(datos_columnas(), input$municipio), {
      req(input$municipio)
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos)) {
        secciones <- c("Todas", datos$todas_secciones)
        updateSelectInput(session, "seccion",
                          choices = secciones,
                          selected = "Todas")
        message("🗺️ Secciones actualizadas: ", length(secciones) - 1, " opciones")
      }
    }, priority = 20)
    
    # ========== LLAMAR A SUBMÓDULOS ==========
    
    lista_nominal_server_main(input, output, session, datos_columnas, combinacion_valida)
    lista_nominal_server_text_analysis(input, output, session, datos_columnas)
    
    message("✅ Módulo lista_nominal_server inicializado correctamente")
  })
}