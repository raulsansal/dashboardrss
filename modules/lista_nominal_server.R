# modules/lista_nominal_server.R

# Configurar nombres de meses en español
meses_es <- c(
  "enero", "febrero", "marzo", "abril", "mayo", "junio",
  "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
)
names(meses_es) <- c(
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
)

# Función auxiliar para formatear fechas en español
formatear_fecha_es <- function(fecha, formato = "%d de %B de %Y") {
  if (is.null(fecha) || is.na(fecha)) return("")
  
  # Formatear en inglés primero
  fecha_str <- format(as.Date(fecha), formato)
  
  # Reemplazar nombres de meses inglés -> español
  for (mes_en in names(meses_es)) {
    fecha_str <- gsub(mes_en, meses_es[mes_en], fecha_str)
  }
  
  return(fecha_str)
}

lista_nominal_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Cargar submódulos
    source("modules/lista_nominal_server_main.R", local = TRUE)
    source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
    
    # Verificar carga de datos_lne
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
          "Periodo: 2017 a la última actualización</small>",
          "</div>"
        ))
      } else {
        HTML(paste0(
          "<div style='background-color: #fff4e6; padding: 10px; border-radius: 5px; margin-top: 10px;'>",
          "<small><strong>Datos semanales detallados</strong><br>",
          "Desgloses por edad, sexo y origen.<br>",
          "Periodo: enero 2025 a la última actualización</small>",
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
          if (length(catalog$historico) > 0) {
            años_disponibles <- sort(unique(format(catalog$historico, "%Y")), decreasing = TRUE)
            
            updateSelectInput(session, "year",
                              choices = años_disponibles,
                              selected = años_disponibles[1])
            
            message("📅 Años históricos actualizados: ", paste(años_disponibles, collapse = ", "))
          } else {
            updateSelectInput(session, "year", choices = NULL)
          }
          
        } else {
          if (length(catalog$semanal_comun) > 0) {
            años_disponibles <- sort(unique(format(catalog$semanal_comun, "%Y")), decreasing = TRUE)
            
            updateSelectInput(session, "year",
                              choices = años_disponibles,
                              selected = años_disponibles[1])
            
            message("📅 Años semanales actualizados: ", paste(años_disponibles, collapse = ", "))
          } else {
            updateSelectInput(session, "year", choices = NULL)
          }
        }
      }
    }, priority = 100)
    
    # ========== ACTUALIZAR FECHAS DISPONIBLES ==========
    
    observeEvent(list(input$tipo_corte, input$year), {
      req(input$tipo_corte, input$year)
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          fechas_year <- catalog$historico[format(catalog$historico, "%Y") == input$year]
          
          if (length(fechas_year) > 0) {
            fechas_year <- sort(fechas_year, decreasing = TRUE)
            
            choices <- setNames(
              as.character(fechas_year),
              sapply(fechas_year, formatear_fecha_es, formato = "%B %Y")
            )
            
            names(choices) <- paste0(toupper(substr(names(choices), 1, 1)), 
                                     substr(names(choices), 2, nchar(names(choices))))
            
            updateSelectInput(session, "date",
                              choices = choices,
                              selected = choices[1])
            
            message("📅 Fechas históricas para ", input$year, ": ", length(fechas_year), " opciones")
          } else {
            updateSelectInput(session, "date", choices = c("Sin datos" = ""))
          }
          
        } else {
          fechas_year <- catalog$semanal_comun[format(catalog$semanal_comun, "%Y") == input$year]
          
          if (length(fechas_year) > 0) {
            fechas_year <- sort(fechas_year, decreasing = TRUE)
            
            choices <- setNames(
              as.character(fechas_year),
              sapply(fechas_year, formatear_fecha_es, formato = "%d de %B de %Y")
            )
            
            updateSelectInput(session, "date",
                              choices = choices,
                              selected = choices[1])
            
            message("📅 Fechas semanales para ", input$year, ": ", length(fechas_year), " opciones")
          } else {
            updateSelectInput(session, "date", choices = c("Sin datos" = ""))
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
      fecha_formateada <- formatear_fecha_es(fecha, "%d de %B de %Y")
      
      HTML(paste0(
        "<div style='background-color: #f0f0f0; padding: 5px; border-radius: 3px; margin-top: 5px;'>",
        "<small><strong>Fecha seleccionada:</strong><br>",
        fecha_formateada, "</small>",
        "</div>"
      ))
    })
    
    # ========== SELECTOR DE DESGLOSE DINÁMICO (SOLO PARA SEMANAL) ==========
    
    output$selector_desglose <- renderUI({
      req(input$tipo_corte)
      
      if (input$tipo_corte == "semanal") {
        selectInput(
          ns("desglose"),
          "Desglose:",
          choices = c("Sexo", "Rango de Edad", "Entidad de Origen"),
          selected = "Sexo"
        )
      } else {
        return(NULL)
      }
    })
    
    # ========== ENCABEZADO PRINCIPAL ==========
    
    output$encabezado_principal <- renderUI({
      req(input$tipo_corte, input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(h3("Lista Nominal Electoral", style = "color: #666;"))
      }
      
      fecha <- as.Date(input$date)
      fecha_formateada <- formatear_fecha_es(fecha, "%d de %B de %Y")
      tipo_texto <- if (input$tipo_corte == "historico") "Datos Históricos" else "Datos Semanales"
      
      HTML(paste0(
        "<h3>Lista Nominal Electoral - ", tipo_texto, "</h3>",
        "<p style='font-size: 14px; color: #666;'>",
        "Corte: <strong>", fecha_formateada, "</strong> | ",
        "Ámbito: <strong>", input$entidad, "</strong>",
        "</p>"
      ))
    })
    
    # ========== REACTIVOS PRINCIPALES ==========
    
    combinacion_valida <- reactive({
      req(input$tipo_corte, input$date)
      
      if (input$date == "" || input$date == "Sin datos") {
        return(FALSE)
      }
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) {
        return(NULL)
      })
      
      if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
        return(FALSE)
      }
      
      if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
        catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
        
        if (input$tipo_corte == "historico") {
          return(fecha_seleccionada %in% catalog$historico)
        } else {
          return(fecha_seleccionada %in% catalog$semanal_comun)
        }
      }
      
      return(TRUE)
    })
    
    datos_columnas <- reactive({
      message("🔍 [DATOS_COLUMNAS_LNE] tipo=", input$tipo_corte %||% "NULL", 
              ", year=", input$year %||% "NULL", 
              ", date=", input$date %||% "NULL", 
              ", entidad=", input$entidad %||% "NULL")
      
      req(input$tipo_corte, input$year, input$date, input$entidad)
      
      if (input$date == "" || input$date == "Sin datos") {
        message("❌ Fecha no válida")
        return(NULL)
      }
      
      fecha_seleccionada <- tryCatch({
        as.Date(input$date)
      }, error = function(e) {
        message("❌ Error convirtiendo fecha: ", e$message)
        return(NULL)
      })
      
      if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
        message("❌ Fecha inválida")
        return(NULL)
      }
      
      estado_filtro <- if (input$entidad == "Nacional") "Nacional" else input$entidad
      distrito_filtro <- input$distrito %||% "Todos"
      municipio_filtro <- input$municipio %||% "Todos"
      seccion_filtro <- input$seccion %||% "Todas"
      
      dimension <- if (input$tipo_corte == "semanal") {
        switch(input$desglose %||% "Sexo",
               "Sexo" = "sexo",
               "Rango de Edad" = "edad",
               "Entidad de Origen" = "origen",
               "completo")
      } else {
        "completo"
      }
      
      message("📂 Llamando cargar_lne: tipo=", input$tipo_corte, 
              ", fecha=", fecha_seleccionada, 
              ", dimension=", dimension,
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
        message("❌ Error en cargar_lne: ", e$message)
        return(NULL)
      })
      
      if (is.null(datos_lne) || !is.list(datos_lne)) {
        message("❌ cargar_lne retornó NULL o no es lista")
        return(NULL)
      }
      
      if (!"datos" %in% names(datos_lne) || nrow(datos_lne$datos) == 0) {
        message("⚠️ Sin datos tras filtros")
        return(NULL)
      }
      
      message("✅ Datos LNE cargados: ", nrow(datos_lne$datos), " filas")
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
        message("🗺️ Distritos: ", length(distritos) - 1)
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
        message("🗺️ Municipios: ", length(municipios) - 1)
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
        message("🗺️ Secciones: ", length(secciones) - 1)
      }
    }, priority = 20)
    
    # ========== LLAMAR A SUBMÓDULOS ==========
    
    if (file.exists("modules/lista_nominal_server_main.R")) {
      source("modules/lista_nominal_server_main.R", local = TRUE)
      lista_nominal_server_main(input, output, session, datos_columnas, combinacion_valida)
    } else {
      message("⚠️ No se encontró lista_nominal_server_main.R")
    }
    
    if (file.exists("modules/lista_nominal_server_text_analysis.R")) {
      source("modules/lista_nominal_server_text_analysis.R", local = TRUE)
      lista_nominal_server_text_analysis(input, output, session, datos_columnas)
    } else {
      message("⚠️ No se encontró lista_nominal_server_text_analysis.R")
    }
    
    message("✅ Módulo lista_nominal_server inicializado")
  })
}