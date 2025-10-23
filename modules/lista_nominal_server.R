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
    
    # ========== FUNCIÓN AUXILIAR: CARGA INICIAL RÁPIDA ==========
    
    cargar_datos_defecto <- function() {
      message("🚀 [CARGA INICIAL] Cargando datos por defecto...")
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      # Obtener última fecha histórica disponible
      ultima_fecha <- max(catalog$historico)
      
      message("📅 [CARGA INICIAL] Última fecha: ", ultima_fecha)
      
      # Cargar SOLO 1 archivo: último mensual Nacional sin filtros
      datos_lne <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = ultima_fecha,
          dimension = "completo",
          estado = "Nacional",
          distrito = "Todos",
          municipio = "Todos",
          seccion = "Todas",
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("❌ [CARGA INICIAL] Error: ", e$message)
        return(NULL)
      })
      
      if (!is.null(datos_lne)) {
        message("✅ [CARGA INICIAL] Datos cargados: ", nrow(datos_lne$datos), " filas")
      }
      
      return(datos_lne)
    }
    
    # ========== REACTIVOS PRINCIPALES ==========
    
    combinacion_valida <- reactive({
      # CARGA INICIAL: siempre válida
      if (input$btn_consultar == 0) {
        return(TRUE)
      }
      
      # CARGA PERSONALIZADA: validar inputs
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
    
    # ========== REACTIVE OPTIMIZADO: datos_columnas CON BOTÓN ==========
    
    datos_columnas <- reactive({
      # ========== CARGA INICIAL (SIN BOTÓN PRESIONADO) ==========
      if (input$btn_consultar == 0) {
        message("🚀 [DATOS_COLUMNAS] CARGA INICIAL - Sin botón presionado")
        return(cargar_datos_defecto())
      }
      
      # ========== CARGA PERSONALIZADA (BOTÓN PRESIONADO) ==========
      message("🔍 [DATOS_COLUMNAS] CARGA PERSONALIZADA - Botón presionado: ", input$btn_consultar)
      
      # Aislar inputs para evitar reactividad no deseada
      tipo_corte <- isolate(input$tipo_corte)
      year <- isolate(input$year)
      date <- isolate(input$date)
      entidad <- isolate(input$entidad)
      distrito <- isolate(input$distrito %||% "Todos")
      municipio <- isolate(input$municipio %||% "Todos")
      seccion <- isolate(input$seccion %||% "Todas")
      desglose <- isolate(input$desglose %||% "Sexo")
      
      message("📊 Configuración: tipo=", tipo_corte, ", fecha=", date, ", entidad=", entidad)
      
      if (date == "" || date == "Sin datos") {
        message("❌ Fecha no válida")
        return(NULL)
      }
      
      fecha_seleccionada <- tryCatch({
        as.Date(date)
      }, error = function(e) {
        message("❌ Error convirtiendo fecha: ", e$message)
        return(NULL)
      })
      
      if (is.null(fecha_seleccionada) || is.na(fecha_seleccionada)) {
        message("❌ Fecha inválida")
        return(NULL)
      }
      
      estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
      
      dimension <- if (tipo_corte == "semanal") {
        switch(desglose,
               "Sexo" = "sexo",
               "Rango de Edad" = "edad",
               "Entidad de Origen" = "origen",
               "completo")
      } else {
        "completo"
      }
      
      message("📂 Llamando cargar_lne: tipo=", tipo_corte, ", fecha=", fecha_seleccionada, 
              ", dimension=", dimension, ", estado=", estado_filtro)
      
      datos_lne <- tryCatch({
        cargar_lne(
          tipo_corte = tipo_corte,
          fecha = fecha_seleccionada,
          dimension = dimension,
          estado = estado_filtro,
          distrito = distrito,
          municipio = municipio,
          seccion = seccion,
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
      
    }) %>% bindCache(input$btn_consultar, input$tipo_corte, input$date, 
                     input$entidad, input$distrito, input$municipio, input$seccion)
    
    # ========== ACTUALIZAR FILTROS GEOGRÁFICOS (SIN DISPARAR CARGAS) ==========
    
    observeEvent(datos_columnas(), {
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos)) {
        estados <- c("Nacional", datos$todos_estados)
        
        # PRESERVAR selección actual si existe
        current_estado <- isolate(input$entidad)
        selected_estado <- if (!is.null(current_estado) && current_estado %in% estados) {
          current_estado
        } else {
          "Nacional"
        }
        
        updateSelectInput(session, "entidad",
                          choices = estados,
                          selected = selected_estado)
        
        message("🗺️ Estados actualizados: ", length(estados) - 1, " entidades")
      }
    }, priority = 50)
    
    # PRESERVAR SELECCIÓN DE DISTRITO
    observeEvent(input$entidad, {
      req(input$entidad)
      
      if (input$entidad != "Nacional") {
        datos <- datos_columnas()
        
        if (!is.null(datos) && is.list(datos)) {
          distritos <- c("Todos", datos$todos_distritos)
          
          current_distrito <- isolate(input$distrito)
          selected_distrito <- if (!is.null(current_distrito) && current_distrito %in% distritos) {
            current_distrito
          } else {
            "Todos"
          }
          
          updateSelectInput(session, "distrito",
                            choices = distritos,
                            selected = selected_distrito)
          
          message("🗺️ Distritos actualizados: ", length(distritos) - 1, " - Seleccionado: ", selected_distrito)
        }
      }
    }, priority = 40, ignoreInit = TRUE)
    
    # PRESERVAR SELECCIÓN DE MUNICIPIO
    observeEvent(input$distrito, {
      req(input$distrito)
      
      datos <- datos_columnas()
      
      if (!is.null(datos) && is.list(datos)) {
        municipios <- c("Todos", datos$todos_municipios)
        
        current_municipio <- isolate(input$municipio)
        selected_municipio <- if (!is.null(current_municipio) && current_municipio %in% municipios) {
          current_municipio
        } else {
          "Todos"
        }
        
        updateSelectInput(session, "municipio",
                          choices = municipios,
                          selected = selected_municipio)
        
        message("🗺️ Municipios actualizados: ", length(municipios) - 1, " - Seleccionado: ", selected_municipio)
      }
    }, priority = 30, ignoreInit = TRUE)
    
    # PRESERVAR SELECCIÓN DE SECCIONES
    observeEvent(input$municipio, {
      req(input$municipio)
      
      datos <- isolate(datos_columnas())
      
      if (!is.null(datos) && is.list(datos)) {
        secciones <- c("Todas", datos$todas_secciones)
        
        current_seccion <- isolate(input$seccion)
        
        if (!is.null(current_seccion) && length(current_seccion) > 0) {
          if ("Todas" %in% current_seccion) {
            selected_seccion <- "Todas"
          } else {
            valid_secciones <- current_seccion[current_seccion %in% secciones]
            selected_seccion <- if (length(valid_secciones) > 0) valid_secciones else "Todas"
          }
        } else {
          selected_seccion <- "Todas"
        }
        
        updateSelectizeInput(session, "seccion",
                             choices = secciones,
                             selected = selected_seccion,
                             options = list(
                               placeholder = "Selecciona una o más secciones",
                               plugins = list("remove_button"),
                               maxItems = NULL
                             ))
        
        message("🗺️ Secciones actualizadas: ", length(secciones) - 1, " - Seleccionadas: ", paste(selected_seccion, collapse = ", "))
      }
    }, priority = 20, ignoreInit = TRUE)
    
    # MANEJAR SELECCIÓN DE "TODAS"
    observeEvent(input$seccion, {
      req(input$seccion)
      
      if (length(input$seccion) > 1 && "Todas" %in% input$seccion) {
        updateSelectizeInput(session, "seccion", 
                             selected = "Todas",
                             options = list(
                               placeholder = "Selecciona una o más secciones",
                               plugins = list("remove_button"),
                               maxItems = NULL
                             ))
        message("🗺️ Usuario seleccionó 'Todas' - limpiando otras selecciones")
      }
    }, priority = 10, ignoreInit = TRUE)
    
    # ========== LLAMAR A SUBMÓDULOS ==========
    
    if (file.exists("modules/lista_nominal_server_main.R")) {
      source("modules/lista_nominal_server_main.R", local = TRUE)
      lista_nominal_server_main(input, output, session, datos_columnas, combinacion_valida)
    } else {
      message("⚠️ No se encontró lista_nominal_server_main.R")
    }
    
    if (file.exists("modules/lista_nominal_server_graficas.R")) {
      source("modules/lista_nominal_server_graficas.R", local = TRUE)
      lista_nominal_server_graficas(input, output, session, datos_columnas, combinacion_valida)
    } else {
      message("⚠️ No se encontró lista_nominal_server_graficas.R")
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