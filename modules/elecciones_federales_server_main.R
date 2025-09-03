# modules/elecciones_federales_server_main

elecciones_federales_server_main <- function(input, output, session, datos_columnas, combinacion_valida) {
  ns <- session$ns
  
  # Diccionario de mapeo para el gráfico de barras
  etiquetas_mapeo <- list(
    "vot_nul" = "VOTOS NULOS",
    "no_reg" = "NO REGISTRADO",
    "total_votos" = "TOTAL DE VOTOS",
    "lne" = "LISTA NOMINAL",
    "part_ciud" = "PARTICIPACIÓN CIUDADANA"
  )
  
  # Diccionario de mapeo para el DataTable
  etiquetas_mapeo_tabla <- list(
    "anio" = "Año",
    "cargo" = "Cargo",
    "estado" = "Estado",
    "cabecera" = "Cabecera Distrital",
    "seccion" = "Sección",
    "municipio" = "Municipio",
    "no_reg" = "No Registrados",
    "vot_nul" = "Votos Nulos",
    "total_votos" = "Total de Votos",
    "part_ciud" = "Participación"
  )
  
  # Variable reactiva para estados
  todos_estados <- reactiveVal(character(0))
  
  # Depurar datos_columnas
  observe({
    datos <- datos_columnas()
    if (is.list(datos)) {
      message("datos_columnas en main: ", paste(names(datos), collapse = ", "))
      message("todos_estados en datos_columnas: ", paste(datos$todos_estados, collapse = ", "))
    } else {
      message("datos_columnas no es una lista válida: ", class(datos))
    }
    if (!is.null(datos$todos_estados)) {
      todos_estados(datos$todos_estados)
    }
  })
  
  # Actualizar estados disponibles
  observeEvent(datos_columnas(), {
    req(is.list(datos_columnas()))
    updateSelectInput(session, "estado", 
                      choices = c("Nacional", todos_estados()), 
                      selected = input$estado %||% "Nacional")
  })
  
  # Actualizar partidos disponibles
  observeEvent(list(input$year, input$cargo), {
    req(input$year, input$cargo)
    key <- paste(input$year, input$cargo, sep = "_")
    columnas_disponibles <- partidos_mapping[[key]]
    if (is.null(columnas_disponibles)) {
      message("No hay columnas disponibles para ", key)
      columnas_disponibles <- character(0)
    }
    message("Actualizando partidos para ", key, ": ", paste(columnas_disponibles, collapse = ", "))
    updateSelectizeInput(session, "partidos", 
                         choices = c("Todos" = "Todos", columnas_disponibles), 
                         selected = "Todos",
                         options = list(
                           placeholder = "Selecciona partidos o coaliciones",
                           plugins = list("remove_button"),
                           maxItems = NULL
                         ))
  })
  
  # Actualizar cargo según año (sin tocar tipo_eleccion)
  observeEvent(input$year, {
    req(input$year)
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
    choices <- valid_combinations[[year_str]]
    if (is.null(choices)) {
      message("Año inválido en main: ", year_str)
      choices <- character(0)
    }
    
    selected <- if (!is.null(input$cargo) && input$cargo %in% choices) {
      input$cargo
    } else {
      choices[1]
    }
    
    # Resetear estado a "Nacional" excepto en casos específicos
    if (!(input$year == "2021" && input$cargo == "SENADURIA") && 
        !(input$year == "2023")) {
      isolate({
        updateSelectInput(session, "estado", 
                          choices = c("Nacional", todos_estados()),
                          selected = "Nacional")
      })
    }
    
    updateSelectInput(session, "cargo", 
                      choices = choices,
                      selected = selected)
  }, priority = 1)
  
  # Manejar casos especiales de elecciones y tipos de elección
  observeEvent(list(input$year, input$cargo, input$estado), {
    req(input$year, input$cargo, input$estado)
    req(datos_columnas())
    datos <- datos_columnas()
    req(is.list(datos))
    
    # Caso especial: 2015, DIPUTACION FEDERAL, AGUASCALIENTES
    if (input$year == "2015" && 
        input$cargo == "DIPUTACION FEDERAL" && 
        toupper(trimws(input$estado)) == "AGUASCALIENTES") {
      
      updateRadioButtons(session, "tipo_eleccion", 
                         choices = c("ORDINARIA", "EXTRAORDINARIA", "AMBAS"), 
                         selected = "AMBAS")
      
      message("✅ CASO ESPECIAL: 2015, DIPUTACION FEDERAL, AGUASCALIENTES → tipo_eleccion = AMBAS")
      
    } else if (input$year == "2023") {
      updateSelectInput(session, "estado", selected = "TAMAULIPAS")
      updateSelectizeInput(session, "partidos", selected = "Todos")
      updateRadioButtons(session, "tipo_eleccion", 
                         choices = c("EXTRAORDINARIA"),
                         selected = "EXTRAORDINARIA")
      
    } else if (input$year == "2021" && input$cargo == "SENADURIA") {
      updateSelectInput(session, "estado", selected = "NAYARIT")
      updateSelectizeInput(session, "partidos", selected = "Todos")
      updateRadioButtons(session, "tipo_eleccion", 
                         choices = c("EXTRAORDINARIA"),
                         selected = "EXTRAORDINARIA")
      
    } else {
      # Comportamiento general para el resto de combinaciones
      choices <- c()
      if (isTRUE(datos$tiene_ordinaria)) choices <- c(choices, "ORDINARIA")
      if (isTRUE(datos$tiene_extraordinaria)) choices <- c(choices, "EXTRAORDINARIA")
      if (length(choices) > 1) choices <- c(choices, "AMBAS")
      
      if (length(choices) == 0) {
        message("No hay tipos de elección disponibles para year=", input$year, ", cargo=", input$cargo)
        choices <- c("ORDINARIA")
      }
      
      selected <- if ("AMBAS" %in% choices) "AMBAS" else choices[1]
      
      updateRadioButtons(session, "tipo_eleccion", 
                         choices = choices,
                         selected = selected)
      
      message("✅ tipo_eleccion general: choices=", paste(choices, collapse = ", "), 
              ", selected=", selected)
    }
  })
  
  # Actualizar distritos según estado
  observeEvent(datos_columnas(), {
    req(input$estado)
    req(datos_columnas())
    datos <- datos_columnas()
    req(is.list(datos))
    
    # Solo actualizar si el estado sigue siendo el mismo
    # (evita actualizaciones fantasma tras cambios rápidos)
    if (input$estado != "Nacional") {
      choices <- c("Todos", sort(datos$todos_cabeceras))
      message("🔍 [ACTUALIZACIÓN CABECERA] Opciones disponibles: ", paste(choices, collapse = ", "))
      
      # Guardar selección actual para no perderla si ya estaba elegido
      current <- input$cabecera
      
      updateSelectInput(session, "cabecera", 
                        choices = choices,
                        selected = if (current %in% choices) current else "Todos")
    }
  })
  
  # Actualizar municipios según estado y distrito
  observeEvent(list(input$estado, input$cabecera), {
    req(input$estado, input$cabecera)
    datos <- datos_columnas()
    req(is.list(datos))
    updateSelectizeInput(session, "municipio", 
                         choices = c("Todos", datos$todos_municipios %||% character(0)), 
                         selected = "Todos")
  })
  
  # Actualizar secciones según distrito y municipio
  observeEvent(list(input$cabecera, input$municipio), {
    req(input$cabecera, input$municipio)
    datos <- datos_columnas()
    req(is.list(datos))
    updateSelectizeInput(session, "seccion", 
                         choices = c("Todas", datos$todas_secciones %||% character(0)), 
                         selected = "Todas")
  })
  
  # Manejar selección "Todas" en secciones
  observeEvent(input$seccion, {
    req(input$seccion)
    if ("Todas" %in% input$seccion) {
      updateSelectizeInput(session, "seccion", selected = "Todas")
    }
  })
  
  # Manejar selección "Todos" en partidos
  observeEvent(input$partidos, {
    req(input$partidos)
    if ("Todos" %in% input$partidos) {
      updateSelectizeInput(session, "partidos", selected = "Todos")
    }
  })
  
  # Renderizar tabla de datos
  output$`main-table_data` <- renderDT({
    req(combinacion_valida(), input$year, input$cargo, 
        input$estado, input$partidos, input$tipo_eleccion,
        input$cabecera, input$municipio, input$seccion)
    
    datos <- datos_columnas()$datos
    req(is.data.frame(datos))
    columnas_fijas <- c("anio", "cargo", "estado", "cabecera", "seccion", "municipio")
    
    key <- paste(input$year, input$cargo, sep = "_")
    columnas_partidos <- if ("Todos" %in% input$partidos) {
      partidos_mapping[[key]]
    } else {
      intersect(partidos_mapping[[key]], input$partidos)
    }
    
    # Añadir columnas adicionales si están en los datos
    columnas_adicionales <- c("total_votos", "part_ciud")
    columnas_adicionales <- columnas_adicionales[columnas_adicionales %in% colnames(datos)]
    message("Columnas adicionales incluidas: ", paste(columnas_adicionales, collapse = ", "))
    
    # Filtrar columnas que existan en datos
    selected_columns <- c(columnas_fijas, columnas_partidos, columnas_adicionales)
    selected_columns <- selected_columns[selected_columns %in% colnames(datos)]
    
    if (length(selected_columns) == 0) {
      message("No hay columnas válidas para mostrar en el DataTable")
      return(datatable(data.frame(Mensaje = "No hay datos disponibles"), 
                       options = list(pageLength = 10)))
    }
    
    # Crear nombres personalizados
    nombres_columnas <- sapply(selected_columns, function(col) {
      if (col %in% names(etiquetas_mapeo_tabla)) {
        etiquetas_mapeo_tabla[[col]]
      } else {
        col
      }
    })
    
    # Verificar longitud
    if (length(nombres_columnas) != length(selected_columns)) {
      message("Error: Longitud de nombres_columnas no coincide")
    }
    
    # Depuración
    message("Columnas seleccionadas: ", paste(selected_columns, collapse = ", "))
    message("Nombres de columnas aplicados: ", paste(nombres_columnas, collapse = ", "))
    
    # Renombrar columnas
    datos_tabla <- datos[, selected_columns, drop = FALSE]
    colnames(datos_tabla) <- nombres_columnas
    
    # --- 🔧 CORRECCIÓN: Formatear "Participación" como texto con % ---
    if ("Participación" %in% colnames(datos_tabla)) {
      datos_tabla[["Participación"]] <- sprintf("%.2f%%", datos_tabla[["Participación"]])
    }
    
    # --- 🔧 CORRECCIÓN: Solo aplicar comas a columnas numéricas de votos ---
    # Columnas que deben tener formato de miles
    columnas_con_comas <- c(
      "PAN", "PRI", "PRD", "PVEM", "PT", "MC", "MORENA", "NVALZ", "ASDC", "APM", 
      "CAND_IND1", "CAND_IND2", "PH", "ES", "FXM", "RSP",
      "Total de Votos", "Votos Nulos", "No Registrados"
    )
    
    # Índices 0-based para DataTable
    indices_con_comas <- which(colnames(datos_tabla) %in% columnas_con_comas) - 1
    indices_con_comas <- indices_con_comas[!is.na(indices_con_comas) & indices_con_comas >= 0]
    
    # Aplicar formato solo si hay columnas que lo requieran
    column_defs <- if (length(indices_con_comas) > 0) list(
      list(
        targets = indices_con_comas,
        render = JS(
          "function(data, type, row) {",
          "  return type === 'display' && data != null ?",
          "    data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') : data;",
          "}"
        )
      )
    ) else NULL
    
    dt <- datatable(
      datos_tabla, 
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: left; font-size: 10px; color: #666666; font-family: Arial, sans-serif;",
        "Fuente: INE. Sistema de Consulta de la Estadística de las Elecciones. https://siceen21.ine.mx/home    "
      ),
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "Todos")),
        dom = 'lfrtip',
        columnDefs = column_defs
      ),
      escape = FALSE
    )
    
    message("DataTable renderizado correctamente")
    dt
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("elecciones_federales_", input$year, "_", 
            gsub(" ", "_", input$estado), "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      datos <- datos_columnas()$datos
      req(is.data.frame(datos))
      columnas_fijas <- c("anio", "cargo", "estado", "cabecera", "seccion", "municipio")
      key <- paste(input$year, input$cargo, sep = "_")
      columnas_partidos <- if ("Todos" %in% input$partidos) {
        partidos_mapping[[key]]
      } else {
        intersect(partidos_mapping[[key]], input$partidos)
      }
      columnas_adicionales <- c("total_votos", "part_ciud")
      columnas_adicionales <- columnas_adicionales[columnas_adicionales %in% colnames(datos)]
      selected_columns <- c(columnas_fijas, columnas_partidos, columnas_adicionales)
      selected_columns <- selected_columns[selected_columns %in% colnames(datos)]
      
      datos_tabla <- datos[, selected_columns, drop = FALSE]
      nombres_columnas <- sapply(selected_columns, function(col) {
        if (col %in% names(etiquetas_mapeo_tabla)) {
          etiquetas_mapeo_tabla[[col]]
        } else {
          col
        }
      })
      colnames(datos_tabla) <- nombres_columnas
      
      # Depuración
      message("Filas exportadas en downloadHandler: ", nrow(datos_tabla))
      
      # Usar write.csv con quote=TRUE para manejar cadenas con comas
      write.csv(datos_tabla, file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
      write("Fuente: INE. Sistema de Consulta de la Estadística de las Elecciones. https://siceen21.ine.mx/home  ", 
            file, append = TRUE)
    }
  )
  
  # Manejar opción extraordinaria
  observeEvent(datos_columnas(), {
    datos <- datos_columnas()
    req(is.list(datos))
    tiene_extraordinaria <- isTRUE(datos$tiene_extraordinaria)
    if (!tiene_extraordinaria) {
      shinyjs::disable(selector = "input[type='radio'][value='EXTRAORDINARIA']")
      shinyjs::addClass(selector = "input[type='radio'][value='EXTRAORDINARIA']", 
                        class = "disabled-option")
    } else {
      shinyjs::enable(selector = "input[type='radio'][value='EXTRAORDINARIA']")
      shinyjs::removeClass(selector = "input[type='radio'][value='EXTRAORDINARIA']", 
                           class = "disabled-option")
    }
  })
  
  # Manejar principio electoral
  observeEvent(datos_columnas(), {
    datos <- datos_columnas()
    req(is.list(datos))
    
    choices <- c()
    if (isTRUE(datos$tiene_mayoria_relativa)) choices <- c(choices, "MAYORÍA RELATIVA")
    if (isTRUE(datos$tiene_representacion_proporcional)) choices <- c(choices, "REPRESENTACIÓN PROPORCIONAL")
    
    if (length(choices) == 0) {
      message("No hay principios electorales disponibles para year=", input$year)
      choices <- c("MAYORÍA RELATIVA")
    }
    
    selected <- if ("MAYORÍA RELATIVA" %in% choices) {
      "MAYORÍA RELATIVA"
    } else {
      choices[1]
    }
    
    updateRadioButtons(session, "principio_electoral",
                       choices = choices,
                       selected = selected)
    
    if (!isTRUE(datos$tiene_mayoria_relativa)) {
      shinyjs::disable(selector = "input[type='radio'][value='MAYORÍA RELATIVA']")
      shinyjs::addClass(selector = "input[type='radio'][value='MAYORÍA RELATIVA']", 
                        class = "disabled-option")
    } else {
      shinyjs::enable(selector = "input[type='radio'][value='MAYORÍA RELATIVA']")
      shinyjs::removeClass(selector = "input[type='radio'][value='MAYORÍA RELATIVA']", 
                           class = "disabled-option")
    }
    
    if (!isTRUE(datos$tiene_representacion_proporcional)) {
      shinyjs::disable(selector = "input[type='radio'][value='REPRESENTACIÓN PROPORCIONAL']")
      shinyjs::addClass(selector = "input[type='radio'][value='REPRESENTACIÓN PROPORCIONAL']", 
                        class = "disabled-option")
    } else {
      shinyjs::enable(selector = "input[type='radio'][value='REPRESENTACIÓN PROPORCIONAL']")
      shinyjs::removeClass(selector = "input[type='radio'][value='REPRESENTACIÓN PROPORCIONAL']", 
                           class = "disabled-option")
    }
  })
  
  # Crear contenedor para gráfico responsivo
  output$`main-plot_container` <- renderUI({
    plotlyOutput(ns("main-grafico_barras"), width = "100%", height = "auto")
  })
  
  # Generar gráfico de barras
  output$`main-grafico_barras` <- renderPlotly({
    req(combinacion_valida(), input$year, input$cargo, 
        input$estado, input$partidos, input$tipo_eleccion,
        input$cabecera, input$municipio, input$seccion)
    
    datos <- datos_columnas()$datos
    req(is.data.frame(datos))
    
    key <- paste(input$year, input$cargo, sep = "_")
    columnas_partidos <- if ("Todos" %in% input$partidos) {
      partidos_mapping[[key]]
    } else {
      intersect(partidos_mapping[[key]], input$partidos)
    }
    
    datos_grafico <- datos %>%
      select(all_of(columnas_partidos)) %>%
      pivot_longer(cols = everything(), names_to = "partido", values_to = "total_votos") %>%
      group_by(partido) %>%
      summarize(total_votos = sum(total_votos, na.rm = TRUE)) %>%
      arrange(desc(total_votos))
    
    # Aplicar mapeo de etiquetas
    datos_grafico <- datos_grafico %>%
      mutate(partido_etiqueta = sapply(partido, function(p) {
        if (p %in% names(etiquetas_mapeo)) etiquetas_mapeo[[p]] else p
      }))
    
    # Depurar mapeo
    message("Mapeo de etiquetas aplicado: ", 
            paste(datos_grafico$partido, "→", datos_grafico$partido_etiqueta, collapse = ", "))
    
    # Calcular el total de votos y el porcentaje
    total_votos_global <- sum(datos_grafico$total_votos, na.rm = TRUE)
    message("Total votos global: ", total_votos_global)
    datos_grafico <- datos_grafico %>%
      mutate(porcentaje = (total_votos / total_votos_global) * 100)
    
    # Depurar porcentajes
    message("Datos gráfico con porcentajes: ", 
            paste(datos_grafico$partido_etiqueta, ":", 
                  round(datos_grafico$porcentaje, 2), "%", collapse = ", "))
    
    colores <- sapply(datos_grafico$partido, function(p) {
      if (!is.null(partidos_colores[[p]])) {
        partidos_colores[[p]]
      } else {
        warning(paste("Color no definido para el partido:", p))
        "#808080"
      }
    })
    
    titulo <- paste("Votos por partido -", input$cargo, "-", input$year)
    subtitulo_base <- paste("Estado:", input$estado, "- Distrito:", input$cabecera, 
                            "- Municipio:", input$municipio)
    
    if (length(input$seccion) == 1 && input$seccion != "Todas") {
      subtitulo <- paste(subtitulo_base, "- Sección:", input$seccion)
      subtitulo_secciones <- NULL
    } else if (length(input$seccion) > 1) {
      subtitulo <- subtitulo_base
      subtitulo_secciones <- paste("Secciones:", paste(input$seccion, collapse = ", "))
    } else {
      subtitulo <- subtitulo_base
      subtitulo_secciones <- NULL
    }
    
    p <- ggplot(datos_grafico, 
                aes(x = reorder(partido_etiqueta, total_votos), 
                    y = total_votos, 
                    fill = partido, 
                    text = paste(partido_etiqueta, "<br>", 
                                 "Votos:", comma(total_votos), "<br>",
                                 "Porcentaje:", sprintf("%.2f%%", porcentaje)))) +
      geom_bar(stat = "identity") +
      scale_fill_manual(values = colores, guide = "none") +
      theme_minimal() +
      labs(title = titulo, x = NULL, y = NULL) +
      coord_flip() +
      theme(
        plot.margin = margin(20, 20, 20, 20),
        legend.position = "none",
        plot.title = element_text(face = "bold", size = rel(1.5), hjust = 0.5)
      )
    
    p_plotly <- ggplotly(p, tooltip = "text")
    
    p_plotly <- p_plotly %>%
      layout(
        title = list(
          text = titulo,
          font = list(size = 18, color = "black", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center",
          pad = list(b = 30)
        ),
        annotations = list(
          list(
            text = subtitulo,
            x = 0.5,
            y = 1.16,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 14, color = "#333333", family = "Arial, sans-serif"),
            align = "center"
          ),
          if (!is.null(subtitulo_secciones)) list(
            text = subtitulo_secciones,
            x = 0.5,
            y = 1.08,
            xref = "paper",
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 14, color = "#333333", family = "Arial, sans-serif"),
            align = "center"
          ),
          list(
            text = "Fuente: INE. Sistema de Consulta de la Estadística de las Elecciones. https://siceen21.ine.mx/home  ",
            x = 0.0,
            y = -0.15,
            xref = "paper",
            yref = "paper",
            xanchor = "left",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "left"
          )
        ),
        autosize = TRUE,
        margin = list(t = 120, l = 20, r = 20, b = 80),
        xaxis = list(
          title = "",
          showticklabels = FALSE,
          showgrid = FALSE
        ),
        yaxis = list(title = "")
      )
    
    for (i in seq_along(p_plotly$x$data)) {
      partido <- p_plotly$x$data[[i]]$name
      if (!is.null(partido) && partido %in% datos_grafico$partido) {
        p_plotly$x$data[[i]]$hoverlabel <- list(
          bgcolor = colores[datos_grafico$partido == partido]
        )
      }
    }
    
    message("Gráfico de barras renderizado correctamente con fuente")
    p_plotly
  })
  
  # Restablecer configuración al presionar el botón
  observeEvent(input$reset_config, {
    message("Restableciendo configuración de consulta")
    
    # Restablecer año
    updateSelectInput(session, "year", 
                      choices = c(2006, 2009, 2012, 2015, 2018, 2021, 2023), 
                      selected = 2021)
    
    # Restablecer cargo
    updateSelectInput(session, "cargo", 
                      choices = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"), 
                      selected = "DIPUTACION FEDERAL")
    
    # Restablecer estado
    updateSelectInput(session, "estado", 
                      choices = c("Nacional", todos_estados()), 
                      selected = "Nacional")
    
    # Restablecer tipo de elección
    updateRadioButtons(session, "tipo_eleccion", 
                       choices = c("ORDINARIA", "EXTRAORDINARIA", "AMBAS"), 
                       selected = "AMBAS")
    
    # Restablecer partidos
    key <- paste("2021", "DIPUTACION FEDERAL", sep = "_")
    columnas_disponibles <- partidos_mapping[[key]]
    if (is.null(columnas_disponibles)) columnas_disponibles <- character(0)
    updateSelectizeInput(session, "partidos", 
                         choices = c("Todos" = "Todos", columnas_disponibles), 
                         selected = "Todos",
                         options = list(
                           placeholder = "Selecciona partidos o coaliciones",
                           plugins = list("remove_button"),
                           maxItems = NULL
                         ))
    
    # Restablecer cabecera
    datos <- datos_columnas()
    updateSelectInput(session, "cabecera", 
                      choices = c("Todos", datos$todos_cabeceras %||% character(0)), 
                      selected = "Todos")
    
    # Restablecer municipio
    updateSelectizeInput(session, "municipio", 
                         choices = c("Todos", datos$todos_municipios %||% character(0)), 
                         selected = "Todos")
    
    # Restablecer sección
    updateSelectizeInput(session, "seccion", 
                         choices = c("Todas", datos$todas_secciones %||% character(0)), 
                         selected = "Todas")
    
    # Restablecer principio electoral
    updateRadioButtons(session, "principio_electoral", 
                       choices = c("MAYORÍA RELATIVA", "REPRESENTACIÓN PROPORCIONAL"), 
                       selected = "MAYORÍA RELATIVA")
    
    message("Configuración restablecida correctamente")
  })
}