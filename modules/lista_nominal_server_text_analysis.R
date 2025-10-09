# modules/lista_nominal_server_text_analysis.R

lista_nominal_server_text_analysis <- function(input, output, session, datos_columnas) {
  ns <- session$ns
  
  # Título del análisis
  output$`text_analysis-titulo_lista` <- renderUI({
    req(input$year, input$date)
    
    fecha_formateada <- tryCatch({
      format(as.Date(input$date), "%d de %B de %Y")
    }, error = function(e) {
      input$date
    })
    
    HTML(paste0(
      "<h3>Lista Nominal Electoral</h3>",
      "<h4 class='cargo-subtitulo'>Corte: ", fecha_formateada, "</h4>"
    ))
  })
  
  # Alcance del análisis
  output$`text_analysis-alcance_lista` <- renderUI({
    req(input$entidad)
    
    alcance_texto <- if (input$entidad == "Nacional") {
      "Ámbito: Nacional"
    } else {
      texto_base <- paste("Entidad:", input$entidad)
      
      if (!is.null(input$distrito) && input$distrito != "Todos") {
        texto_base <- paste(texto_base, "- Distrito:", input$distrito)
      }
      
      if (!is.null(input$municipio) && input$municipio != "Todos") {
        texto_base <- paste(texto_base, "- Municipio:", input$municipio)
      }
      
      if (!is.null(input$seccion) && input$seccion != "Todas") {
        texto_base <- paste(texto_base, "- Sección:", input$seccion)
      }
      
      texto_base
    }
    
    HTML(paste0(
      "<h4>Alcance del análisis</h4>",
      "<p class='alcance-analisis'>", alcance_texto, "</p>"
    ))
  })
  
  # Resumen general
  output$`text_analysis-resumen_general_lista` <- renderUI({
    datos <- datos_columnas()
    
    if (is.null(datos) || nrow(datos$datos) == 0) {
      return(HTML("<p class='sidebar-alert'>No hay datos disponibles con los filtros seleccionados.</p>"))
    }
    
    # Calcular resumen usando la función auxiliar
    if (exists("obtener_resumen_lne")) {
      resumen <- obtener_resumen_lne(datos)
    } else {
      # Cálculo manual si la función no está disponible
      resumen <- list(
        total_padron = sum(datos$datos$padron_electoral, na.rm = TRUE),
        total_lista = sum(datos$datos$lista_nominal, na.rm = TRUE),
        tasa_inclusion_promedio = round(mean(datos$datos$tasa_inclusion, na.rm = TRUE), 2),
        num_secciones = nrow(datos$datos)
      )
    }
    
    HTML(paste0(
      "<h4>Resumen general</h4>",
      "<p>El padrón electoral totaliza <strong>", format(resumen$total_padron, big.mark = ","), " ciudadanos</strong>, ",
      "de los cuales <strong>", format(resumen$total_lista, big.mark = ","), "</strong> están incluidos en la lista nominal.</p>",
      "<p>Esto representa una <strong>tasa de inclusión promedio de ", resumen$tasa_inclusion_promedio, "%</strong> ",
      "en las ", resumen$num_secciones, " secciones analizadas.</p>"
    ))
  })
  
  # Demografía
  output$`text_analysis-demografia_lista` <- renderUI({
    datos <- datos_columnas()
    
    if (is.null(datos) || nrow(datos$datos) == 0) {
      return(NULL)
    }
    
    # Verificar si hay datos de sexo
    if ("padron_hombres" %in% colnames(datos$datos) && "padron_mujeres" %in% colnames(datos$datos)) {
      total_hombres <- sum(datos$datos$padron_hombres, na.rm = TRUE)
      total_mujeres <- sum(datos$datos$padron_mujeres, na.rm = TRUE)
      total <- total_hombres + total_mujeres
      
      if (total > 0) {
        pct_hombres <- round((total_hombres / total) * 100, 2)
        pct_mujeres <- round((total_mujeres / total) * 100, 2)
        
        HTML(paste0(
          "<h4>Composición por sexo</h4>",
          "<p>Del padrón electoral, <strong>", pct_hombres, "%</strong> son hombres (",
          format(total_hombres, big.mark = ","), ") y <strong>", pct_mujeres, "%</strong> son mujeres (",
          format(total_mujeres, big.mark = ","), ").</p>"
        ))
      } else {
        return(NULL)
      }
    } else {
      HTML("<p>Información demográfica detallada no disponible para este corte.</p>")
    }
  })
  
  # Comparación (placeholder por ahora)
  output$`text_analysis-comparacion_lista` <- renderUI({
    HTML(paste0(
      "<h4>Comparación temporal</h4>",
      "<p><em>Selecciona una segunda fecha para activar la comparación temporal.</em></p>"
    ))
  })
}