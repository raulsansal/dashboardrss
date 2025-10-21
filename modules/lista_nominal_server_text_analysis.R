# modules/lista_nominal_server_text_analysis.R

# Función auxiliar para formatear fechas en español (debe estar disponible del módulo principal)
if (!exists("formatear_fecha_es")) {
  meses_es <- c(
    "enero", "febrero", "marzo", "abril", "mayo", "junio",
    "julio", "agosto", "septiembre", "octubre", "noviembre", "diciembre"
  )
  names(meses_es) <- c(
    "January", "February", "March", "April", "May", "June",
    "July", "August", "September", "October", "November", "December"
  )
  
  formatear_fecha_es <- function(fecha, formato = "%d de %B de %Y") {
    if (is.null(fecha) || is.na(fecha)) return("")
    fecha_str <- format(as.Date(fecha), formato)
    for (mes_en in names(meses_es)) {
      fecha_str <- gsub(mes_en, meses_es[mes_en], fecha_str)
    }
    return(fecha_str)
  }
}

lista_nominal_server_text_analysis <- function(input, output, session, datos_columnas) {
  ns <- session$ns
  
  # ========== TÍTULO DEL ANÁLISIS ==========
  
  output$`text_analysis-titulo_lista` <- renderUI({
    req(input$year, input$date)
    
    fecha_formateada <- tryCatch({
      formatear_fecha_es(as.Date(input$date), "%d de %B de %Y")
    }, error = function(e) {
      input$date
    })
    
    HTML(paste0(
      "<h3>Lista Nominal Electoral</h3>",
      "<h4 class='cargo-subtitulo'>Corte: ", fecha_formateada, "</h4>"
    ))
  })
  
  # ========== ALCANCE DEL ANÁLISIS (MEJORADO - ESTILO ELECCIONES FEDERALES) ==========
  
  output$`text_analysis-alcance_lista` <- renderUI({
    req(input$entidad)
    
    # Construir texto de alcance similar a Elecciones Federales
    alcance_partes <- c()
    
    # Estado
    if (input$entidad == "Nacional") {
      alcance_texto <- "Ámbito: Nacional"
    } else {
      alcance_partes <- c(paste("Estado:", input$entidad))
      
      # Distrito
      if (!is.null(input$distrito) && input$distrito != "Todos") {
        alcance_partes <- c(alcance_partes, paste("Distrito:", input$distrito))
      }
      
      # Municipio
      if (!is.null(input$municipio) && input$municipio != "Todos") {
        alcance_partes <- c(alcance_partes, paste("Municipio:", input$municipio))
      }
      
      # Secciones
      if (!is.null(input$seccion) && length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
        if (length(input$seccion) == 1) {
          alcance_partes <- c(alcance_partes, paste("Sección:", input$seccion))
        } else if (length(input$seccion) <= 5) {
          # Si son 5 o menos, mostrar todas
          secciones_texto <- paste(input$seccion, collapse = ", ")
          alcance_partes <- c(alcance_partes, paste("Secciones:", secciones_texto))
        } else {
          # Si son más de 5, mostrar cantidad
          alcance_partes <- c(alcance_partes, paste("Secciones:", length(input$seccion), "seleccionadas"))
        }
      }
      
      # Unir con guiones como en Elecciones Federales
      alcance_texto <- paste(alcance_partes, collapse = " - ")
    }
    
    HTML(paste0(
      "<h4>Alcance del análisis</h4>",
      "<p class='alcance-analisis'>", alcance_texto, "</p>"
    ))
  })
  
  # ========== RESUMEN GENERAL ==========
  
  output$`text_analysis-resumen_general_lista` <- renderUI({
    datos <- datos_columnas()
    
    if (is.null(datos) || nrow(datos$datos) == 0) {
      return(HTML("<p class='sidebar-alert'>No hay datos disponibles con los filtros seleccionados.</p>"))
    }
    
    # Calcular resumen
    if (exists("obtener_resumen_lne")) {
      resumen <- obtener_resumen_lne(datos)
    } else {
      resumen <- list(
        total_padron = sum(datos$datos$padron_electoral, na.rm = TRUE),
        total_lista = sum(datos$datos$lista_nominal, na.rm = TRUE),
        tasa_inclusion_promedio = round(mean(datos$datos$tasa_inclusion, na.rm = TRUE), 2),
        num_secciones = nrow(datos$datos)
      )
    }
    
    # Calcular diferencia entre padrón y lista
    diferencia <- resumen$total_padron - resumen$total_lista
    
    # Construir texto descriptivo del filtro geográfico
    filtro_geo <- if (input$entidad == "Nacional") {
      "a nivel nacional"
    } else {
      texto_base <- paste("en el estado de", input$entidad)
      
      if (!is.null(input$distrito) && input$distrito != "Todos") {
        texto_base <- paste(texto_base, ", distrito", input$distrito)
      }
      
      if (!is.null(input$municipio) && input$municipio != "Todos") {
        texto_base <- paste(texto_base, ", municipio de", input$municipio)
      }
      
      if (!is.null(input$seccion) && length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
        if (length(input$seccion) == 1) {
          texto_base <- paste(texto_base, ", sección", input$seccion)
        } else if (length(input$seccion) <= 5) {
          secciones_texto <- paste(input$seccion, collapse = ", ")
          texto_base <- paste(texto_base, ", secciones", secciones_texto)
        } else {
          texto_base <- paste(texto_base, ",", length(input$seccion), "secciones seleccionadas")
        }
      }
      
      texto_base
    }
    
    HTML(paste0(
      "<h4>Resumen general</h4>",
      "<p>El padrón electoral ", filtro_geo, " totaliza <strong>", 
      format(resumen$total_padron, big.mark = ","), " ciudadanos</strong>. ",
      "De estos, <strong>", format(resumen$total_lista, big.mark = ","), 
      " están incluidos en la lista nominal</strong>, ",
      "lo que representa una <strong>tasa de inclusión del ", 
      resumen$tasa_inclusion_promedio, "%</strong>.</p>",
      "<p>La diferencia de <strong>", format(diferencia, big.mark = ","), " ciudadanos</strong> ",
      "corresponde a personas que están en el padrón pero aún no cumplen con los requisitos para ser incluidos ",
      "en la lista nominal (por ejemplo, trámites pendientes o suspensiones de derechos políticos).</p>",
      "<p><em>Análisis basado en ", resumen$num_secciones, " secciones electorales.</em></p>"
    ))
  })
  
  # ========== DEMOGRAFÍA ==========
  
  output$`text_analysis-demografia_lista` <- renderUI({
    datos <- datos_columnas()
    
    if (is.null(datos) || nrow(datos$datos) == 0) {
      return(NULL)
    }
    
    # Verificar si hay datos de sexo
    if (all(c("padron_hombres", "padron_mujeres") %in% colnames(datos$datos))) {
      
      total_hombres <- sum(datos$datos$padron_hombres, na.rm = TRUE)
      total_mujeres <- sum(datos$datos$padron_mujeres, na.rm = TRUE)
      total <- total_hombres + total_mujeres
      
      lista_hombres <- sum(datos$datos$lista_hombres, na.rm = TRUE)
      lista_mujeres <- sum(datos$datos$lista_mujeres, na.rm = TRUE)
      
      if (total > 0) {
        pct_padron_h <- round((total_hombres / total) * 100, 2)
        pct_padron_m <- round((total_mujeres / total) * 100, 2)
        
        tasa_inc_h <- round((lista_hombres / total_hombres) * 100, 2)
        tasa_inc_m <- round((lista_mujeres / total_mujeres) * 100, 2)
        
        # Determinar cuál sexo tiene mayor tasa
        comparacion <- if (tasa_inc_m > tasa_inc_h) {
          paste0("Las mujeres tienen una tasa de inclusión ligeramente mayor (", 
                 tasa_inc_m, "%) en comparación con los hombres (", tasa_inc_h, "%).")
        } else if (tasa_inc_h > tasa_inc_m) {
          paste0("Los hombres tienen una tasa de inclusión ligeramente mayor (", 
                 tasa_inc_h, "%) en comparación con las mujeres (", tasa_inc_m, "%).")
        } else {
          paste0("Tanto hombres como mujeres tienen una tasa de inclusión similar (", 
                 tasa_inc_h, "%).")
        }
        
        HTML(paste0(
          "<h4>Composición por sexo</h4>",
          "<p>Del padrón electoral, <strong>", pct_padron_h, "%</strong> son hombres (",
          format(total_hombres, big.mark = ","), ") y <strong>", pct_padron_m, "%</strong> son mujeres (",
          format(total_mujeres, big.mark = ","), ").</p>",
          "<p>En la lista nominal: <strong>", format(lista_hombres, big.mark = ","), " hombres</strong> y ",
          "<strong>", format(lista_mujeres, big.mark = ","), " mujeres</strong>.</p>",
          "<p><em>", comparacion, "</em></p>"
        ))
      } else {
        return(NULL)
      }
    } else {
      HTML("<p><em>Información demográfica detallada no disponible para este corte.</em></p>")
    }
  })
  
  # ========== COMPARACIÓN TEMPORAL ==========
  
  output$`text_analysis-comparacion_lista` <- renderUI({
    datos_actual <- datos_columnas()
    
    if (is.null(datos_actual) || nrow(datos_actual$datos) == 0) {
      return(NULL)
    }
    
    # Por ahora, mostrar un placeholder
    # En el futuro, se puede implementar una comparación con el periodo anterior
    
    tipo_periodo <- if (input$tipo_corte == "historico") "mensual" else "semanal"
    
    HTML(paste0(
      "<h4>Evolución temporal</h4>",
      "<p><em>La comparación con periodos anteriores estará disponible próximamente.</em></p>",
      "<p>Para realizar análisis comparativos, seleccione otra fecha de corte y ",
      "observe los cambios en el padrón electoral y la lista nominal.</p>"
    ))
  })
  
  message("✅ Módulo lista_nominal_server_text_analysis inicializado")
}