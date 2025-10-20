# modules/lista_nominal_server_graficas.R
# Módulo especializado en la generación de gráficas para Lista Nominal Electoral

lista_nominal_server_graficas <- function(input, output, session, datos_columnas, combinacion_valida) {
  
  # ========== REACTIVE: CARGAR DATOS HISTÓRICOS DEL AÑO SELECCIONADO ==========
  
  datos_historicos_year <- reactive({
    req(input$tipo_corte == "historico")
    req(input$year)
    req(input$date)
    
    message("🔄 Cargando datos históricos del año ", input$year, "...")
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    
    # SOLO cargar fechas del año seleccionado
    fechas_year <- catalog$historico[format(catalog$historico, "%Y") == input$year]
    
    if (length(fechas_year) == 0) {
      return(NULL)
    }
    
    # Obtener filtros geográficos
    estado_filtro <- if (input$entidad == "Nacional") "Nacional" else input$entidad
    distrito_filtro <- input$distrito %||% "Todos"
    municipio_filtro <- input$municipio %||% "Todos"
    seccion_filtro <- input$seccion %||% "Todas"
    
    message("📥 Cargando ", length(fechas_year), " fechas del año ", input$year, "...")
    
    lista_datos <- list()
    
    for (i in seq_along(fechas_year)) {
      fecha <- fechas_year[i]
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = distrito_filtro,
          municipio = municipio_filtro,
          seccion = seccion_filtro,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        return(NULL)
      })
      
      if (!is.null(datos_temp) && !is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
        df <- datos_temp$datos
        
        totales <- data.frame(
          fecha = as.Date(fecha, origin = "1970-01-01"),
          padron_electoral = sum(df$padron_electoral, na.rm = TRUE),
          lista_nominal = sum(df$lista_nominal, na.rm = TRUE),
          padron_hombres = if ("padron_hombres" %in% colnames(df)) sum(df$padron_hombres, na.rm = TRUE) else NA,
          padron_mujeres = if ("padron_mujeres" %in% colnames(df)) sum(df$padron_mujeres, na.rm = TRUE) else NA,
          lista_hombres = if ("lista_hombres" %in% colnames(df)) sum(df$lista_hombres, na.rm = TRUE) else NA,
          lista_mujeres = if ("lista_mujeres" %in% colnames(df)) sum(df$lista_mujeres, na.rm = TRUE) else NA,
          stringsAsFactors = FALSE
        )
        
        lista_datos[[length(lista_datos) + 1]] <- totales
      }
    }
    
    if (length(lista_datos) == 0) {
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    message("✅ Datos del año ", input$year, " cargados: ", nrow(datos_completos), " registros")
    
    return(datos_completos)
  }) %>% 
    bindCache(input$tipo_corte, input$year, input$date, input$entidad, input$distrito, input$municipio, input$seccion)
 
  # ========== FUNCIÓN AUXILIAR: PROYECCIÓN CON TASA DE CRECIMIENTO ==========
  
  proyectar_con_tasa_crecimiento <- function(datos, meses_proyectar = 5) {
    if (is.null(datos) || nrow(datos) < 2) {
      return(NULL)
    }
    
    # Calcular tasa de crecimiento mensual promedio
    n <- nrow(datos)
    valor_inicial <- datos$lista_nominal[1]
    valor_final <- datos$lista_nominal[n]
    
    if (valor_inicial == 0 || is.na(valor_inicial) || is.na(valor_final)) {
      return(NULL)
    }
    
    tasa_mensual <- ((valor_final / valor_inicial) ^ (1 / (n - 1))) - 1
    
    # Crear fechas proyectadas
    ultima_fecha <- max(datos$fecha)
    fechas_proyectadas <- seq(ultima_fecha, by = "month", length.out = meses_proyectar + 1)[-1]
    
    # Proyectar valores
    proyecciones <- data.frame(
      fecha = fechas_proyectadas,
      lista_nominal = numeric(meses_proyectar),
      padron_electoral = numeric(meses_proyectar),
      stringsAsFactors = FALSE
    )
    
    ultimo_valor_lista <- datos$lista_nominal[n]
    ultimo_valor_padron <- datos$padron_electoral[n]
    
    for (i in 1:meses_proyectar) {
      proyecciones$lista_nominal[i] <- ultimo_valor_lista * ((1 + tasa_mensual) ^ i)
      proyecciones$padron_electoral[i] <- ultimo_valor_padron * ((1 + tasa_mensual) ^ i)
    }
    
    proyecciones$tipo <- "Proyección"
    
    message("✅ Proyección calculada: tasa mensual = ", round(tasa_mensual * 100, 4), "%")
    
    return(proyecciones)
  }
  
  # ========== GRÁFICAS PARA DATOS HISTÓRICOS ==========
  
  # ========== GRÁFICA 1: EVOLUCIÓN MENSUAL 2025 + PROYECCIÓN ==========
  output$grafico_evolucion_2025 <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos)) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Filtrar solo datos de 2025
    datos_2025 <- datos_completos[format(datos_completos$fecha, "%Y") == "2025", ]
    
    if (nrow(datos_2025) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos de 2025 disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Calcular meses restantes hasta diciembre
    ultimo_mes <- as.numeric(format(max(datos_2025$fecha), "%m"))
    meses_restantes <- 12 - ultimo_mes
    
    # Proyectar si hay meses pendientes
    proyeccion <- NULL
    if (meses_restantes > 0) {
      proyeccion <- proyectar_con_tasa_crecimiento(datos_2025, meses_restantes)
    }
    
    # Crear gráfico base con datos reales
    p <- plot_ly()
    
    # Línea Padrón Electoral (real)
    p <- p %>% add_trace(
      data = datos_2025,
      x = ~fecha,
      y = ~padron_electoral,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Electoral',
      line = list(color = '#44559B', width = 3),
      marker = list(size = 8, color = '#44559B'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Padrón: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Línea Lista Nominal (real)
    p <- p %>% add_trace(
      data = datos_2025,
      x = ~fecha,
      y = ~lista_nominal,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Nominal',
      line = list(color = '#C0311A', width = 3),
      marker = list(size = 8, color = '#C0311A'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Lista: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Agregar proyecciones si existen
    if (!is.null(proyeccion)) {
      # Proyección Padrón Electoral
      p <- p %>% add_trace(
        data = proyeccion,
        x = ~fecha,
        y = ~padron_electoral,
        type = 'scatter',
        mode = 'lines',
        name = 'Proyección Padrón',
        line = list(color = '#44559B', width = 2, dash = 'dash'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Proyección Padrón: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Proyección Lista Nominal
      p <- p %>% add_trace(
        data = proyeccion,
        x = ~fecha,
        y = ~lista_nominal,
        type = 'scatter',
        mode = 'lines',
        name = 'Proyección Lista',
        line = list(color = '#C0311A', width = 2, dash = 'dash'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Proyección Lista: %{y:,.0f}<extra></extra>'
        )
      )
    }
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = "Evolución Mensual 2025 - Padrón Electoral y Lista Nominal",
        font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "",
        type = 'date',
        tickformat = "%b"
      ),
      yaxis = list(
        title = "Número de Electores",
        separatethousands = TRUE
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.15
      ),
      margin = list(t = 80, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
          x = 0.0,
          y = -0.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "left"
        )
      )
    )
    
    message("✅ Gráfico 1: Evolución 2025 renderizado")
    return(p)
  })
  
  # ========== GRÁFICA 2: EVOLUCIÓN ANUAL (2017-2025) ==========
  output$grafico_evolucion_anual <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    # Cargar último mes de cada año (SOLO 9 archivos)
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    años <- 2017:2025
    
    lista_anuales <- list()
    
    for (año in años) {
      fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año]
      if (length(fechas_año) > 0) {
        ultima_fecha <- max(fechas_año)
        
        datos_temp <- tryCatch({
          cargar_lne(
            tipo_corte = "historico",
            fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
            dimension = "completo",
            estado = if (input$entidad == "Nacional") "Nacional" else input$entidad,
            incluir_extranjero = TRUE
          )
        }, error = function(e) NULL)
        
        if (!is.null(datos_temp) && !is.null(datos_temp$datos)) {
          df <- datos_temp$datos
          lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
            año = año,
            fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
            padron_electoral = sum(df$padron_electoral, na.rm = TRUE),
            lista_nominal = sum(df$lista_nominal, na.rm = TRUE)
          )
        }
      }
    }
    
    if (length(lista_anuales) == 0) {
      return(NULL)
    }
    
    datos_anuales <- do.call(rbind, lista_anuales)
    
    # Crear gráfico (mismo código de layout que antes)
    p <- plot_ly()
    
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~padron_electoral,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Electoral',
      line = list(color = '#44559B', width = 3),
      marker = list(size = 10, color = '#44559B'),
      hovertemplate = paste0('<b>%{x}</b><br>Padrón: %{y:,.0f}<extra></extra>')
    )
    
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~lista_nominal,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Nominal',
      line = list(color = '#C0311A', width = 3),
      marker = list(size = 10, color = '#C0311A'),
      hovertemplate = paste0('<b>%{x}</b><br>Lista: %{y:,.0f}<extra></extra>')
    )
    
    p <- p %>% layout(
      title = list(
        text = "Evolución Anual (2017-2025) - Padrón Electoral y Lista Nominal",
        font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis = list(title = "Año", type = 'category'),
      yaxis = list(title = "Número de Electores", separatethousands = TRUE),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
      margin = list(t = 80, b = 100, l = 80, r = 50),
      hovermode = 'x unified'
    )
    
    return(p)
  })
  
  # ========== GRÁFICA 3: EVOLUCIÓN ANUAL + DESGLOSE POR SEXO ==========
  output$grafico_evolucion_anual_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos)) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Verificar que existan columnas de sexo
    if (!all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(datos_completos))) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "Desglose por sexo no disponible",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Extraer último mes de cada año
    datos_completos$año <- format(datos_completos$fecha, "%Y")
    datos_completos$mes <- as.numeric(format(datos_completos$fecha, "%m"))
    
    datos_anuales <- datos_completos %>%
      group_by(año) %>%
      filter(mes == max(mes)) %>%
      slice(1) %>%
      ungroup() %>%
      as.data.frame()
    
    datos_anuales <- datos_anuales[order(datos_anuales$fecha), ]
    
    # Crear gráfico
    p <- plot_ly()
    
    # Padrón Hombres
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~padron_hombres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Hombres',
      line = list(color = '#4A90E2', width = 2.5),
      marker = list(size = 8, color = '#4A90E2'),
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Padrón H: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Padrón Mujeres
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~padron_mujeres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Mujeres',
      line = list(color = '#E24A90', width = 2.5),
      marker = list(size = 8, color = '#E24A90'),
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Padrón M: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Lista Hombres
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~lista_hombres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Hombres',
      line = list(color = '#2E5C8A', width = 2.5, dash = 'dot'),
      marker = list(size = 8, color = '#2E5C8A', symbol = 'square'),
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Lista H: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Lista Mujeres
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~lista_mujeres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Mujeres',
      line = list(color = '#A83565', width = 2.5, dash = 'dot'),
      marker = list(size = 8, color = '#A83565', symbol = 'square'),
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Lista M: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = "Evolución Anual por Sexo (2017-2025)",
        font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "Año",
        type = 'category'
      ),
      yaxis = list(
        title = "Número de Electores",
        separatethousands = TRUE
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.15
      ),
      margin = list(t = 80, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
          x = 0.0,
          y = -0.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "left"
        )
      )
    )
    
    message("✅ Gráfico 3: Evolución anual por sexo renderizado")
    return(p)
  })
  
  # ========== GRÁFICA 4: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO ==========
  output$grafico_evolucion_year <- renderPlotly({
    req(input$tipo_corte == "historico", input$year)
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos)) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Filtrar datos del año seleccionado
    datos_year <- datos_completos[format(datos_completos$fecha, "%Y") == input$year, ]
    
    if (nrow(datos_year) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = paste0("No hay datos para el año ", input$year),
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Crear gráfico
    p <- plot_ly()
    
    # Línea Padrón Electoral
    p <- p %>% add_trace(
      data = datos_year,
      x = ~fecha,
      y = ~padron_electoral,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Electoral',
      line = list(color = '#44559B', width = 3),
      marker = list(size = 8, color = '#44559B'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Padrón: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Línea Lista Nominal
    p <- p %>% add_trace(
      data = datos_year,
      x = ~fecha,
      y = ~lista_nominal,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Nominal',
      line = list(color = '#C0311A', width = 3),
      marker = list(size = 8, color = '#C0311A'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Lista: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Mensual ", input$year, " - Padrón Electoral y Lista Nominal"),
        font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "",
        type = 'date',
        tickformat = "%b"
      ),
      yaxis = list(
        title = "Número de Electores",
        separatethousands = TRUE
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.15
      ),
      margin = list(t = 80, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
          x = 0.0,
          y = -0.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "left"
        )
      )
    )
    
    message("✅ Gráfico 4: Evolución mensual del año ", input$year, " renderizado")
    return(p)
  })
  
  # ========== GRÁFICA 5: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO + SEXO ==========
  output$grafico_evolucion_year_sexo <- renderPlotly({
    req(input$tipo_corte == "historico", input$year)
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos)) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "No hay datos disponibles",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Verificar columnas de sexo
    if (!all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(datos_completos))) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "Desglose por sexo no disponible",
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Filtrar datos del año seleccionado
    datos_year <- datos_completos[format(datos_completos$fecha, "%Y") == input$year, ]
    
    if (nrow(datos_year) == 0) {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = paste0("No hay datos para el año ", input$year),
                     xref = "paper", yref = "paper",
                     x = 0.5, y = 0.5,
                     showarrow = FALSE,
                     font = list(size = 14, color = "#666")
                   )
                 )
               ))
    }
    
    # Crear gráfico
    p <- plot_ly()
    
    # Padrón Hombres
    p <- p %>% add_trace(
      data = datos_year,
      x = ~fecha,
      y = ~padron_hombres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Hombres',
      line = list(color = '#4A90E2', width = 2.5),
      marker = list(size = 8, color = '#4A90E2'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Padrón H: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Padrón Mujeres
    p <- p %>% add_trace(
      data = datos_year,
      x = ~fecha,
      y = ~padron_mujeres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Mujeres',
      line = list(color = '#E24A90', width = 2.5),
      marker = list(size = 8, color = '#E24A90'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Padrón M: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Lista Hombres
    p <- p %>% add_trace(
      data = datos_year,
      x = ~fecha,
      y = ~lista_hombres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Hombres',
      line = list(color = '#2E5C8A', width = 2.5, dash = 'dot'),
      marker = list(size = 8, color = '#2E5C8A', symbol = 'square'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Lista H: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Lista Mujeres
    p <- p %>% add_trace(
      data = datos_year,
      x = ~fecha,
      y = ~lista_mujeres,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Mujeres',
      line = list(color = '#A83565', width = 2.5, dash = 'dot'),
      marker = list(size = 8, color = '#A83565', symbol = 'square'),
      hovertemplate = paste0(
        '<b>%{x|%B %Y}</b><br>',
        'Lista M: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Mensual ", input$year, " por Sexo"),
        font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(
        title = "",
        type = 'date',
        tickformat = "%b"
      ),
      yaxis = list(
        title = "Número de Electores",
        separatethousands = TRUE
      ),
      legend = list(
        orientation = "h",
        xanchor = "center",
        x = 0.5,
        y = -0.15
      ),
      margin = list(t = 80, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
          x = 0.0,
          y = -0.25,
          xref = "paper",
          yref = "paper",
          xanchor = "left",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "left"
        )
      )
    )
    
    message("✅ Gráfico 5: Evolución mensual ", input$year, " por sexo renderizado")
    return(p)
  })
  
  # ========== GRÁFICAS PARA DATOS SEMANALES ==========
  
  output$`main-plot_container` <- renderUI({
    plotlyOutput(session$ns("main-grafico_barras"), width = "100%", height = "450px")
  })
  
  # ========== GRÁFICO PRINCIPAL SEMANALES (BARRAS) ==========
  output$`main-grafico_barras` <- renderPlotly({
    req(input$tipo_corte == "semanal")
    req(combinacion_valida())
    datos <- datos_columnas()
    
    if (is.null(datos) || is.null(datos$datos) || nrow(datos$datos) == 0) {
      p <- plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "No hay datos disponibles con los filtros seleccionados",
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
              xanchor = "center", yanchor = "middle",
              showarrow = FALSE,
              font = list(size = 16, color = "#666")
            )
          )
        )
      return(p)
    }
    
    df <- datos$datos
    desglose_actual <- input$desglose %||% "Sexo"
    
    message("📊 Renderizando gráfico semanal: ", desglose_actual)
    
    # ========== GRÁFICA POR SEXO ==========
    if (desglose_actual == "Sexo") {
      
      cols_sexo <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
      
      if (all(cols_sexo %in% colnames(df))) {
        padron_h <- sum(df$padron_hombres, na.rm = TRUE)
        padron_m <- sum(df$padron_mujeres, na.rm = TRUE)
        lista_h <- sum(df$lista_hombres, na.rm = TRUE)
        lista_m <- sum(df$lista_mujeres, na.rm = TRUE)
        
        message("📊 Padrón H: ", format(padron_h, big.mark = ","), " | M: ", format(padron_m, big.mark = ","))
        message("📊 Lista H: ", format(lista_h, big.mark = ","), " | M: ", format(lista_m, big.mark = ","))
        
        datos_grafico <- data.frame(
          Categoria = rep(c("Hombres", "Mujeres"), 2),
          Tipo = rep(c("Padrón Electoral", "Lista Nominal"), each = 2),
          Cantidad = c(padron_h, padron_m, lista_h, lista_m),
          stringsAsFactors = FALSE
        )
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Categoria,
          y = ~Cantidad,
          color = ~Tipo,
          type = 'bar',
          colors = c("#44559B", "#C0311A"),
          text = ~paste0(format(Cantidad, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = "Padrón Electoral y Lista Nominal por Sexo",
              font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
              x = 0.5, xanchor = "center"
            ),
            xaxis = list(title = ""),
            yaxis = list(title = "Número de Electores", separatethousands = TRUE),
            barmode = 'group',
            margin = list(t = 80, b = 80, l = 80, r = 50),
            legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
            annotations = list(
              list(
                text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
                x = 0.0, y = -0.25,
                xref = "paper", yref = "paper",
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
        
      } else {
        # SIN DESGLOSE - MOSTRAR TOTALES
        if (all(c("padron_electoral", "lista_nominal") %in% colnames(df))) {
          
          total_padron <- sum(df$padron_electoral, na.rm = TRUE)
          total_lista <- sum(df$lista_nominal, na.rm = TRUE)
          
          datos_grafico <- data.frame(
            Tipo = c("Padrón Electoral", "Lista Nominal"),
            Cantidad = c(total_padron, total_lista),
            stringsAsFactors = FALSE
          )
          
          p <- plot_ly(
            data = datos_grafico,
            x = ~Tipo,
            y = ~Cantidad,
            type = 'bar',
            marker = list(color = c("#44559B", "#C0311A")),
            text = ~paste0(format(Cantidad, big.mark = ","), " electores"),
            hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
          ) %>%
            layout(
              title = list(
                text = "Padrón Electoral y Lista Nominal",
                font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
                x = 0.5, xanchor = "center"
              ),
              xaxis = list(title = ""),
              yaxis = list(title = "Número de Electores", separatethousands = TRUE),
              margin = list(t = 80, b = 80, l = 80, r = 50),
              annotations = list(
                list(
                  text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
                  x = 0.0, y = -0.15,
                  xref = "paper", yref = "paper",
                  xanchor = "left", yanchor = "top",
                  showarrow = FALSE,
                  font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                  align = "left"
                )
              )
            )
          
        } else {
          p <- plot_ly() %>%
            layout(
              xaxis = list(visible = FALSE),
              yaxis = list(visible = FALSE),
              annotations = list(
                list(
                  text = "Datos no disponibles para este corte",
                  xref = "paper", yref = "paper",
                  x = 0.5, y = 0.5,
                  showarrow = FALSE,
                  font = list(size = 14, color = "#666")
                )
              )
            )
        }
      }
      
    } else if (desglose_actual == "Rango de Edad") {
      
      # ========== GRÁFICA POR EDAD ==========
      
      cols_edad_lista <- grep("^lista_(\\d+|\\d+_\\d+)", colnames(df), value = TRUE, ignore.case = TRUE)
      
      if (length(cols_edad_lista) > 0) {
        
        grupos_raw <- gsub("lista_", "", cols_edad_lista, ignore.case = TRUE)
        grupos_raw <- gsub("_(hombres|mujeres|nobinario).*", "", grupos_raw, ignore.case = TRUE)
        grupos <- unique(grupos_raw)
        
        datos_grafico <- data.frame(
          Grupo = character(),
          Lista_Nominal = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (grupo in grupos) {
          cols_grupo <- grep(paste0("^lista_", grupo, "($|_)"), colnames(df), value = TRUE, ignore.case = TRUE)
          total <- sum(df[, cols_grupo, drop = FALSE], na.rm = TRUE)
          nombre_grupo <- gsub("_", "-", grupo)
          nombre_grupo <- gsub("y-mas", "y más", nombre_grupo, ignore.case = TRUE)
          
          datos_grafico <- rbind(
            datos_grafico, 
            data.frame(
              Grupo = nombre_grupo,
              Lista_Nominal = total,
              stringsAsFactors = FALSE
            )
          )
        }
        
        orden_edad <- c("18", "19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                        "45-49", "50-54", "55-59", "60-64", "65-y-más", "65-y-mas")
        datos_grafico$Grupo <- factor(
          datos_grafico$Grupo, 
          levels = intersect(orden_edad, datos_grafico$Grupo)
        )
        datos_grafico <- datos_grafico[order(datos_grafico$Grupo), ]
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Grupo,
          y = ~Lista_Nominal,
          type = 'bar',
          marker = list(color = "#C0311A"),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = "Lista Nominal por Grupo de Edad",
              font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
              x = 0.5,
              xanchor = "center"
            ),
            xaxis = list(title = "Grupo de Edad"),
            yaxis = list(
              title = "Número de Electores",
              separatethousands = TRUE
            ),
            margin = list(t = 80, b = 100, l = 80, r = 50),
            annotations = list(
              list(
                text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
                x = 0.0,
                y = -0.20,
                xref = "paper",
                yref = "paper",
                xanchor = "left",
                yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
        
      } else {
        p <- plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Datos de edad no disponibles para este corte",
                xref = "paper", 
                yref = "paper",
                x = 0.5, 
                y = 0.5,
                showarrow = FALSE,
                font = list(size = 14, color = "#666")
              )
            )
          )
      }
      
    } else if (desglose_actual == "Entidad de Origen") {
      
      # ========== GRÁFICA POR ENTIDAD DE ORIGEN ==========
      
      if ("nombre_entidad" %in% colnames(df) && "lista_nominal" %in% colnames(df)) {
        
        datos_grafico <- df %>%
          group_by(Entidad = nombre_entidad) %>%
          summarise(
            Lista_Nominal = sum(lista_nominal, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(Lista_Nominal)) %>%
          head(10)
        
        datos_grafico <- as.data.frame(datos_grafico)
        
        p <- plot_ly(
          data = datos_grafico,
          y = ~reorder(Entidad, Lista_Nominal),
          x = ~Lista_Nominal,
          type = 'bar',
          orientation = 'h',
          marker = list(color = "#44559B"),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '<b>%{y}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = "Top 10 Entidades por Lista Nominal",
              font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
              x = 0.5,
              xanchor = "center"
            ),
            xaxis = list(
              title = "Número de Electores",
              separatethousands = TRUE
            ),
            yaxis = list(title = ""),
            margin = list(t = 80, b = 80, l = 180, r = 50),
            annotations = list(
              list(
                text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
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
            )
          )
        
      } else {
        p <- plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Datos de origen no disponibles para este corte",
                xref = "paper", 
                yref = "paper",
                x = 0.5, 
                y = 0.5,
                showarrow = FALSE,
                font = list(size = 14, color = "#666")
              )
            )
          )
      }
      
    } else {
      p <- plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "Tipo de desglose no reconocido",
              xref = "paper", 
              yref = "paper",
              x = 0.5, 
              y = 0.5,
              showarrow = FALSE,
              font = list(size = 14, color = "#666")
            )
          )
        )
    }
    
    message("✅ Gráfico semanal renderizado: ", desglose_actual)
    return(p)
  })
  
  # ========== GRÁFICO DE TASA DE INCLUSIÓN (SOLO SEMANALES) ==========
  output$`main-tasa_inclusion_plot` <- renderPlotly({
    req(input$tipo_corte == "semanal")
    req(combinacion_valida())
    datos <- datos_columnas()
    
    if (is.null(datos) || is.null(datos$datos) || nrow(datos$datos) == 0) {
      return(NULL)
    }
    
    df <- datos$datos
    
    if (!all(c("padron_electoral", "lista_nominal") %in% colnames(df))) {
      return(NULL)
    }
    
    total_padron <- sum(df$padron_electoral, na.rm = TRUE)
    total_lista <- sum(df$lista_nominal, na.rm = TRUE)
    
    if (total_padron == 0) {
      return(NULL)
    }
    
    tasa_inclusion <- round((total_lista / total_padron) * 100, 2)
    tasa_exclusion <- round(100 - tasa_inclusion, 2)
    
    datos_grafico <- data.frame(
      grupo = c(
        paste0("Lista Nominal:<br>", sprintf("%.2f%%", tasa_inclusion)),
        sprintf("Diferencia: %.2f%%", tasa_exclusion)
      ),
      valor = c(tasa_inclusion, tasa_exclusion),
      stringsAsFactors = FALSE
    )
    
    color_lista <- "#4CAF50"
    color_diferencia <- "#FFC107"
    
    p <- plot_ly(
      data = datos_grafico,
      values = ~valor,
      labels = ~grupo,
      type = "pie",
      hole = 0.6,
      textinfo = "label",
      textposition = "outside",
      textfont = list(
        color = c(color_lista, color_diferencia),
        size = 14
      ),
      marker = list(colors = c(color_lista, color_diferencia)),
      showlegend = FALSE,
      hoverinfo = "none"
    ) %>%
      layout(
        title = list(
          text = "Tasa de Inclusión en Lista Nominal",
          x = 0.5,
          xanchor = "center",
          y = 0.95,
          yanchor = "top",
          font = list(size = 20, color = "black", family = "Arial, sans-serif")
        ),
        annotations = list(
          list(
            text = paste0("Padrón Total: ", format(total_padron, big.mark = ",")),
            x = 0.5,
            xref = "paper",
            y = 1.15,
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 16, color = "black", family = "Arial, sans-serif")
          ),
          list(
            text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
            xref = "paper", yref = "paper",
            x = 0.0, y = -0.20,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            showarrow = FALSE,
            align = "left"
          )
        ),
        margin = list(t = 100, b = 100, l = 50, r = 50),
        showlegend = FALSE
      )
    
    message("✅ Gráfico de tasa de inclusión renderizado")
    return(p)
  })
  
  message("✅ Módulo lista_nominal_server_graficas inicializado")
}
  