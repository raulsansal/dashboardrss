# modules/lista_nominal_server_graficas.R
# Módulo especializado en la generación de gráficas para Lista Nominal Electoral

# ========== FUNCIÓN AUXILIAR: GENERAR TEXTO DE ALCANCE ==========

generar_texto_alcance <- function(input) {
  if (input$entidad == "Nacional") {
    return("Ámbito: Nacional")
  }
  
  alcance_partes <- c(paste("Estado:", input$entidad))
  
  if (!is.null(input$distrito) && input$distrito != "Todos") {
    alcance_partes <- c(alcance_partes, paste("Distrito:", input$distrito))
  }
  
  if (!is.null(input$municipio) && input$municipio != "Todos") {
    alcance_partes <- c(alcance_partes, paste("Municipio:", input$municipio))
  }
  
  if (!is.null(input$seccion) && length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
    if (length(input$seccion) == 1) {
      alcance_partes <- c(alcance_partes, paste("Sección:", input$seccion))
    } else if (length(input$seccion) <= 5) {
      secciones_texto <- paste(input$seccion, collapse = ", ")
      alcance_partes <- c(alcance_partes, paste("Secciones:", secciones_texto))
    } else {
      alcance_partes <- c(alcance_partes, paste("Secciones:", length(input$seccion), "seleccionadas"))
    }
  }
  
  return(paste(alcance_partes, collapse = " - "))
}

lista_nominal_server_graficas <- function(input, output, session, datos_columnas, combinacion_valida) {
  
  message("🚀 Iniciando módulo lista_nominal_server_graficas")
  
  # ========== REACTIVE: OBTENER AÑO ACTUAL ==========
  
  anio_actual <- reactive({
    as.integer(format(Sys.Date(), "%Y"))
  })
  
  # ========== REACTIVE: CARGAR DATOS HISTÓRICOS DEL AÑO SELECCIONADO ==========
  
  datos_historicos_year <- reactive({
    # CARGA INICIAL: Cargar datos del año actual
    if (input$btn_consultar == 0) {
      message("🚀 [datos_historicos_year] CARGA INICIAL - Año actual: ", anio_actual())
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      # Filtrar fechas del año actual
      fechas_anio_actual <- catalog$historico[format(catalog$historico, "%Y") == anio_actual()]
      
      if (length(fechas_anio_actual) == 0) {
        message("⚠️ [datos_historicos_year] Sin fechas para año actual")
        return(NULL)
      }
      
      message("📥 [datos_historicos_year] Cargando ", length(fechas_anio_actual), " fechas del año ", anio_actual())
      
      lista_datos <- list()
      
      for (i in seq_along(fechas_anio_actual)) {
        fecha <- fechas_anio_actual[i]
        
        datos_temp <- tryCatch({
          cargar_lne(
            tipo_corte = "historico",
            fecha = as.Date(fecha, origin = "1970-01-01"),
            dimension = "completo",
            estado = "Nacional",
            distrito = "Todos",
            municipio = "Todos",
            seccion = "Todas",
            incluir_extranjero = TRUE
          )
        }, error = function(e) {
          message("⚠️ Error cargando fecha ", fecha, ": ", e$message)
          return(NULL)
        })
        
        # ========== USAR FILA DE TOTALES (NO SUMAR DATAFRAME) ==========
        if (!is.null(datos_temp) && !is.null(datos_temp$totales)) {
          totales_fila <- datos_temp$totales
          
          # CRÍTICO: Limpiar comas y convertir a numérico
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
          
          # Valores por sexo (si existen)
          padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
          } else NA
          
          # Validar que no sean NA
          if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
            registro <- data.frame(
              fecha = as.Date(fecha, origin = "1970-01-01"),
              padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
              lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
              padron_hombres = padron_hombres,
              padron_mujeres = padron_mujeres,
              lista_hombres = lista_hombres,
              lista_mujeres = lista_mujeres,
              stringsAsFactors = FALSE
            )
            
            message("   ✅ ", format(fecha, "%Y-%m-%d"), 
                    " | Padrón: ", format(registro$padron_electoral, big.mark = ","),
                    " | Lista: ", format(registro$lista_nominal, big.mark = ","))
            
            lista_datos[[length(lista_datos) + 1]] <- registro
          } else {
            message("   ⚠️ ", format(fecha, "%Y-%m-%d"), " - Valores NA en totales")
          }
        } else {
          message("   ⚠️ ", format(fecha, "%Y-%m-%d"), " - Sin fila de totales")
        }
      }
      
      if (length(lista_datos) == 0) {
        return(NULL)
      }
      
      datos_completos <- do.call(rbind, lista_datos)
      datos_completos <- datos_completos[order(datos_completos$fecha), ]
      
      message("✅ [datos_historicos_year] CARGA INICIAL: ", nrow(datos_completos), " registros del año ", anio_actual())
      return(datos_completos)
    }
    
    # CARGA PERSONALIZADA: depende del botón
    req(input$btn_consultar > 0)
    req(input$tipo_corte == "historico")
    req(input$year)
    
    # Aislar inputs
    year <- isolate(input$year)
    entidad <- isolate(input$entidad)
    distrito <- isolate(input$distrito %||% "Todos")
    municipio <- isolate(input$municipio %||% "Todos")
    seccion <- isolate(input$seccion %||% "Todas")
    
    message("🔄 [datos_historicos_year] CONSULTA PERSONALIZADA - Año ", year, ", Entidad: ", entidad)
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    
    # Cargar fechas del año seleccionado
    fechas_year <- catalog$historico[format(catalog$historico, "%Y") == year]
    
    if (length(fechas_year) == 0) {
      return(NULL)
    }
    
    estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
    
    message("📥 Cargando ", length(fechas_year), " fechas del año ", year, "...")
    
    lista_datos <- list()
    
    for (i in seq_along(fechas_year)) {
      fecha <- fechas_year[i]
      
      datos_temp <- tryCatch({
        cargar_lne(
          tipo_corte = "historico",
          fecha = as.Date(fecha, origin = "1970-01-01"),
          dimension = "completo",
          estado = estado_filtro,
          distrito = distrito,
          municipio = municipio,
          seccion = seccion,
          incluir_extranjero = TRUE
        )
      }, error = function(e) {
        message("⚠️ Error cargando fecha ", fecha, ": ", e$message)
        return(NULL)
      })
      
      # Usar fila de totales si está en Nacional, sino sumar dataframe
      if (!is.null(datos_temp)) {
        if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
          # Usar totales
          totales_fila <- datos_temp$totales
          
          padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
          padron_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
          lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
          lista_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
          
          padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
          } else NA
          
          padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
          } else NA
          
          lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
          } else NA
          
          lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
            as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
          } else NA
          
          if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
            registro <- data.frame(
              fecha = as.Date(fecha, origin = "1970-01-01"),
              padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
              lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
              padron_hombres = padron_hombres,
              padron_mujeres = padron_mujeres,
              lista_hombres = lista_hombres,
              lista_mujeres = lista_mujeres,
              stringsAsFactors = FALSE
            )
            
            lista_datos[[length(lista_datos) + 1]] <- registro
          }
        } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
          # Sumar dataframe (para filtros específicos)
          df <- datos_temp$datos
          
          registro <- data.frame(
            fecha = as.Date(fecha, origin = "1970-01-01"),
            padron_electoral = sum(df$padron_nacional, na.rm = TRUE) + sum(df$padron_extranjero, na.rm = TRUE),
            lista_nominal = sum(df$lista_nacional, na.rm = TRUE) + sum(df$lista_extranjero, na.rm = TRUE),
            padron_hombres = if ("padron_nacional_hombres" %in% colnames(df)) sum(df$padron_nacional_hombres, na.rm = TRUE) else NA,
            padron_mujeres = if ("padron_nacional_mujeres" %in% colnames(df)) sum(df$padron_nacional_mujeres, na.rm = TRUE) else NA,
            lista_hombres = if ("lista_nacional_hombres" %in% colnames(df)) sum(df$lista_nacional_hombres, na.rm = TRUE) else NA,
            lista_mujeres = if ("lista_nacional_mujeres" %in% colnames(df)) sum(df$lista_nacional_mujeres, na.rm = TRUE) else NA,
            stringsAsFactors = FALSE
          )
          
          lista_datos[[length(lista_datos) + 1]] <- registro
        }
      }
    }
    
    if (length(lista_datos) == 0) {
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_datos)
    datos_completos <- datos_completos[order(datos_completos$fecha), ]
    
    message("✅ Datos del año ", year, " cargados: ", nrow(datos_completos), " registros")
    
    return(datos_completos)
  }) %>% 
    bindCache(input$btn_consultar, input$tipo_corte, input$year, input$entidad, 
              input$distrito, input$municipio, input$seccion)
  
  # ========== REACTIVE: DATOS ANUALES (2017-HASTA AÑO ACTUAL) ==========
  
  datos_anuales_completos <- reactive({
    # CARGA INICIAL: Cargar evolución anual completa hasta año actual
    if (input$btn_consultar == 0) {
      message("🚀 [datos_anuales_completos] CARGA INICIAL - Evolución 2017 hasta ", anio_actual())
      
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
        return(NULL)
      }
      
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      años <- 2017:anio_actual()
      
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
              estado = "Nacional",
              distrito = "Todos",
              municipio = "Todos",
              seccion = "Todas",
              incluir_extranjero = TRUE
            )
          }, error = function(e) NULL)
          
          # Usar fila de totales
          if (!is.null(datos_temp) && !is.null(datos_temp$totales)) {
            totales_fila <- datos_temp$totales
            
            padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
            padron_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
            lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
            lista_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
            
            padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
            } else NA
            
            padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
            } else NA
            
            lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
            } else NA
            
            lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
            } else NA
            
            if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
              lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
                año = as.character(año),
                fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
                padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
                lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
                padron_hombres = padron_hombres,
                padron_mujeres = padron_mujeres,
                lista_hombres = lista_hombres,
                lista_mujeres = lista_mujeres,
                stringsAsFactors = FALSE
              )
            }
          }
        }
      }
      
      if (length(lista_anuales) == 0) {
        return(NULL)
      }
      
      datos_completos <- do.call(rbind, lista_anuales)
      
      message("✅ [datos_anuales_completos] CARGA INICIAL: ", nrow(datos_completos), " años cargados")
      
      return(datos_completos)
    }
    
    # CARGA PERSONALIZADA: Solo cuando usuario presiona botón
    req(input$btn_consultar > 0)
    req(input$tipo_corte == "historico")
    
    # Aislar inputs
    entidad <- isolate(input$entidad)
    distrito <- isolate(input$distrito %||% "Todos")
    municipio <- isolate(input$municipio %||% "Todos")
    seccion <- isolate(input$seccion %||% "Todas")
    
    message("🔄 [datos_anuales_completos] CONSULTA PERSONALIZADA - Entidad: ", entidad)
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) {
      return(NULL)
    }
    
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    años <- 2017:anio_actual()
    
    lista_anuales <- list()
    
    estado_filtro <- if (entidad == "Nacional") "Nacional" else entidad
    
    for (año in años) {
      fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año]
      if (length(fechas_año) > 0) {
        ultima_fecha <- max(fechas_año)
        
        datos_temp <- tryCatch({
          cargar_lne(
            tipo_corte = "historico",
            fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
            dimension = "completo",
            estado = estado_filtro,
            distrito = distrito,
            municipio = municipio,
            seccion = seccion,
            incluir_extranjero = TRUE
          )
        }, error = function(e) NULL)
        
        if (!is.null(datos_temp)) {
          if (estado_filtro == "Nacional" && !is.null(datos_temp$totales)) {
            # Usar totales
            totales_fila <- datos_temp$totales
            
            padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
            padron_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
            lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
            lista_extranjero <- as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
            
            padron_hombres <- if ("padron_nacional_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_hombres)))
            } else NA
            
            padron_mujeres <- if ("padron_nacional_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional_mujeres)))
            } else NA
            
            lista_hombres <- if ("lista_nacional_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_hombres)))
            } else NA
            
            lista_mujeres <- if ("lista_nacional_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional_mujeres)))
            } else NA
            
            if (!is.na(padron_nacional) && !is.na(lista_nacional)) {
              lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
                año = as.character(año),
                fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
                padron_electoral = padron_nacional + ifelse(is.na(padron_extranjero), 0, padron_extranjero),
                lista_nominal = lista_nacional + ifelse(is.na(lista_extranjero), 0, lista_extranjero),
                padron_hombres = padron_hombres,
                padron_mujeres = padron_mujeres,
                lista_hombres = lista_hombres,
                lista_mujeres = lista_mujeres,
                stringsAsFactors = FALSE
              )
            }
          } else if (!is.null(datos_temp$datos) && nrow(datos_temp$datos) > 0) {
            # Sumar dataframe
            df <- datos_temp$datos
            
            lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
              año = as.character(año),
              fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
              padron_electoral = sum(df$padron_nacional, na.rm = TRUE) + sum(df$padron_extranjero, na.rm = TRUE),
              lista_nominal = sum(df$lista_nacional, na.rm = TRUE) + sum(df$lista_extranjero, na.rm = TRUE),
              padron_hombres = if ("padron_nacional_hombres" %in% colnames(df)) sum(df$padron_nacional_hombres, na.rm = TRUE) else NA,
              padron_mujeres = if ("padron_nacional_mujeres" %in% colnames(df)) sum(df$padron_nacional_mujeres, na.rm = TRUE) else NA,
              lista_hombres = if ("lista_nacional_hombres" %in% colnames(df)) sum(df$lista_nacional_hombres, na.rm = TRUE) else NA,
              lista_mujeres = if ("lista_nacional_mujeres" %in% colnames(df)) sum(df$lista_nacional_mujeres, na.rm = TRUE) else NA,
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    
    if (length(lista_anuales) == 0) {
      return(NULL)
    }
    
    datos_completos <- do.call(rbind, lista_anuales)
    
    message("✅ Datos anuales cargados: ", nrow(datos_completos), " años")
    
    return(datos_completos)
  }) %>% 
    bindCache(input$btn_consultar, input$tipo_corte, input$entidad, 
              input$distrito, input$municipio, input$seccion)
  
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
  
  # ========== GRÁFICA 1: EVOLUCIÓN MENSUAL AÑO ACTUAL/SELECCIONADO + PROYECCIÓN ==========
  output$grafico_evolucion_2025 <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos) || nrow(datos_completos) == 0) {
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
    
    # Obtener año de los datos (puede ser año actual o año seleccionado)
    year_datos <- format(datos_completos$fecha[1], "%Y")
    
    # Calcular meses restantes hasta diciembre
    ultimo_mes <- as.numeric(format(max(datos_completos$fecha), "%m"))
    meses_restantes <- 12 - ultimo_mes
    
    # Proyectar si hay meses pendientes
    proyeccion <- NULL
    if (meses_restantes > 0) {
      proyeccion <- proyectar_con_tasa_crecimiento(datos_completos, meses_restantes)
    }
    
    # Crear gráfico base con datos reales
    p <- plot_ly()
    
    # Línea Padrón Electoral (real)
    p <- p %>% add_trace(
      data = datos_completos,
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
      data = datos_completos,
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
    
    # Layout con alcance agregado
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Mensual ", year_datos, " - Padrón Electoral y Lista Nominal"),
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
      margin = list(t = 120, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = generar_texto_alcance(input),
          x = 0.5,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
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
    
    message("✅ Gráfico 1: Evolución ", year_datos, " renderizado")
    return(p)
  })
  
  # ========== GRÁFICA 2: EVOLUCIÓN ANUAL (2017-AÑO ACTUAL) ==========
  output$grafico_evolucion_anual <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    datos_anuales <- datos_anuales_completos()
    
    if (is.null(datos_anuales) || nrow(datos_anuales) == 0) {
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
    
    # Crear gráfico
    p <- plot_ly()
    
    # Línea Padrón Electoral
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~padron_electoral,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Padrón Electoral',
      line = list(color = '#44559B', width = 3),
      marker = list(size = 10, color = '#44559B'),
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Padrón: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Línea Lista Nominal
    p <- p %>% add_trace(
      data = datos_anuales,
      x = ~año,
      y = ~lista_nominal,
      type = 'scatter',
      mode = 'lines+markers',
      name = 'Lista Nominal',
      line = list(color = '#C0311A', width = 3),
      marker = list(size = 10, color = '#C0311A'),
      hovertemplate = paste0(
        '<b>%{x}</b><br>',
        'Lista: %{y:,.0f}<extra></extra>'
      )
    )
    
    # Layout con alcance
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Anual (2017-", anio_actual(), ") - Padrón Electoral y Lista Nominal"),
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
      margin = list(t = 120, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = generar_texto_alcance(input),
          x = 0.5,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
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
    
    message("✅ Gráfico 2: Evolución anual renderizado")
    return(p)
  })
  
  # ========== GRÁFICA 3: EVOLUCIÓN ANUAL + DESGLOSE POR SEXO ==========
  output$grafico_evolucion_anual_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    datos_anuales <- datos_anuales_completos()
    
    if (is.null(datos_anuales) || nrow(datos_anuales) == 0) {
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
    if (!all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(datos_anuales))) {
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
    
    # Crear gráfico
    p <- plot_ly()
    
    # Padrón Hombres
    p <- p %>% add_trace(data = datos_anuales,
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
    
    # Layout con alcance
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Anual por Sexo (2017-", anio_actual(), ")"),
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
      margin = list(t = 120, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = generar_texto_alcance(input),
          x = 0.5,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
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
    req(input$tipo_corte == "historico")
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos) || nrow(datos_completos) == 0) {
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
    
    # Obtener año de los datos
    year_datos <- format(datos_completos$fecha[1], "%Y")
    
    # Crear gráfico
    p <- plot_ly()
    
    # Línea Padrón Electoral
    p <- p %>% add_trace(
      data = datos_completos,
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
      data = datos_completos,
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
    
    # Layout con alcance
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Mensual ", year_datos, " - Padrón Electoral y Lista Nominal"),
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
      margin = list(t = 120, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = generar_texto_alcance(input),
          x = 0.5,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
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
    
    message("✅ Gráfico 4: Evolución mensual del año ", year_datos, " renderizado")
    return(p)
  })
  
  # ========== GRÁFICA 5: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO + SEXO ==========
  output$grafico_evolucion_year_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    
    datos_completos <- datos_historicos_year()
    
    if (is.null(datos_completos) || nrow(datos_completos) == 0) {
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
    
    # Obtener año de los datos
    year_datos <- format(datos_completos$fecha[1], "%Y")
    
    # Crear gráfico
    p <- plot_ly()
    
    # Padrón Hombres
    p <- p %>% add_trace(
      data = datos_completos,
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
      data = datos_completos,
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
      data = datos_completos,
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
      data = datos_completos,
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
    
    # Layout con alcance
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Mensual ", year_datos, " por Sexo"),
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
      margin = list(t = 120, b = 100, l = 80, r = 50),
      hovermode = 'x unified',
      annotations = list(
        list(
          text = generar_texto_alcance(input),
          x = 0.5,
          y = 1.12,
          xref = "paper",
          yref = "paper",
          xanchor = "center",
          yanchor = "top",
          showarrow = FALSE,
          font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
          align = "center"
        ),
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
    
    message("✅ Gráfico 5: Evolución mensual ", year_datos, " por sexo renderizado")
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
    desglose_actual <- isolate(input$desglose) %||% "Sexo"
    
    message("📊 Renderizando gráfico semanal: ", desglose_actual)
    
    # ========== GRÁFICA POR SEXO ==========
    if (desglose_actual == "Sexo") {
      
      cols_sexo <- c("padron_nacional_hombres", "padron_nacional_mujeres", 
                     "lista_nacional_hombres", "lista_nacional_mujeres")
      
      if (all(cols_sexo %in% colnames(df))) {
        padron_h <- sum(df$padron_nacional_hombres, na.rm = TRUE)
        padron_m <- sum(df$padron_nacional_mujeres, na.rm = TRUE)
        lista_h <- sum(df$lista_nacional_hombres, na.rm = TRUE)
        lista_m <- sum(df$lista_nacional_mujeres, na.rm = TRUE)
        
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
            margin = list(t = 120, b = 100, l = 80, r = 50),
            legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.15),
            annotations = list(
              list(
                text = generar_texto_alcance(input),
                x = 0.5, y = 1.12,
                xref = "paper", yref = "paper",
                xanchor = "center", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
                align = "center"
              ),
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
        total_padron <- sum(df$padron_nacional, na.rm = TRUE)
        total_lista <- sum(df$lista_nacional, na.rm = TRUE)
        
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
            margin = list(t = 120, b = 100, l = 80, r = 50),
            annotations = list(
              list(
                text = generar_texto_alcance(input),
                x = 0.5, y = 1.12,
                xref = "paper", yref = "paper",
                xanchor = "center", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
                align = "center"
              ),
              list(
                text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
                x = 0.0, y = -0.20,
                xref = "paper", yref = "paper",
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
                align = "left"
              )
            )
          )
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
            margin = list(t = 120, b = 100, l = 80, r = 50),
            annotations = list(
              list(
                text = generar_texto_alcance(input),
                x = 0.5, y = 1.12,
                xref = "paper", yref = "paper",
                xanchor = "center", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
                align = "center"
              ),
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
        p <- plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Datos de edad no disponibles para este corte",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
                showarrow = FALSE,
                font = list(size = 14, color = "#666")
              )
            )
          )
      }
      
    } else if (desglose_actual == "Entidad de Origen") {
      
      # ========== GRÁFICA POR ENTIDAD DE ORIGEN ==========
      
      if ("nombre_entidad" %in% colnames(df) && "lista_nacional" %in% colnames(df)) {
        
        datos_grafico <- df %>%
          group_by(Entidad = nombre_entidad) %>%
          summarise(
            Lista_Nominal = sum(lista_nacional, na.rm = TRUE),
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
            margin = list(t = 120, b = 100, l = 180, r = 50),
            annotations = list(
              list(
                text = generar_texto_alcance(input),
                x = 0.5, y = 1.12,
                xref = "paper", yref = "paper",
                xanchor = "center", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
                align = "center"
              ),
              list(
                text = "Fuente: INE. Padrón Electoral y Lista Nominal de Electores.",
                x = 0.0, y = -0.20,
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
                text = "Datos de origen no disponibles para este corte",
                xref = "paper", yref = "paper",
                x = 0.5, y = 0.5,
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
              xref = "paper", yref = "paper",
              x = 0.5, y = 0.5,
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
    
    if (!all(c("padron_nacional", "lista_nacional") %in% colnames(df))) {
      return(NULL)
    }
    
    total_padron <- sum(df$padron_nacional, na.rm = TRUE)
    total_lista <- sum(df$lista_nacional, na.rm = TRUE)
    
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
            text = generar_texto_alcance(input),
            x = 0.5,
            xref = "paper",
            y = 1.05,
            yref = "paper",
            xanchor = "center",
            yanchor = "top",
            showarrow = FALSE,
            font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
            align = "center"
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
        margin = list(t = 120, b = 100, l = 50, r = 50),
        showlegend = FALSE
      )
    
    message("✅ Gráfico de tasa de inclusión renderizado")
    return(p)
  })
  
  message("✅ Módulo lista_nominal_server_graficas inicializado correctamente")
}