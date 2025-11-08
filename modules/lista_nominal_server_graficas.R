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
        
        # ========== USAR FILA DE TOTALES CON COLUMNAS SEPARADAS ==========
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
            # ========== COLUMNAS SEPARADAS + TOTALES ==========
            registro <- data.frame(
              fecha = as.Date(fecha, origin = "1970-01-01"),
              padron_nacional = padron_nacional,
              padron_extranjero = ifelse(is.na(padron_extranjero), NA, padron_extranjero),
              lista_nacional = lista_nacional,
              lista_extranjero = ifelse(is.na(lista_extranjero), NA, lista_extranjero),
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
              padron_nacional = padron_nacional,
              padron_extranjero = ifelse(is.na(padron_extranjero), NA, padron_extranjero),
              lista_nacional = lista_nacional,
              lista_extranjero = ifelse(is.na(lista_extranjero), NA, lista_extranjero),
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
          
          padron_nacional <- sum(df$padron_nacional, na.rm = TRUE)
          padron_extranjero <- sum(df$padron_extranjero, na.rm = TRUE)
          lista_nacional <- sum(df$lista_nacional, na.rm = TRUE)
          lista_extranjero <- sum(df$lista_extranjero, na.rm = TRUE)
          
          registro <- data.frame(
            fecha = as.Date(fecha, origin = "1970-01-01"),
            padron_nacional = padron_nacional,
            padron_extranjero = ifelse(is.na(padron_extranjero) || padron_extranjero == 0, NA, padron_extranjero),
            lista_nacional = lista_nacional,
            lista_extranjero = ifelse(is.na(lista_extranjero) || lista_extranjero == 0, NA, lista_extranjero),
            padron_electoral = padron_nacional + padron_extranjero,
            lista_nominal = lista_nacional + lista_extranjero,
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
        message("🔍 [DEBUG] Procesando año: ", año)
        
        fechas_año <- catalog$historico[format(catalog$historico, "%Y") == año]
        
        if (length(fechas_año) == 0) {
          message("   ⚠️ [DEBUG] Sin fechas para año ", año)
          next
        }
        
        if (length(fechas_año) > 0) {
          ultima_fecha <- max(fechas_año)
          message("   📅 [DEBUG] Última fecha del año ", año, ": ", as.Date(ultima_fecha, origin = "1970-01-01"))
          
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
          }, error = function(e) {
            message("   ❌ [DEBUG] Error en cargar_lne para año ", año, ": ", e$message)
            return(NULL)
          })
          
          if (is.null(datos_temp)) {
            message("   ❌ [DEBUG] datos_temp es NULL para año ", año)
            next
          }
          
          if (is.null(datos_temp$totales)) {
            message("   ❌ [DEBUG] datos_temp$totales es NULL para año ", año)
            next
          }
          
          # ========== USAR FILA DE TOTALES CON DETECCIÓN DINÁMICA ==========
          if (!is.null(datos_temp) && !is.null(datos_temp$totales)) {
            totales_fila <- datos_temp$totales
            
            message("   ✅ [DEBUG] Fila totales obtenida para año ", año)
            
            # Columnas principales (siempre existen)
            padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
            lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
            
            # Columnas extranjero
            padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
            } else NULL
            
            lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
            } else NULL
            
            # Columnas de sexo NACIONAL
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
            
            # ========== NUEVO: Columnas de sexo EXTRANJERO ==========
            padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_hombres)))
            } else NA
            
            padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_mujeres)))
            } else NA
            
            lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_hombres)))
            } else NA
            
            lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_mujeres)))
            } else NA
            
            # Construir padrón electoral
            padron_electoral <- padron_nacional
            if (!is.null(padron_extranjero) && !is.na(padron_extranjero)) {
              padron_electoral <- padron_electoral + padron_extranjero
            }
            
            # Construir lista nominal
            lista_nominal <- lista_nacional
            if (!is.null(lista_extranjero) && !is.na(lista_extranjero)) {
              lista_nominal <- lista_nominal + lista_extranjero
            }
            
            # Validar que los valores principales no sean NA
            if (!is.na(padron_electoral) && !is.na(lista_nominal)) {
              lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
                año = as.character(año),
                fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
                padron_nacional = padron_nacional,
                padron_extranjero = ifelse(!is.null(padron_extranjero) && !is.na(padron_extranjero), padron_extranjero, NA),
                lista_nacional = lista_nacional,
                lista_extranjero = ifelse(!is.null(lista_extranjero) && !is.na(lista_extranjero), lista_extranjero, NA),
                padron_hombres = padron_hombres,
                padron_mujeres = padron_mujeres,
                lista_hombres = lista_hombres,
                lista_mujeres = lista_mujeres,
                # NUEVO: Columnas extranjero por sexo
                padron_extranjero_hombres = padron_extranjero_hombres,
                padron_extranjero_mujeres = padron_extranjero_mujeres,
                lista_extranjero_hombres = lista_extranjero_hombres,
                lista_extranjero_mujeres = lista_extranjero_mujeres,
                stringsAsFactors = FALSE
              )
              
              message("   ✅ ", año, " | Padrón: ", format(padron_electoral, big.mark = ","),
                      " | Lista: ", format(lista_nominal, big.mark = ","))
            } else {
              message("   ❌ [DEBUG] Valores principales son NA para año ", año)
            }
          }
        }
      }
      
      if (length(lista_anuales) == 0) {
        message("⚠️ [datos_anuales_completos] No se cargaron datos anuales")
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
            
            # Columnas principales
            padron_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$padron_nacional)))
            lista_nacional <- as.numeric(gsub(",", "", as.character(totales_fila$lista_nacional)))
            
            # Columnas extranjero
            padron_extranjero <- if ("padron_extranjero" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero)))
            } else NULL
            
            lista_extranjero <- if ("lista_extranjero" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero)))
            } else NULL
            
            # Columnas de sexo nacional
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
            
            # NUEVO: Columnas de sexo extranjero
            padron_extranjero_hombres <- if ("padron_extranjero_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_hombres)))
            } else NA
            
            padron_extranjero_mujeres <- if ("padron_extranjero_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$padron_extranjero_mujeres)))
            } else NA
            
            lista_extranjero_hombres <- if ("lista_extranjero_hombres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_hombres)))
            } else NA
            
            lista_extranjero_mujeres <- if ("lista_extranjero_mujeres" %in% names(totales_fila)) {
              as.numeric(gsub(",", "", as.character(totales_fila$lista_extranjero_mujeres)))
            } else NA
            
            # Construir totales
            padron_electoral <- padron_nacional
            if (!is.null(padron_extranjero) && !is.na(padron_extranjero)) {
              padron_electoral <- padron_electoral + padron_extranjero
            }
            
            lista_nominal <- lista_nacional
            if (!is.null(lista_extranjero) && !is.na(lista_extranjero)) {
              lista_nominal <- lista_nominal + lista_extranjero
            }
            
            # Validar que los valores principales no sean NA
            if (!is.na(padron_electoral) && !is.na(lista_nominal)) {
              lista_anuales[[length(lista_anuales) + 1]] <- data.frame(
                año = as.character(año),
                fecha = as.Date(ultima_fecha, origin = "1970-01-01"),
                padron_nacional = padron_nacional,
                padron_extranjero = ifelse(!is.null(padron_extranjero) && !is.na(padron_extranjero), padron_extranjero, NA),
                lista_nacional = lista_nacional,
                lista_extranjero = ifelse(!is.null(lista_extranjero) && !is.na(lista_extranjero), lista_extranjero, NA),
                padron_hombres = padron_hombres,
                padron_mujeres = padron_mujeres,
                lista_hombres = lista_hombres,
                lista_mujeres = lista_mujeres,
                # NUEVO: Columnas extranjero por sexo
                padron_extranjero_hombres = padron_extranjero_hombres,
                padron_extranjero_mujeres = padron_extranjero_mujeres,
                lista_extranjero_hombres = lista_extranjero_hombres,
                lista_extranjero_mujeres = lista_extranjero_mujeres,
                stringsAsFactors = FALSE
              )
            }
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
  
  # ========== FUNCIÓN AUXILIAR: PROYECCIÓN CON TASA DE CRECIMIENTO (NACIONAL) ==========
  
  proyectar_con_tasa_crecimiento <- function(datos, meses_proyectar = 5, usar_columnas_separadas = FALSE) {
    if (is.null(datos) || nrow(datos) < 2) {
      return(NULL)
    }
    
    # Calcular tasa de crecimiento mensual promedio
    n <- nrow(datos)
    
    if (usar_columnas_separadas) {
      # Para Nacional: usar lista_nacional
      valor_inicial <- datos$lista_nacional[1]
      valor_final <- datos$lista_nacional[n]
      padron_inicial <- datos$padron_nacional[1]
      padron_final <- datos$padron_nacional[n]
    } else {
      # Para totales (retrocompatibilidad)
      valor_inicial <- datos$lista_nominal[1]
      valor_final <- datos$lista_nominal[n]
      padron_inicial <- datos$padron_electoral[1]
      padron_final <- datos$padron_electoral[n]
    }
    
    if (valor_inicial == 0 || is.na(valor_inicial) || is.na(valor_final)) {
      return(NULL)
    }
    
    tasa_mensual_lista <- ((valor_final / valor_inicial) ^ (1 / (n - 1))) - 1
    tasa_mensual_padron <- ((padron_final / padron_inicial) ^ (1 / (n - 1))) - 1
    
    # Crear fechas proyectadas - FORZAR ÚLTIMO DÍA DEL MES
    ultima_fecha <- max(datos$fecha)
    anio_base <- as.integer(format(ultima_fecha, "%Y"))
    mes_base <- as.integer(format(ultima_fecha, "%m"))
    
    # Crear lista para almacenar fechas
    fechas_proyectadas <- list()
    
    for (i in 1:meses_proyectar) {
      mes_proyectado <- mes_base + i
      anio_proyectado <- anio_base
      
      # Ajustar si pasa de diciembre
      if (mes_proyectado > 12) {
        anio_proyectado <- anio_base + floor((mes_proyectado - 1) / 12)
        mes_proyectado <- ((mes_proyectado - 1) %% 12) + 1
      }
      
      # Obtener último día del mes
      # Crear fecha del día 1 del mes siguiente, luego restar 1 día
      if (mes_proyectado == 12) {
        ultimo_dia <- as.Date(paste0(anio_proyectado + 1, "-01-01")) - 1
      } else {
        ultimo_dia <- as.Date(paste0(anio_proyectado, "-", sprintf("%02d", mes_proyectado + 1), "-01")) - 1
      }
      
      fechas_proyectadas[[i]] <- ultimo_dia
    }
    
    # Convertir lista a vector de fechas
    fechas_proyectadas <- do.call(c, fechas_proyectadas)
    
    message("📅 [PROYECCIÓN] Fechas generadas: ", paste(fechas_proyectadas, collapse = ", "))
    
    # Proyectar valores
    lista_proyectada <- numeric(meses_proyectar)
    padron_proyectado <- numeric(meses_proyectar)
    
    for (i in 1:meses_proyectar) {
      lista_proyectada[i] <- valor_final * ((1 + tasa_mensual_lista) ^ i)
      padron_proyectado[i] <- padron_final * ((1 + tasa_mensual_padron) ^ i)
    }
    
    proyecciones <- data.frame(
      fecha = fechas_proyectadas,
      lista_proyectada = lista_proyectada,
      padron_proyectado = padron_proyectado,
      tipo = "Proyección",
      stringsAsFactors = FALSE
    )
    
    message("✅ Proyección calculada: tasa mensual lista = ", round(tasa_mensual_lista * 100, 4), "%")
    
    return(proyecciones)
  }
  
  # ========== GRÁFICAS PARA DATOS HISTÓRICOS ==========
  
  # ========== GRÁFICA 1: EVOLUCIÓN MENSUAL AÑO ACTUAL ==========
  output$grafico_evolucion_2025 <- renderPlotly({
    req(input$tipo_corte == "historico")
    req(input$ambito_datos)
    
    datos_completos <- datos_historicos_year()
    
    # Obtener año de los datos (del último registro disponible)
    year_datos <- format(max(datos_completos$fecha), "%Y")
    
    # ========== DEBUG: IMPRIMIR FECHAS REALES ==========
    message("📅 [DEBUG] Fechas en datos_completos:")
    message(paste(datos_completos$fecha, collapse = ", "))
    message("📅 [DEBUG] Total de fechas: ", nrow(datos_completos))
    # ========== FIN DEBUG ==========
    
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
    
    # Obtener año de los datos (del último registro disponible)
    year_datos <- format(max(datos_completos$fecha), "%Y")
    
    # Calcular meses restantes hasta diciembre
    ultimo_mes <- as.numeric(format(max(datos_completos$fecha), "%m"))
    meses_restantes <- 12 - ultimo_mes
    
    # ========== GRÁFICA NACIONAL ==========
    if (input$ambito_datos == "nacional") {
      
      # Proyectar usando columnas nacionales
      proyeccion <- NULL
      if (meses_restantes > 0) {
        # Crear dataframe temporal con solo columnas nacionales
        datos_para_proyeccion <- datos_completos
        datos_para_proyeccion$lista_nominal <- datos_para_proyeccion$lista_nacional
        datos_para_proyeccion$padron_electoral <- datos_para_proyeccion$padron_nacional
        proyeccion <- proyectar_con_tasa_crecimiento(datos_para_proyeccion, meses_restantes)
      }
      
      # Crear gráfico
      p <- plot_ly()
      
      # 1. Padrón Nacional
      p <- p %>% add_trace(
        data = datos_completos,
        x = ~fecha,
        y = ~padron_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Nacional',
        line = list(color = '#003E66', width = 3),
        marker = list(size = 8, color = '#003E66'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Padrón Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Nacional
      p <- p %>% add_trace(
        data = datos_completos,
        x = ~fecha,
        y = ~lista_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Nacional',
        line = list(color = '#AE0E35', width = 3),
        marker = list(size = 8, color = '#AE0E35'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Lista Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Proyecciones
      if (!is.null(proyeccion)) {
        # Proyección Padrón
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~padron_proyectado,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Padrón',
          line = list(color = '#6B8FB3', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Proyección Lista
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~lista_proyectada,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Lista',
          line = list(color = '#D66B7D', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Lista: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== CONFIGURACIÓN DEL EJE X CORREGIDA ==========
      # Combinar fechas reales + fechas proyectadas
      fechas_reales <- datos_completos$fecha
      
      # Si hay proyección, combinar fechas
      if (!is.null(proyeccion) && nrow(proyeccion) > 0) {
        fechas_completas_eje <- c(fechas_reales, proyeccion$fecha)
      } else {
        fechas_completas_eje <- fechas_reales
      }
      
      # Generar etiquetas para todas las fechas
      etiquetas_meses <- format(fechas_completas_eje, "%b")
      
      # Layout con eje X corregido
      p <- p %>% layout(
        title = list(
          text = paste0("Proyección de Padrón y Lista Nominal ", year_datos, " - Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_completas_eje,  # ← INCLUYE datos reales + proyección
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_reales) - 5, 
                    as.Date(paste0(year_datos, "-12-31")))
        ),
        yaxis = list(
          title = "Número de Electores", 
          separatethousands = TRUE
        ),
        legend = list(
          orientation = "h", 
          xanchor = "center", 
          x = 0.5, 
          y = -0.20
        ),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.35,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 1: Proyección ", year_datos, " Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      # Filtrar solo datos con extranjero
      datos_extranjero <- datos_completos[!is.na(datos_completos$padron_extranjero) & 
                                            !is.na(datos_completos$lista_extranjero), ]
      
      if (nrow(datos_extranjero) == 0) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero no disponibles para este año",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      # Proyectar usando columnas extranjero
      proyeccion <- NULL
      if (meses_restantes > 0) {
        # Crear dataframe temporal
        datos_para_proyeccion <- datos_extranjero
        datos_para_proyeccion$lista_nominal <- datos_para_proyeccion$lista_extranjero
        datos_para_proyeccion$padron_electoral <- datos_para_proyeccion$padron_extranjero
        proyeccion <- proyectar_con_tasa_crecimiento(datos_para_proyeccion, meses_restantes)
      }
      
      # Crear gráfico
      p <- plot_ly()
      
      # 1. Padrón Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~fecha,
        y = ~padron_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Extranjero',
        line = list(color = '#EAC43E', width = 3),
        marker = list(size = 8, color = '#EAC43E'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Padrón Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~fecha,
        y = ~lista_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Extranjero',
        line = list(color = '#B3D491', width = 3),
        marker = list(size = 8, color = '#B3D491'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Lista Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Proyecciones
      if (!is.null(proyeccion)) {
        # Proyección Padrón
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~padron_proyectado,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Padrón',
          line = list(color = '#F5CA45', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Proyección Lista
        p <- p %>% add_trace(
          data = proyeccion,
          x = ~fecha,
          y = ~lista_proyectada,
          type = 'scatter',
          mode = 'lines',
          name = 'Proyección Lista',
          line = list(color = '#CCE4B1', width = 2, dash = 'dash'),
          hovertemplate = paste0(
            '<b>%{x|%B %Y}</b><br>',
            'Proyección Lista: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== CONFIGURACIÓN DEL EJE X CORREGIDA ==========
      # Combinar fechas reales + fechas proyectadas
      fechas_reales <- datos_extranjero$fecha
      
      # Si hay proyección, combinar fechas
      if (!is.null(proyeccion) && nrow(proyeccion) > 0) {
        fechas_completas_eje <- c(fechas_reales, proyeccion$fecha)
      } else {
        fechas_completas_eje <- fechas_reales
      }
      
      # Generar etiquetas para todas las fechas
      etiquetas_meses <- format(fechas_completas_eje, "%b")
      
      # Layout con eje X corregido
      p <- p %>% layout(
        title = list(
          text = paste0("Proyección de Padrón y Lista Nominal ", year_datos, " - Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(
          title = "",
          type = 'date',
          tickmode = "array",
          tickvals = fechas_completas_eje,  # ← INCLUYE datos reales + proyección
          ticktext = etiquetas_meses,
          tickangle = 0,
          range = c(min(fechas_reales) - 5, 
                    as.Date(paste0(year_datos, "-12-31")))
        ),
        yaxis = list(
          title = "Número de Electores", 
          separatethousands = TRUE
        ),
        legend = list(
          orientation = "h", 
          xanchor = "center", 
          x = 0.5, 
          y = -0.20
        ),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.35,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 1: Proyección ", year_datos, " Extranjero renderizado")
      return(p)
    }
  })
  
  # ========== GRÁFICA 2: EVOLUCIÓN ANUAL ==========
  output$grafico_evolucion_anual <- renderPlotly({
    req(input$tipo_corte == "historico")
    req(input$ambito_datos)
    
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
    
    # ========== GRÁFICA NACIONAL ==========
    if (input$ambito_datos == "nacional") {
      
      p <- plot_ly()
      
      # 1. Padrón Nacional
      p <- p %>% add_trace(
        data = datos_anuales,
        x = ~año,
        y = ~padron_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Nacional',
        line = list(color = '#003E66', width = 3),
        marker = list(size = 10, color = '#003E66'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Padrón Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Nacional
      p <- p %>% add_trace(
        data = datos_anuales,
        x = ~año,
        y = ~lista_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Nacional',
        line = list(color = '#AE0E35', width = 3),
        marker = list(size = 10, color = '#AE0E35'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Lista Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual (2017-", anio_actual(), ") - Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.35,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 2: Evolución anual Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      # Filtrar solo años con datos de extranjero (2020 en adelante)
      datos_extranjero <- datos_anuales[!is.na(datos_anuales$padron_extranjero) & 
                                          !is.na(datos_anuales$lista_extranjero), ]
      
      if (nrow(datos_extranjero) == 0) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero disponibles desde 2020",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      p <- plot_ly()
      
      # 1. Padrón Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~año,
        y = ~padron_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Extranjero',
        line = list(color = '#EAC43E', width = 3),
        marker = list(size = 10, color = '#EAC43E'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Padrón Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # 2. Lista Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~año,
        y = ~lista_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Extranjero',
        line = list(color = '#B3D491', width = 3),
        marker = list(size = 10, color = '#B3D491'),
        hovertemplate = paste0(
          '<b>%{x}</b><br>',
          'Lista Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual (2020-", anio_actual(), ") - Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.35,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 2: Evolución anual Extranjero renderizado")
      return(p)
    }
  })
  
  # ========== GRÁFICA 3: EVOLUCIÓN ANUAL + DESGLOSE POR SEXO ==========
  output$grafico_evolucion_anual_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    req(input$ambito_datos)
    
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
    
    # ========== GRÁFICA NACIONAL ==========
    if (input$ambito_datos == "nacional") {
      
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
      
      # Crear gráfico con ORDEN REORDENADO
      p <- plot_ly()
      
      # ========== ORDEN NUEVO: M → M → H → H ==========
      
      # 1. Padrón Mujeres (PRIMERO)
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
      
      # 2. Lista Mujeres (SEGUNDO)
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
      
      # 3. Padrón Hombres (TERCERO)
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
      
      # 4. Lista Hombres (CUARTO)
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
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual por Sexo (2017-", anio_actual(), ") - Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.35,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 3: Evolución anual por sexo Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO (NUEVA LÓGICA HÍBRIDA) ==========
      
      # Filtrar años con datos de extranjero
      datos_extranjero <- datos_anuales[!is.na(datos_anuales$padron_extranjero) & 
                                          !is.na(datos_anuales$lista_extranjero), ]
      
      if (nrow(datos_extranjero) == 0) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero disponibles desde 2020",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      # ========== DETECTAR AÑOS CON/SIN DATOS DE SEXO ==========
      datos_extranjero$tiene_sexo <- !is.na(datos_extranjero$padron_extranjero_hombres) & 
        !is.na(datos_extranjero$padron_extranjero_mujeres) &
        !is.na(datos_extranjero$lista_extranjero_hombres) &
        !is.na(datos_extranjero$lista_extranjero_mujeres)
      
      años_sin_sexo <- datos_extranjero$año[!datos_extranjero$tiene_sexo]
      años_con_sexo <- datos_extranjero$año[datos_extranjero$tiene_sexo]
      
      message("📊 Años SIN sexo: ", paste(años_sin_sexo, collapse = ", "))
      message("📊 Años CON sexo: ", paste(años_con_sexo, collapse = ", "))
      
      # Crear gráfico
      p <- plot_ly()
      
      # ========== GRAFICAR AÑOS SIN SEXO (2 LÍNEAS) ==========
      if (length(años_sin_sexo) > 0) {
        datos_sin_sexo <- datos_extranjero[datos_extranjero$año %in% años_sin_sexo, ]
        
        # Padrón Total
        p <- p %>% add_trace(
          data = datos_sin_sexo,
          x = ~año,
          y = ~padron_extranjero,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Padrón Extranjero',
          line = list(color = '#EAC43E', width = 3),
          marker = list(size = 10, color = '#EAC43E'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Padrón: %{y:,.0f}<extra></extra>'
          )
        )
        
        # Lista Total
        p <- p %>% add_trace(
          data = datos_sin_sexo,
          x = ~año,
          y = ~lista_extranjero,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Lista Extranjero',
          line = list(color = '#B3D491', width = 3),
          marker = list(size = 10, color = '#B3D491'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Lista: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== GRAFICAR AÑOS CON SEXO (4 LÍNEAS) - ORDEN CORREGIDO ==========
      if (length(años_con_sexo) > 0) {
        datos_con_sexo <- datos_extranjero[datos_extranjero$año %in% años_con_sexo, ]
        
        # ========== ORDEN CORREGIDO: H → H → M → M (coincide con orden visual descendente) ==========
        
        # 1. Padrón Hombres (PRIMERO - línea más alta)
        p <- p %>% add_trace(
          data = datos_con_sexo,
          x = ~año,
          y = ~padron_extranjero_hombres,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Padrón Hombres',
          line = list(color = '#D4A500', width = 2.5),
          marker = list(size = 8, color = '#D4A500'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Padrón H: %{y:,.0f}<extra></extra>'
          )
        )
        
        # 2. Padrón Mujeres (SEGUNDO)
        p <- p %>% add_trace(
          data = datos_con_sexo,
          x = ~año,
          y = ~padron_extranjero_mujeres,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Padrón Mujeres',
          line = list(color = '#F5CA45', width = 2.5),
          marker = list(size = 8, color = '#F5CA45'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Padrón M: %{y:,.0f}<extra></extra>'
          )
        )
        
        # 3. Lista Hombres (TERCERO)
        p <- p %>% add_trace(
          data = datos_con_sexo,
          x = ~año,
          y = ~lista_extranjero_hombres,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Lista Hombres',
          line = list(color = '#8FB369', width = 2.5, dash = 'dot'),
          marker = list(size = 8, color = '#8FB369', symbol = 'square'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Lista H: %{y:,.0f}<extra></extra>'
          )
        )
        
        # 4. Lista Mujeres (CUARTO - línea más baja)
        p <- p %>% add_trace(
          data = datos_con_sexo,
          x = ~año,
          y = ~lista_extranjero_mujeres,
          type = 'scatter',
          mode = 'lines+markers',
          name = 'Lista Mujeres',
          line = list(color = '#CCE4B1', width = 2.5, dash = 'dot'),
          marker = list(size = 8, color = '#CCE4B1', symbol = 'square'),
          hovertemplate = paste0(
            '<b>%{x}</b><br>',
            'Lista M: %{y:,.0f}<extra></extra>'
          )
        )
      }
      
      # ========== PREPARAR TEXTO DE ANOTACIÓN ==========
      texto_nota <- ""
      if (length(años_sin_sexo) > 0) {
        if (length(años_sin_sexo) == 1) {
          texto_nota <- paste0("Nota: Año ", años_sin_sexo, " sin desglose por sexo (se muestran totales).")
        } else {
          texto_nota <- paste0("Nota: Años ", paste(años_sin_sexo, collapse = ", "), " sin desglose por sexo (se muestran totales).")
        }
      }
      
      # ========== LAYOUT CON ANOTACIÓN ==========
      annotations_list <- list(
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
          text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
          x = 0.5, y = -0.45,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
        )
      )
      
      # Agregar nota si hay años sin sexo
      if (texto_nota != "") {
        annotations_list[[length(annotations_list) + 1]] <- list(
          text = texto_nota,
          x = 0.5, y = 1.05,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 11, color = "#EAC43E", family = "Arial, sans-serif", style = "italic"),
          align = "center"
        )
      }
      
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Anual por Sexo (2020-", anio_actual(), ") - Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'category'),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.25),
        margin = list(t = 130, b = 140, l = 90, r = 50),
        hovermode = 'x unified',
        annotations = annotations_list
      )
      
      message("✅ Gráfico 3: Evolución anual por sexo Extranjero (híbrido) renderizado")
      return(p)
    }
  })
  
  # ========== GRÁFICA 4: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO ==========
  output$grafico_evolucion_year <- renderPlotly({
    req(input$tipo_corte == "historico")
    req(input$btn_consultar > 0)  # ← NUEVO: Solo cargar si usuario consultó
    req(input$ambito_datos)
    
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
    
    # ========== GRÁFICA NACIONAL ==========
    if (input$ambito_datos == "nacional") {
      
      p <- plot_ly()
      
      # Padrón Nacional
      p <- p %>% add_trace(
        data = datos_completos,
        x = ~fecha,
        y = ~padron_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Nacional',
        line = list(color = '#003E66', width = 3),
        marker = list(size = 8, color = '#003E66'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Padrón Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Lista Nacional
      p <- p %>% add_trace(
        data = datos_completos,
        x = ~fecha,
        y = ~lista_nacional,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Nacional',
        line = list(color = '#AE0E35', width = 3),
        marker = list(size = 8, color = '#AE0E35'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Lista Nacional: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Mensual ", year_datos, " - Nacional"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'date', tickformat = "%b"),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.30,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 4: Evolución mensual ", year_datos, " Nacional renderizado")
      return(p)
      
    } else {
      # ========== GRÁFICA EXTRANJERO ==========
      
      # Filtrar solo datos con extranjero
      datos_extranjero <- datos_completos[!is.na(datos_completos$padron_extranjero) & 
                                            !is.na(datos_completos$lista_extranjero), ]
      
      # Obtener año de la consulta para mensaje personalizado
      year_consultado <- format(datos_completos$fecha[1], "%Y")
      
      if (nrow(datos_extranjero) == 0) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = paste0("Datos de extranjero no disponibles para el año ", year_consultado, 
                                     ".<br>Los datos de extranjero están disponibles desde 2020."),
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
      p <- plot_ly()
      
      # Padrón Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~fecha,
        y = ~padron_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Padrón Extranjero',
        line = list(color = '#EAC43E', width = 3),
        marker = list(size = 8, color = '#EAC43E'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Padrón Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Lista Extranjero
      p <- p %>% add_trace(
        data = datos_extranjero,
        x = ~fecha,
        y = ~lista_extranjero,
        type = 'scatter',
        mode = 'lines+markers',
        name = 'Lista Extranjero',
        line = list(color = '#B3D491', width = 3),
        marker = list(size = 8, color = '#B3D491'),
        hovertemplate = paste0(
          '<b>%{x|%B %Y}</b><br>',
          'Lista Extranjero: %{y:,.0f}<extra></extra>'
        )
      )
      
      # Layout
      p <- p %>% layout(
        title = list(
          text = paste0("Evolución Mensual ", year_datos, " - Extranjero"),
          font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
          x = 0.5,
          xanchor = "center"
        ),
        xaxis = list(title = "", type = 'date', tickformat = "%b"),
        yaxis = list(title = "Número de Electores", separatethousands = TRUE),
        legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
        margin = list(t = 120, b = 120, l = 90, r = 50),
        hovermode = 'x unified',
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
            x = 0.5, y = -0.30,
            xref = "paper", yref = "paper",
            xanchor = "center", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
            align = "center"
          )
        )
      )
      
      message("✅ Gráfico 4: Evolución mensual ", year_datos, " Extranjero renderizado")
      return(p)
    }
  })
  
  # ========== GRÁFICA 5: EVOLUCIÓN MENSUAL DEL AÑO SELECCIONADO + SEXO ==========
  output$grafico_evolucion_year_sexo <- renderPlotly({
    req(input$tipo_corte == "historico")
    req(input$btn_consultar > 0)  # ← NUEVO: Solo cargar si usuario consultó
    req(input$ambito_datos)
    
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
    
    # Solo disponible para Nacional
    if (input$ambito_datos == "extranjero") {
      return(plot_ly() %>%
               layout(
                 xaxis = list(visible = FALSE),
                 yaxis = list(visible = FALSE),
                 annotations = list(
                   list(
                     text = "Desglose por sexo no disponible para ámbito Extranjero",
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
    
    # Layout
    p <- p %>% layout(
      title = list(
        text = paste0("Evolución Mensual ", year_datos, " por Sexo - Nacional"),
        font = list(size = 18, color = "#333", family = "Arial, sans-serif"),
        x = 0.5,
        xanchor = "center"
      ),
      xaxis = list(title = "", type = 'date', tickformat = "%b"),
      yaxis = list(title = "Número de Electores", separatethousands = TRUE),
      legend = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.20),
      margin = list(t = 120, b = 120, l = 90, r = 50),
      hovermode = 'x unified',
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
          text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
          x = 0.5, y = -0.30,
          xref = "paper", yref = "paper",
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
          align = "center"
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
    req(input$ambito_datos)
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
    
    message("📊 Renderizando gráfico semanal: ", desglose_actual, " - Ámbito: ", input$ambito_datos)
    
    # ========== FILTRAR POR ÁMBITO ==========
    if (input$ambito_datos == "nacional") {
      # Usar columnas nacionales
      col_padron <- "padron_nacional"
      col_lista <- "lista_nacional"
      titulo_base <- "Nacional"
    } else {
      # Usar columnas extranjero
      col_padron <- "padron_extranjero"
      col_lista <- "lista_extranjero"
      titulo_base <- "Extranjero"
      
      # Verificar que existan columnas
      if (!col_padron %in% colnames(df) || !col_lista %in% colnames(df)) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero no disponibles para este corte",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
    }
    
    # ========== GRÁFICA POR SEXO ==========
    if (desglose_actual == "Sexo") {
      
      # Definir columnas según ámbito
      if (input$ambito_datos == "nacional") {
        cols_sexo <- c("padron_nacional_hombres", "padron_nacional_mujeres", 
                       "lista_nacional_hombres", "lista_nacional_mujeres")
      } else {
        # Para extranjero, normalmente no hay desglose por sexo
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Desglose por sexo no disponible para ámbito Extranjero",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
      
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
              text = paste0("Padrón Electoral y Lista Nominal por Sexo - ", titulo_base),
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
                text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
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
        total_padron <- sum(df[[col_padron]], na.rm = TRUE)
        total_lista <- sum(df[[col_lista]], na.rm = TRUE)
        
        datos_grafico <- data.frame(
          Tipo = c("Padrón Electoral", "Lista Nominal"),
          Cantidad = c(total_padron, total_lista),
          stringsAsFactors = FALSE
        )
        
        # Colores según ámbito
        colores <- if (input$ambito_datos == "nacional") {
          c("#003E66", "#AE0E35")
        } else {
          c("#EAC43E", "#B3D491")
        }
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Tipo,
          y = ~Cantidad,
          type = 'bar',
          marker = list(color = colores),
          text = ~paste0(format(Cantidad, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = paste0("Padrón Electoral y Lista Nominal - ", titulo_base),
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
                text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
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
        
        color_edad <- if (input$ambito_datos == "nacional") "#C0311A" else "#B3D491"
        
        p <- plot_ly(
          data = datos_grafico,
          x = ~Grupo,
          y = ~Lista_Nominal,
          type = 'bar',
          marker = list(color = color_edad),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '<b>%{x}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(text = paste0("Lista Nominal por Grupo de Edad - ", titulo_base),
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
                text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
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
      
      if ("nombre_entidad" %in% colnames(df) && col_lista %in% colnames(df)) {
        
        datos_grafico <- df %>%
          group_by(Entidad = nombre_entidad) %>%
          summarise(
            Lista_Nominal = sum(.data[[col_lista]], na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(Lista_Nominal)) %>%
          head(10)
        
        datos_grafico <- as.data.frame(datos_grafico)
        
        color_entidad <- if (input$ambito_datos == "nacional") "#44559B" else "#EAC43E"
        
        p <- plot_ly(
          data = datos_grafico,
          y = ~reorder(Entidad, Lista_Nominal),
          x = ~Lista_Nominal,
          type = 'bar',
          orientation = 'h',
          marker = list(color = color_entidad),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '<b>%{y}</b><br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = paste0("Top 10 Entidades por Lista Nominal - ", titulo_base),
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
                text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
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
    
    message("✅ Gráfico semanal renderizado: ", desglose_actual, " - ", titulo_base)
    return(p)
  })
  
  # ========== GRÁFICO DE TASA DE INCLUSIÓN (SOLO SEMANALES) ==========
  output$`main-tasa_inclusion_plot` <- renderPlotly({
    req(input$tipo_corte == "semanal")
    req(input$ambito_datos)
    req(combinacion_valida())
    
    datos <- datos_columnas()
    
    if (is.null(datos) || is.null(datos$datos) || nrow(datos$datos) == 0) {
      return(NULL)
    }
    
    df <- datos$datos
    
    # ========== SELECCIONAR COLUMNAS SEGÚN ÁMBITO ==========
    if (input$ambito_datos == "nacional") {
      col_padron <- "padron_nacional"
      col_lista <- "lista_nacional"
      titulo_ambito <- "Nacional"
      color_lista <- "#4CAF50"
      color_diferencia <- "#FFC107"
    } else {
      col_padron <- "padron_extranjero"
      col_lista <- "lista_extranjero"
      titulo_ambito <- "Extranjero"
      color_lista <- "#8BC34A"
      color_diferencia <- "#FFB74D"
      
      # Verificar que existan columnas
      if (!col_padron %in% colnames(df) || !col_lista %in% colnames(df)) {
        return(plot_ly() %>%
                 layout(
                   xaxis = list(visible = FALSE),
                   yaxis = list(visible = FALSE),
                   annotations = list(
                     list(
                       text = "Datos de extranjero no disponibles para este corte",
                       xref = "paper", yref = "paper",
                       x = 0.5, y = 0.5,
                       showarrow = FALSE,
                       font = list(size = 14, color = "#666")
                     )
                   )
                 ))
      }
    }
    
    total_padron <- sum(df[[col_padron]], na.rm = TRUE)
    total_lista <- sum(df[[col_lista]], na.rm = TRUE)
    
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
          text = paste0("Tasa de Inclusión en Lista Nominal - ", titulo_ambito),
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
            text = "Fuente: INE. Estadística de Padrón Electoral y Lista Nominal del Electorado",
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
    
    message("✅ Gráfico de tasa de inclusión renderizado - ", titulo_ambito)
    return(p)
  })
  
  # ========== MODAL: INFORMACIÓN METODOLOGÍA GRÁFICA 1 ==========
  observeEvent(input$info_grafica1, {
    showModal(modalDialog(
      title = tags$div(
        style = "color: #003E66; font-weight: bold; font-size: 18px;",
        icon("chart-line"), " Metodología de Proyección"
      ),
      tags$div(
        style = "font-size: 14px; line-height: 1.8;",
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 15px;",
          "¿Cómo se calcula la proyección?"
        ),
        
        tags$p(
          "La proyección mostrada en esta gráfica utiliza un ", 
          tags$strong("modelo de tasa de crecimiento mensual promedio"), 
          " basado en los datos históricos disponibles del año en curso."
        ),
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 15px;",
          "Pasos del cálculo:"
        ),
        
        tags$ol(
          style = "padding-left: 20px;",
          tags$li(tags$strong("Datos base:"), " Se toman todos los cortes mensuales disponibles del año actual (último día de cada mes)."),
          tags$li(tags$strong("Tasa de crecimiento:"), " Se calcula la tasa de crecimiento mensual promedio entre el primer y último mes disponible."),
          tags$li(tags$strong("Proyección:"), " Se aplica esta tasa a los meses restantes hasta diciembre del año en curso."),
          tags$li(tags$strong("Fechas proyectadas:"), " Cada proyección corresponde al último día del mes respectivo (ej: 30/sep, 31/oct, 30/nov, 31/dic)."),
          tags$li(tags$strong("Visualización:"), " Las líneas punteadas representan los valores proyectados.")
        ),
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 15px;",
          "Fórmula aplicada:"
        ),
        
        tags$div(
          style = "background-color: #f8f9fa; padding: 15px; border-left: 4px solid #003E66; margin: 10px 0; font-family: 'Courier New', monospace;",
          tags$code("Tasa mensual = (Valor final / Valor inicial)^(1 / (n-1)) - 1"),
          tags$br(),
          tags$code("Valor proyectado(mes i) = Último valor × (1 + tasa)^i"),
          tags$br(),
          tags$code("Fecha proyectada(mes i) = Último día del mes i")
        ),
        
        tags$h5(
          style = "color: #44559B; font-weight: bold; margin-top: 15px;",
          icon("calculator"), " Ejemplo de cálculo:"
        ),
        
        tags$div(
          style = "background-color: #f0f8ff; padding: 12px; border-radius: 5px; margin: 10px 0;",
          tags$p(
            style = "margin: 5px 0;",
            tags$strong("Supongamos:"), " Lista Nominal enero = 95,000,000 | agosto = 97,500,000"
          ),
          tags$p(
            style = "margin: 5px 0;",
            "Tasa mensual = (97,500,000 / 95,000,000)^(1/7) - 1 = 0.378% mensual"
          ),
          tags$p(
            style = "margin: 5px 0;",
            "Proyección septiembre (30/sep) = 97,500,000 × (1.00378)^1 = 97,868,550"
          ),
          tags$p(
            style = "margin: 5px 0;",
            "Proyección octubre (31/oct) = 97,500,000 × (1.00378)^2 = 98,239,019"
          )
        ),
        
        tags$h5(
          style = "color: #AE0E35; font-weight: bold; margin-top: 15px;",
          icon("exclamation-triangle"), " Consideraciones importantes:"
        ),
        
        tags$ul(
          style = "padding-left: 20px;",
          tags$li("La proyección asume un ", tags$strong("crecimiento constante"), " basado en tendencias históricas del año."),
          tags$li("Es una ", tags$strong("estimación estadística"), " y puede variar con respecto a los valores reales."),
          tags$li("Se proyecta hasta ", tags$strong("diciembre del año en curso"), " únicamente."),
          tags$li("Las fechas proyectadas corresponden al ", tags$strong("último día de cada mes"), " para mantener consistencia con los datos históricos del INE."),
          tags$li("Se recomienda ", tags$strong("actualizar regularmente"), " con los datos oficiales del INE conforme se publiquen."),
          tags$li("Los valores proyectados se distinguen visualmente con ", tags$strong("líneas punteadas"), ".")
        ),
        
        tags$hr(style = "margin: 20px 0;"),
        
        tags$p(
          style = "font-size: 12px; color: #666; text-align: center;",
          icon("info-circle"), " Esta proyección es una herramienta de referencia y análisis. ",
          "Los datos oficiales son publicados mensualmente por el INE y prevalecen sobre cualquier estimación."
        )
      ),
      
      easyClose = TRUE,
      fade = TRUE,
      size = "l",
      footer = modalButton("Cerrar")
    ))
  })
  
  message("✅ Módulo lista_nominal_server_graficas inicializado correctamente")
}