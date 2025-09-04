#elecciones_federales_server_text_analysis

elecciones_federales_server_text_analysis <- function(input, output, session, datos_columnas) {
  ns <- session$ns
  
  output$`text_analysis-titulo_analisis_fed` <- renderUI({
    req(input$year, input$cargo, input$tipo_eleccion)
    
    cargo_formateado <- switch(input$cargo,
                               "DIPUTACION FEDERAL" = "Diputación Federal",
                               "SENADURIA" = "Senaduría",
                               "PRESIDENCIA" = "Presidencia",
                               input$cargo)
    
    if (input$tipo_eleccion == "EXTRAORDINARIA") {
      HTML(paste0(
        "<h3>Elección Federal Extraordinaria <span class='year-highlight'>", input$year, "</span></h3>",
        "<h3 class='cargo-subtitulo'>", cargo_formateado, "</h3>"
      ))
    } else {
      HTML(paste0(
        "<h3>Elecciones Federales <span class='year-highlight'>", input$year, "</span></h3>",
        "<h3 class='cargo-subtitulo'>", cargo_formateado, "</h3>"
      ))
    }
  })
  
  output$`text_analysis-alcance_analisis_fed` <- renderUI({
    req(input$year, input$cargo, input$estado, 
        input$cabecera, input$municipio)
    
    subtitulo_base <- paste("Estado:", input$estado, "- Distrito:", input$cabecera, 
                            "- Municipio:", input$municipio)
    
    subtitulo <- if (length(input$seccion) == 1 && input$seccion != "Todas") {
      paste(subtitulo_base, "- Sección:", input$seccion)
    } else if (length(input$seccion) > 1 && !("Todas" %in% input$seccion)) {
      secciones_seleccionadas <- paste(input$seccion, collapse = ", ")
      paste(subtitulo_base, "- Secciones:", secciones_seleccionadas)
    } else {
      subtitulo_base
    }
    
    HTML(paste0("<h4>Alcance del análisis:</h4><p class='alcance-analisis'>", subtitulo, "</p>"))
  })
  
  output$`text_analysis-texto_resumen_general_fed` <- renderUI({
    req(input$year, input$cargo, input$tipo_eleccion)
    datos <- datos_columnas()$datos
    
    if (nrow(datos) == 0) {
      return(HTML("<p class='sidebar-alert'>No hay datos disponibles con los filtros seleccionados.</p>"))
    }
    
    titulo_analisis <- switch(input$cargo,
                              "DIPUTACION FEDERAL" = "diputaciones federales",
                              "SENADURIA" = "senadurías",
                              "PRESIDENCIA" = "presidencia")
    
    filtro_geo <- if (input$estado != "Nacional") {
      if (input$cabecera != "Todos") {
        if (input$municipio != "Todos") {
          if (length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
            if (length(input$seccion) <= 5) {
              paste("en las secciones", paste(input$seccion, collapse = ", "), 
                    "del municipio de", input$municipio, " distrito", input$cabecera, 
                    "de", input$estado)
            } else {
              paste("en", length(input$seccion), "secciones seleccionadas del municipio de", 
                    input$municipio, ", distrito", input$cabecera, "de", input$estado)
            }
          } else {
            paste("en el municipio de", input$municipio, "del distrito", input$cabecera, 
                  "de", input$estado)
          }
        } else {
          if (length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
            if (length(input$seccion) <= 5) {
              paste("en las secciones", paste(input$seccion, collapse = ", "), 
                    "del distrito", input$cabecera, "de", input$estado)
            } else {
              paste("en", length(input$seccion), "secciones seleccionadas del distrito", 
                    input$cabecera, "de", input$estado)
            }
          } else {
            paste("en el distrito", input$cabecera, "de", input$estado)
          }
        }
      } else {
        if (input$municipio != "Todos") {
          if (length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
            if (length(input$seccion) <= 5) {
              paste("en las secciones", paste(input$seccion, collapse = ", "), 
                    "del municipio de", input$municipio, "en", input$estado)
            } else {
              paste("en", length(input$seccion), "secciones seleccionadas del municipio de", 
                    input$municipio, "en", input$estado)
            }
          } else {
            paste("en el municipio de", input$municipio, "de", input$estado)
          }
        } else {
          paste("en el estado de", input$estado)
        }
      }
    } else {
      "a nivel nacional"
    }
    
    key <- paste(input$year, input$cargo, sep = "_")
    columnas_partidos <- partidos_mapping[[key]]
    total_votos <- sum(datos[, columnas_partidos], na.rm = TRUE)
    total_formateado <- format(total_votos, big.mark = ",")
    
    if (input$tipo_eleccion == "EXTRAORDINARIA") {
      HTML(paste0(
        "<h4>Resumen general</h4>",
        "<p>Los resultados para la elección extraordinaria de <strong>", titulo_analisis, " </strong> del año ", 
        input$year, " ", filtro_geo,
        " muestran un total de <strong>", total_formateado, " votos</strong>.</p>"
      ))
    } else {
      HTML(paste0(
        "<h4>Resumen general</h4>",
        "<p>Los resultados para la elección de <strong>", titulo_analisis, " </strong> del año ", 
        input$year, " ", filtro_geo,
        " muestran un total de <strong>", total_formateado, " votos</strong>.</p>"
      ))
    }
  })
  
  output$`text_analysis-texto_resultados_partido_fed` <- renderUI({
    req(input$year, input$cargo)
    datos <- datos_columnas()$datos
    if (nrow(datos) == 0) {
      return(NULL)
    }
    
    # Obtener TODOS los partidos que compitieron en esta elección (año + cargo)
    key <- paste(input$year, input$cargo, sep = "_")
    columnas_partidos_todos <- partidos_mapping[[key]]
    if (is.null(columnas_partidos_todos) || length(columnas_partidos_todos) == 0) {
      return(NULL)
    }
    
    # Calcular el total de votos de todos los partidos (denominador correcto)
    total_votos_todos <- sum(datos[, intersect(columnas_partidos_todos, colnames(datos))], na.rm = TRUE)
    if (total_votos_todos == 0) {
      return(NULL)
    }
    
    # Calcular votos y porcentajes para todos los partidos
    datos_totales <- datos %>%
      select(all_of(intersect(columnas_partidos_todos, colnames(datos)))) %>%
      summarize_all(sum, na.rm = TRUE) %>%
      pivot_longer(cols = everything(), names_to = "partido", values_to = "votos") %>%
      mutate(porcentaje = (votos / total_votos_todos) * 100) %>%
      arrange(desc(votos))
    
    # Tomar los 3 primeros
    principales <- datos_totales %>% 
      slice_head(n = 3) %>%
      mutate(porcentaje_texto = sprintf("%.2f%%", porcentaje))
    
    # Calcular diferencias
    diferencia_1_2 <- principales$porcentaje[1] - principales$porcentaje[2]
    diferencia_2_3 <- principales$porcentaje[2] - principales$porcentaje[3]
    
    # Formatear diferencias
    diferencia_1_2_texto <- sprintf("%.2f", diferencia_1_2)
    diferencia_2_3_texto <- sprintf("%.2f", diferencia_2_3)
    
    # Generar texto final
    texto_principales <- paste0(
      "<h4>Fuerza partidista</h4>",
      "<p>La diferencia entre el primer lugar (", principales$partido[1], ": ", 
      principales$porcentaje_texto[1], ") y el segundo (", principales$partido[2], ": ",
      principales$porcentaje_texto[2], ") fue de <strong>", diferencia_1_2_texto, 
      " puntos porcentuales</strong>; y la diferencia entre este y el tercer lugar (", 
      principales$partido[3], ": ", principales$porcentaje_texto[3], ") fue de <strong>", 
      diferencia_2_3_texto, " puntos porcentuales</strong>.</p>"
    )
    
    HTML(texto_principales)
  })
  
  output$`text_analysis-texto_participacion_electoral_fed` <- renderUI({
    req(input$year, input$cargo, input$tipo_eleccion)
    
    is_voto_extranjero <- input$municipio == "VOTO EN EL EXTRANJERO" || 
      (length(input$seccion) == 1 && input$seccion == "0") ||
      grepl("VOTO EN EL EXTRANJERO", input$cabecera, ignore.case = TRUE)
    
    if (is_voto_extranjero) {
      datos <- cargar_datos(input$year, input$cargo, input$tipo_eleccion, 
                            input$estado, input$cabecera, input$municipio, 
                            input$seccion)$datos
      datos_total <- cargar_datos(input$year, input$cargo, input$tipo_eleccion, 
                                  "Nacional", "Todos", "Todos", "Todas")$datos
      
      if (nrow(datos) == 0) {
        return(NULL)
      }
      
      key <- paste(input$year, input$cargo, sep = "_")
      columnas_partidos <- partidos_mapping[[key]]
      
      total_votos_extranjero <- sum(datos[, columnas_partidos], na.rm = TRUE)
      
      cargo_formateado <- switch(input$cargo,
                                 "DIPUTACION FEDERAL" = "Diputación Federal",
                                 "SENADURIA" = "Senaduría",
                                 "PRESIDENCIA" = "Presidencia",
                                 input$cargo)
      
      format_porcentaje_adaptativo <- function(porcentaje) {
        if (porcentaje < 0.01) {
          sprintf("%.3f%%", porcentaje)
        } else if (porcentaje < 0.1) {
          sprintf("%.2f%%", porcentaje)
        } else {
          sprintf("%.2f%%", porcentaje)
        }
      }
      
      if (input$estado == "Nacional") {
        total_votos_nacional <- sum(datos_total[, columnas_partidos], na.rm = TRUE)
        porcentaje <- (total_votos_extranjero / total_votos_nacional) * 100
        
        texto_extranjero <- paste0(
          "<h4>Votación en el extranjero</h4>",
          "<p>El voto de mexicanos en el extranjero representa <strong>", 
          format_porcentaje_adaptativo(porcentaje), 
          " (", format(total_votos_extranjero, big.mark = ","), " votos)</strong> del total de votos recibidos para ",
          cargo_formateado,
          " a nivel nacional (", format(total_votos_nacional, big.mark = ","), " votos).</p>"
        )
      } else if (input$cabecera == "Todos" || grepl("VOTO EN EL EXTRANJERO", input$cabecera, ignore.case = TRUE)) {
        datos_estado <- datos_total %>% 
          filter(estado == input$estado)
        total_votos_estatal <- sum(datos_estado[, columnas_partidos], na.rm = TRUE)
        porcentaje <- (total_votos_extranjero / total_votos_estatal) * 100
        
        texto_extranjero <- paste0(
          "<h4>Votación en el extranjero</h4>",
          "<p>El voto de mexicanos en el extranjero en ", input$estado, 
          " representa <strong>", format_porcentaje_adaptativo(porcentaje), 
          " (", format(total_votos_extranjero, big.mark = ","), " votos)</strong> del total de votos recibidos para ",
          cargo_formateado,
          " a nivel estatal (", format(total_votos_estatal, big.mark = ","), " votos).</p>"
        )
      } else {
        datos_distrito <- datos_total %>% 
          filter(estado == input$estado, cabecera == input$cabecera)
        total_votos_distrito <- sum(datos_distrito[, columnas_partidos], na.rm = TRUE)
        porcentaje <- (total_votos_extranjero / total_votos_distrito) * 100
        
        texto_extranjero <- paste0(
          "<h4>Votación en el extranjero</h4>",
          "<p>El voto de mexicanos en el extranjero en el distrito ", input$cabecera, 
          " representa <strong>", format_porcentaje_adaptativo(porcentaje), 
          " (", format(total_votos_extranjero, big.mark = ","), " votos)</strong> del total de votos recibidos para ",
          cargo_formateado,
          " en el distrito (", format(total_votos_distrito, big.mark = ","), " votos).</p>"
        )
      }
      
      return(HTML(texto_extranjero))
    }
    
    datos_orig <- cargar_datos(input$year, input$cargo, input$tipo_eleccion, 
                               "Nacional", "Todos", "Todos", "Todas")$datos
    
    if (nrow(datos_orig) == 0) {
      return(NULL)
    }
    
    calcular_promedio_participacion <- function(df) {
      mean(df$part_ciud, na.rm = TRUE)
    }
    
    datos_nacional <- datos_orig %>% 
      filter(!is.na(part_ciud))
    participacion_nacional <- round(calcular_promedio_participacion(datos_nacional), 2)
    
    texto_participacion <- paste0(
      "<h4>Participación electoral</h4>"
    )
    
    mostrar_participacion_nacional <- TRUE
    
    if (input$tipo_eleccion == "EXTRAORDINARIA") {
      if (input$cargo == "SENADURIA") {
        mostrar_participacion_nacional <- FALSE
      } else if (input$cargo == "DIPUTACION FEDERAL" || input$cargo == "PRESIDENCIA") {
        mostrar_participacion_nacional <- FALSE
      }
    }
    
    if (input$estado == "Nacional" || (mostrar_participacion_nacional && input$estado != "Nacional")) {
      texto_participacion <- paste0(
        texto_participacion,
        "<p>La participación electoral promedio, a nivel nacional, fue de <strong>", 
        participacion_nacional, "%</strong>.</p>"
      )
    }
    
    if (input$estado != "Nacional") {
      datos_estado <- datos_orig %>% 
        filter(estado == input$estado, !is.na(part_ciud))
      
      participacion_estado <- round(calcular_promedio_participacion(datos_estado), 2)
      
      texto_participacion <- paste0(
        texto_participacion,
        "<p>La participación electoral promedio en ", input$estado, " fue de <strong>", 
        participacion_estado, "%</strong>.</p>"
      )
      
      if (input$cabecera != "Todos") {
        datos_distrito <- datos_orig %>% 
          filter(estado == input$estado, cabecera == input$cabecera, !is.na(part_ciud))
        
        if (nrow(datos_distrito) > 0) {
          participacion_distrito <- round(calcular_promedio_participacion(datos_distrito), 2)
          
          texto_participacion <- paste0(
            texto_participacion,
            "<p>La participación electoral promedio en el distrito ", input$cabecera, 
            " fue de <strong>", participacion_distrito, "%</strong>.</p>"
          )
        }
        
        if (input$municipio != "Todos") {
          datos_municipio <- datos_orig %>% 
            filter(estado == input$estado, cabecera == input$cabecera, 
                   municipio == input$municipio, !is.na(part_ciud))
          
          if (nrow(datos_municipio) > 0) {
            participacion_municipio <- round(calcular_promedio_participacion(datos_municipio), 2)
            
            texto_participacion <- paste0(
              texto_participacion,
              "<p>En tanto que la participación electoral promedio en ", input$municipio, 
              " fue de <strong>", participacion_municipio, "%</strong>.</p>"
            )
          }
          
          if (length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
            datos_seccion <- datos_orig %>% 
              filter(estado == input$estado, cabecera == input$cabecera,
                     municipio == input$municipio, seccion %in% input$seccion, 
                     !is.na(part_ciud))
            
            if (nrow(datos_seccion) > 0) {
              participacion_seccion <- round(calcular_promedio_participacion(datos_seccion), 2)
              
              texto_participacion <- paste0(
                texto_participacion,
                "<p>Y la participación electoral promedio en las secciones seleccionadas fue de <strong>", 
                participacion_seccion, "%</strong>.</p>"
              )
            }
          }
        } else {
          if (length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
            datos_seccion <- datos_orig %>% 
              filter(estado == input$estado, cabecera == input$cabecera,
                     seccion %in% input$seccion, !is.na(part_ciud))
            
            if (nrow(datos_seccion) > 0) {
              participacion_seccion <- round(calcular_promedio_participacion(datos_seccion), 2)
              
              texto_participacion <- paste0(
                texto_participacion,
                "<p>Y la participación electoral promedio en las secciones seleccionadas fue de <strong>", 
                participacion_seccion, "%</strong>.</p>"
              )
            }
          }
        }
      } else {
        if (input$municipio != "Todos") {
          datos_municipio <- datos_orig %>% 
            filter(estado == input$estado, municipio == input$municipio, !is.na(part_ciud))
          
          if (nrow(datos_municipio) > 0) {
            participacion_municipio <- round(calcular_promedio_participacion(datos_municipio), 2)
            
            texto_participacion <- paste0(
              texto_participacion,
              "<p>En tanto que la participación electoral promedio en ", input$municipio, 
              " fue de <strong>", participacion_municipio, "%</strong>.</p>"
            )
          }
          
          if (length(input$seccion) > 0 && !("Todas" %in% input$seccion)) {
            datos_seccion <- datos_orig %>% 
              filter(estado == input$estado, municipio == input$municipio,
                     seccion %in% input$seccion, !is.na(part_ciud))
            
            if (nrow(datos_seccion) > 0) {
              participacion_seccion <- round(calcular_promedio_participacion(datos_seccion), 2)
              
              texto_participacion <- paste0(
                texto_participacion,
                "<p>Y la participación electoral promedio en las secciones seleccionadas fue de <strong>", 
                participacion_seccion, "%</strong>.</p>"
              )
            }
          }
        }
      }
    }
  
  HTML(texto_participacion)
})
  
  output$`text_analysis-texto_comparacion_historica_fed` <- renderUI({
    req(input$year, input$cargo, input$tipo_eleccion)
    
    # Excluimos elecciones extraordinarias
    if (input$tipo_eleccion == "EXTRAORDINARIA") {
      return(NULL)
    }
    
    # No mostramos comparación para años anteriores
    if (as.numeric(input$year) < 2018) {
      return(NULL)
    }
    
    datos_columnas_actual <- datos_columnas()
    datos_actual <- datos_columnas_actual$datos
    
    if (nrow(datos_actual) == 0) {
      return(NULL)
    }
    
    key_actual <- paste(input$year, input$cargo, sep = "_")
    columnas_partidos_actual <- partidos_mapping[[key_actual]]
    
    # Preparamos el año de comparación
    year_comp <- as.character(as.numeric(input$year) - 6)
    key_comp <- paste(year_comp, input$cargo, sep = "_")
    
    # Verificamos que exista el mapping para el año de comparación
    if (!key_comp %in% names(partidos_mapping)) {
      return(NULL)
    }
    
    # Cargamos datos del año anterior
    datos_comp <- tryCatch({
      cargar_datos(year_comp, input$cargo, "ORDINARIA", 
                   input$estado, input$cabecera, input$municipio, 
                   input$seccion)$datos
    }, error = function(e) {
      return(NULL)
    })
    
    if (is.null(datos_comp) || nrow(datos_comp) == 0) {
      return(NULL)
    }
    
    columnas_partidos_comp <- partidos_mapping[[key_comp]]
    
    # Calculamos los totales para el año actual
    datos_totales_actual <- datos_actual %>%
      select(all_of(columnas_partidos_actual)) %>%
      summarize_all(sum, na.rm = TRUE) %>%
      pivot_longer(cols = everything(), names_to = "partido", values_to = "votos") %>%
      mutate(porcentaje = (votos / sum(votos)) * 100) %>%
      arrange(desc(votos))
    
    # Calculamos los totales para el año de comparación
    datos_totales_comp <- datos_comp %>%
      select(all_of(columnas_partidos_comp)) %>%
      summarize_all(sum, na.rm = TRUE) %>%
      pivot_longer(cols = everything(), names_to = "partido", values_to = "votos") %>%
      mutate(porcentaje = (votos / sum(votos)) * 100) %>%
      arrange(desc(votos))
    
    # Obtenemos el primer lugar actual y de comparación
    primer_lugar_actual <- datos_totales_actual %>% slice_head(n = 1)
    primer_lugar_comp <- datos_totales_comp %>% slice_head(n = 1)
    
    # Verificamos el cambio de posición
    cambio_ganador <- primer_lugar_actual$partido[1] != primer_lugar_comp$partido[1]
    
    # Obtenemos posición del partido ganador actual en la elección anterior
    if (cambio_ganador) {
      posicion_anterior <- datos_totales_comp %>%
        mutate(posicion = row_number()) %>%
        filter(partido == primer_lugar_actual$partido[1]) %>%
        pull(posicion)
      
      if (length(posicion_anterior) == 0) {
        posicion_anterior <- "no aparecía entre las principales fuerzas políticas"
      } else {
        posicion_anterior <- switch(as.character(posicion_anterior),
                                    "1" = "era también el partido más votado",
                                    "2" = "ocupaba el segundo lugar",
                                    "3" = "ocupaba el tercer lugar",
                                    paste("ocupaba el lugar", posicion_anterior))
      }
    }
    
    # Obtenemos la diferencia porcentual para el partido ganador actual
    partido_ganador <- primer_lugar_actual$partido[1]
    porcentaje_actual <- round(primer_lugar_actual$porcentaje[1], 2)
    
    partido_ganador_anterior <- datos_totales_comp %>%
      filter(partido == partido_ganador)
    
    if (nrow(partido_ganador_anterior) > 0) {
      porcentaje_anterior <- round(partido_ganador_anterior$porcentaje[1], 2)
      diferencia <- round(porcentaje_actual - porcentaje_anterior, 2)
      
      tendencia <- if (diferencia > 0) {
        paste("un aumento de", abs(diferencia), "puntos porcentuales")
      } else if (diferencia < 0) {
        paste("una disminución de", abs(diferencia), "puntos porcentuales")
      } else {
        "se mantuvo sin cambios"
      }
    } else {
      tendencia <- "no estaba presente en la elección anterior para hacer una comparación"
    }
    
    # Generamos el texto de comparación
    if (cambio_ganador) {
      texto_comparacion <- paste0(
        "<h4>Comparación histórica</h4>",
        "<p>Respecto a la elección de ", year_comp, " se observa un cambio en las preferencias electorales. ",
        "Mientras que en ", year_comp, " el partido con mayor votación fue <strong>", primer_lugar_comp$partido[1], 
        "</strong> (con ", round(primer_lugar_comp$porcentaje[1], 2), "% de los votos), ",
        "en ", input$year, " el partido más votado es <strong>", partido_ganador, 
        "</strong> (con ", porcentaje_actual, "% de los votos).</p>",
        "<p>El actual partido ganador, ", partido_ganador, ", en la elección de ", year_comp, " ", 
        posicion_anterior, ". En términos de porcentaje de votación, ", tendencia, ".</p>"
      )
    } else {
      texto_comparacion <- paste0(
        "<h4>Comparación histórica</h4>",
        "<p>Respecto a la elección de ", year_comp, ", el partido <strong>", partido_ganador, 
        "</strong> se mantiene como la fuerza política más votada. ",
        "En cuanto a su porcentaje de votación, pasó de ", porcentaje_anterior, "% en ", year_comp, 
        " a ", porcentaje_actual, "% en ", input$year, ", lo que representa ", tendencia, ".</p>"
      )
    }
    
    HTML(texto_comparacion)
  })
  
  }
  