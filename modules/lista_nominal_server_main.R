# modules/lista_nominal_server_main.R
# Coordinador principal que integra los módulos de gráficas y tablas

lista_nominal_server_main <- function(input, output, session, datos_columnas, combinacion_valida) {
  ns <- session$ns
  
  # ========== CONTENEDOR DEL GRÁFICO PRINCIPAL ==========
  output$`main-plot_container` <- renderUI({
    plotlyOutput(session$ns("main-grafico_barras"), width = "100%", height = "450px")
  })
  
  # ========== GRÁFICO PRINCIPAL ==========
  output$`main-grafico_barras` <- renderPlotly({
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
    
    message("📊 Renderizando gráfico: ", desglose_actual)
    
    # ========== GRÁFICA POR SEXO ==========
    if (desglose_actual == "Sexo") {
      
      # Verificar si tiene desglose por sexo
      cols_sexo <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
      
      if (all(cols_sexo %in% colnames(df))) {
        # CON DESGLOSE POR SEXO (datos semanales)
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
        # SIN DESGLOSE POR SEXO - MOSTRAR TOTALES (datos históricos)
        if (all(c("padron_electoral", "lista_nominal") %in% colnames(df))) {
          
          total_padron <- sum(df$padron_electoral, na.rm = TRUE)
          total_lista <- sum(df$lista_nominal, na.rm = TRUE)
          
          message("📊 Padrón Total: ", format(total_padron, big.mark = ","))
          message("📊 Lista Total: ", format(total_lista, big.mark = ","))
          
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
    
    message("✅ Gráfico renderizado: ", desglose_actual)
    return(p)
  })
  
  # ========== GRÁFICO DE TASA DE INCLUSIÓN ==========
  output$`main-tasa_inclusion_plot` <- renderPlotly({
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
  
  # ========== DICCIONARIO DE ETIQUETAS PARA TABLA ==========
  
  etiquetas_mapeo_tabla <- list(
    "clave_entidad" = "Clave Entidad",
    "nombre_entidad" = "Entidad",
    "clave_distrito" = "Clave Distrito",
    "cabecera_distrital" = "Cabecera Distrital",
    "clave_municipio" = "Clave Municipio",
    "nombre_municipio" = "Municipio",
    "seccion" = "Sección",
    "padron_electoral" = "Padrón Electoral",
    "lista_nominal" = "Lista Nominal",
    "padron_hombres" = "Padrón Hombres",
    "padron_mujeres" = "Padrón Mujeres",
    "lista_hombres" = "Lista Hombres",
    "lista_mujeres" = "Lista Mujeres",
    "tasa_inclusion" = "Tasa de Inclusión (%)"
  )
  
  # ========== RENDERIZAR TABLA DE DATOS ==========
  
  output$`main-table_data` <- renderDT({
    req(combinacion_valida())
    datos <- datos_columnas()
    req(is.list(datos), !is.null(datos$datos))
    
    df <- datos$datos
    req(is.data.frame(df), nrow(df) > 0)
    
    columnas_base <- c("nombre_entidad", "seccion")
    
    if ("cabecera_distrital" %in% colnames(df)) {
      columnas_base <- c(columnas_base[1], "cabecera_distrital", columnas_base[-1])
    }
    if ("nombre_municipio" %in% colnames(df)) {
      columnas_base <- c(columnas_base, "nombre_municipio")
    }
    
    columnas_principales <- c("padron_electoral", "lista_nominal", "tasa_inclusion")
    
    desglose_actual <- input$desglose %||% "Sexo"
    
    columnas_desglose <- c()
    
    if (desglose_actual == "Sexo") {
      cols_sexo <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
      columnas_desglose <- cols_sexo[cols_sexo %in% colnames(df)]
    } else if (desglose_actual == "Rango de Edad") {
      cols_edad <- grep("^(padron|lista)_\\d+", colnames(df), value = TRUE, ignore.case = TRUE)
      columnas_desglose <- cols_edad
    } else if (desglose_actual == "Entidad de Origen") {
      cols_origen <- grep("^(pad|ln)_[A-Z]", colnames(df), value = TRUE, ignore.case = TRUE)
      columnas_desglose <- cols_origen
    }
    
    columnas_seleccionadas <- c(columnas_base, columnas_principales, columnas_desglose)
    columnas_seleccionadas <- columnas_seleccionadas[columnas_seleccionadas %in% colnames(df)]
    
    if (length(columnas_seleccionadas) == 0) {
      message("⚠️ No hay columnas válidas para mostrar en la tabla")
      return(datatable(
        data.frame(Mensaje = "No hay datos disponibles"),
        options = list(pageLength = 10)
      ))
    }
    
    datos_tabla <- df[, columnas_seleccionadas, drop = FALSE]
    
    nombres_columnas <- sapply(columnas_seleccionadas, function(col) {
      if (col %in% names(etiquetas_mapeo_tabla)) {
        etiquetas_mapeo_tabla[[col]]
      } else {
        nombre_limpio <- gsub("_", " ", col)
        nombre_limpio <- gsub("padron", "Padrón", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- gsub("lista", "Lista", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- gsub("hombres", "H", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- gsub("mujeres", "M", nombre_limpio, ignore.case = TRUE)
        nombre_limpio <- tools::toTitleCase(nombre_limpio)
        nombre_limpio
      }
    })
    
    if (length(nombres_columnas) != length(columnas_seleccionadas)) {
      message("⚠️ Error: Longitud de nombres no coincide")
    }
    
    message("🔍 Columnas tabla: ", paste(columnas_seleccionadas, collapse = ", "))
    message("🔍 Nombres aplicados: ", paste(nombres_columnas, collapse = ", "))
    
    colnames(datos_tabla) <- nombres_columnas
    
    if ("Tasa de Inclusión (%)" %in% colnames(datos_tabla)) {
      tasa_numeric <- suppressWarnings(as.numeric(datos_tabla[["Tasa de Inclusión (%)"]]))
      if (!all(is.na(tasa_numeric))) {
        datos_tabla[["Tasa de Inclusión (%)"]] <- sprintf("%.2f%%", tasa_numeric)
      } else {
        datos_tabla[["Tasa de Inclusión (%)"]] <- "NA%"
      }
    }
    
    columnas_con_comas <- c(
      "Padrón Electoral", "Lista Nominal", 
      "Padrón Hombres", "Padrón Mujeres", 
      "Lista Hombres", "Lista Mujeres"
    )
    
    cols_numericas_adicionales <- grep("Padrón|Lista|PAD|LN", colnames(datos_tabla), value = TRUE)
    columnas_con_comas <- unique(c(columnas_con_comas, cols_numericas_adicionales))
    columnas_con_comas <- columnas_con_comas[columnas_con_comas %in% colnames(datos_tabla)]
    
    indices_con_comas <- which(colnames(datos_tabla) %in% columnas_con_comas) - 1
    indices_con_comas <- indices_con_comas[!is.na(indices_con_comas) & indices_con_comas >= 0]
    
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
        "Fuente: INE. Padrón Electoral y Lista Nominal de Electores."
      ),
      options = list(
        pageLength = 10,
        lengthMenu = list(c(10, 25, 50, 100, -1), c("10", "25", "50", "100", "Todos")),
        dom = 'lfrtip',
        columnDefs = column_defs,
        scrollX = TRUE
      ),
      escape = FALSE
    )
    
    message("✅ DataTable renderizado correctamente")
    dt
  })
  
  # ========== DESCARGA CSV ==========
  
  output$download_csv <- downloadHandler(
    filename = function() {
      tipo <- if (input$tipo_corte == "historico") "historico" else "semanal"
      fecha_str <- format(as.Date(input$date), "%Y%m%d")
      entidad_str <- gsub(" ", "_", input$entidad)
      paste0("lista_nominal_", tipo, "_", fecha_str, "_", entidad_str, ".csv")
    },
    content = function(file) {
      datos <- datos_columnas()
      req(is.list(datos), !is.null(datos$datos))
      
      df <- datos$datos
      req(is.data.frame(df), nrow(df) > 0)
      
      columnas_base <- c("nombre_entidad", "seccion")
      
      if ("cabecera_distrital" %in% colnames(df)) {
        columnas_base <- c(columnas_base[1], "cabecera_distrital", columnas_base[-1])
      }
      if ("nombre_municipio" %in% colnames(df)) {
        columnas_base <- c(columnas_base, "nombre_municipio")
      }
      
      columnas_principales <- c("padron_electoral", "lista_nominal", "tasa_inclusion")
      
      desglose_actual <- input$desglose %||% "Sexo"
      columnas_desglose <- c()
      
      if (desglose_actual == "Sexo") {
        cols_sexo <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
        columnas_desglose <- cols_sexo[cols_sexo %in% colnames(df)]
      } else if (desglose_actual == "Rango de Edad") {
        cols_edad <- grep("^(padron|lista)_\\d+", colnames(df), value = TRUE, ignore.case = TRUE)
        columnas_desglose <- cols_edad
      } else if (desglose_actual == "Entidad de Origen") {
        cols_origen <- grep("^(pad|ln)_[A-Z]", colnames(df), value = TRUE, ignore.case = TRUE)
        columnas_desglose <- cols_origen
      }
      
      columnas_seleccionadas <- c(columnas_base, columnas_principales, columnas_desglose)
      columnas_seleccionadas <- columnas_seleccionadas[columnas_seleccionadas %in% colnames(df)]
      
      datos_tabla <- df[, columnas_seleccionadas, drop = FALSE]
      
      nombres_columnas <- sapply(columnas_seleccionadas, function(col) {
        if (col %in% names(etiquetas_mapeo_tabla)) {
          etiquetas_mapeo_tabla[[col]]
        } else {
          nombre_limpio <- gsub("_", " ", col)
          nombre_limpio <- tools::toTitleCase(nombre_limpio)
          nombre_limpio
        }
      })
      
      colnames(datos_tabla) <- nombres_columnas
      
      message("🔍 Filas exportadas: ", nrow(datos_tabla))
      
      write.csv(datos_tabla, file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
      write("Fuente: INE. Padrón Electoral y Lista Nominal de Electores.", 
            file, append = TRUE)
    }
  )
  
  # ========== BOTÓN RESTABLECER ==========
  
  observeEvent(input$reset_config, {
    message("🔄 Restableciendo configuración de Lista Nominal Electoral")
    
    if (exists("LNE_CATALOG", envir = .GlobalEnv)) {
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      
      if (input$tipo_corte == "historico" && length(catalog$historico) > 0) {
        año_reciente <- format(max(catalog$historico), "%Y")
        updateSelectInput(session, "year", selected = año_reciente)
      } else if (input$tipo_corte == "semanal" && length(catalog$semanal_comun) > 0) {
        año_reciente <- format(max(catalog$semanal_comun), "%Y")
        updateSelectInput(session, "year", selected = año_reciente)
      }
    }
    
    updateRadioButtons(session, "tipo_corte", selected = "historico")
    updateSelectInput(session, "entidad", selected = "Nacional")
    updateSelectInput(session, "distrito", selected = "Todos")
    updateSelectInput(session, "municipio", selected = "Todos")
    updateSelectInput(session, "seccion", selected = "Todas")
    
    if (!is.null(input$desglose)) {
      updateSelectInput(session, "desglose", selected = "Sexo")
    }
    
    message("✅ Configuración de Lista Nominal restablecida correctamente")
  })
  
  message("✅ Módulo lista_nominal_server_main inicializado correctamente")
}
                      
                      