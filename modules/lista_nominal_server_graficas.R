# modules/lista_nominal_server_graficas.R
# Módulo especializado en la generación de gráficas para Lista Nominal Electoral

lista_nominal_server_graficas <- function(input, output, session, datos_columnas, combinacion_valida) {
  
  # ========== GRÁFICO PRINCIPAL (DIRECTO, SIN CONTENEDOR) ==========
  output$`main-grafico_barras` <- renderPlotly({
    req(combinacion_valida())
    datos <- datos_columnas()
    
    # Validar que hay datos disponibles
    if (is.null(datos) || is.null(datos$datos) || nrow(datos$datos) == 0) {
      p <- plot_ly() %>%
        layout(
          xaxis = list(visible = FALSE),
          yaxis = list(visible = FALSE),
          annotations = list(
            list(
              text = "No hay datos disponibles con los filtros seleccionados",
              xref = "paper", 
              yref = "paper",
              x = 0.5, 
              y = 0.5,
              xanchor = "center", 
              yanchor = "middle",
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
      
      # Verificar columnas necesarias
      cols_requeridas <- c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres")
      
      if (all(cols_requeridas %in% colnames(df))) {
        
        # Calcular totales
        padron_h <- sum(df$padron_hombres, na.rm = TRUE)
        padron_m <- sum(df$padron_mujeres, na.rm = TRUE)
        lista_h <- sum(df$lista_hombres, na.rm = TRUE)
        lista_m <- sum(df$lista_mujeres, na.rm = TRUE)
        
        message("📊 Padrón H: ", format(padron_h, big.mark = ","), 
                " | M: ", format(padron_m, big.mark = ","))
        message("📊 Lista H: ", format(lista_h, big.mark = ","), 
                " | M: ", format(lista_m, big.mark = ","))
        
        # Preparar datos
        datos_grafico <- data.frame(
          Categoria = rep(c("Hombres", "Mujeres"), 2),
          Tipo = rep(c("Padrón Electoral", "Lista Nominal"), each = 2),
          Cantidad = c(padron_h, padron_m, lista_h, lista_m),
          stringsAsFactors = FALSE
        )
        
        # Crear gráfico
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
              x = 0.5,
              xanchor = "center"
            ),
            xaxis = list(title = ""),
            yaxis = list(
              title = "Número de Electores",
              separatethousands = TRUE
            ),
            barmode = 'group',
            margin = list(t = 80, b = 80, l = 80, r = 50),
            legend = list(
              orientation = "h",
              xanchor = "center",
              x = 0.5,
              y = -0.15
            ),
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
        
      } else {
        # Sin datos de sexo
        p <- plot_ly() %>%
          layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE),
            annotations = list(
              list(
                text = "Datos de sexo no disponibles para este corte",
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
      
    } else if (desglose_actual == "Rango de Edad") {
      
      # ========== GRÁFICA POR EDAD ==========
      
      # Buscar columnas de edad en lista nominal
      cols_edad_lista <- grep("^lista_(\\d+|\\d+_\\d+)", colnames(df), value = TRUE, ignore.case = TRUE)
      
      if (length(cols_edad_lista) > 0) {
        
        # Extraer grupos de edad únicos
        grupos_raw <- gsub("lista_", "", cols_edad_lista, ignore.case = TRUE)
        grupos_raw <- gsub("_(hombres|mujeres|nobinario).*", "", grupos_raw, ignore.case = TRUE)
        grupos <- unique(grupos_raw)
        
        # Preparar datos
        datos_grafico <- data.frame(
          Grupo = character(),
          Lista_Nominal = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (grupo in grupos) {
          cols_grupo <- grep(paste0("^lista_", grupo, "($|_)"), colnames(df), value = TRUE, ignore.case = TRUE)
          total <- sum(df[, cols_grupo, drop = FALSE], na.rm = TRUE)
          
          # Formatear nombre
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
        
        # Ordenar por edad
        orden_edad <- c("18", "19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                        "45-49", "50-54", "55-59", "60-64", "65-y-más", "65-y-mas")
        datos_grafico$Grupo <- factor(
          datos_grafico$Grupo, 
          levels = intersect(orden_edad, datos_grafico$Grupo)
        )
        datos_grafico <- datos_grafico[order(datos_grafico$Grupo), ]
        
        # Crear gráfico
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
        # Sin datos de edad
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
        
        # Agregar por entidad
        datos_grafico <- df %>%
          group_by(Entidad = nombre_entidad) %>%
          summarise(
            Lista_Nominal = sum(lista_nominal, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(Lista_Nominal)) %>%
          head(10)
        
        datos_grafico <- as.data.frame(datos_grafico)
        
        # Crear gráfico horizontal
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
        # Sin datos de origen
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
      # Desglose no reconocido
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
    
    # Verificar que existan las columnas necesarias
    if (!all(c("padron_electoral", "lista_nominal") %in% colnames(df))) {
      return(NULL)
    }
    
    # Calcular totales
    total_padron <- sum(df$padron_electoral, na.rm = TRUE)
    total_lista <- sum(df$lista_nominal, na.rm = TRUE)
    
    if (total_padron == 0) {
      return(NULL)
    }
    
    tasa_inclusion <- round((total_lista / total_padron) * 100, 2)
    tasa_exclusion <- round(100 - tasa_inclusion, 2)
    
    # Datos para el gráfico de dona
    datos_grafico <- data.frame(
      grupo = c(
        paste0("Lista Nominal:<br>", sprintf("%.2f%%", tasa_inclusion)),
        sprintf("Diferencia: %.2f%%", tasa_exclusion)
      ),
      valor = c(tasa_inclusion, tasa_exclusion),
      stringsAsFactors = FALSE
    )
    
    # Colores
    color_lista <- "#4CAF50"
    color_diferencia <- "#FFC107"
    
    # Crear gráfico
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