# modules/lista_nominal_server_main.R

lista_nominal_server_main <- function(input, output, session, datos_columnas, combinacion_valida) {
  ns <- session$ns
  
  # ========== TABLA DE RESUMEN ==========
  
  output$summary_table <- renderDT({
    req(combinacion_valida())
    datos <- datos_columnas()
    
    if (is.null(datos) || nrow(datos$datos) == 0) {
      return(datatable(
        data.frame(Mensaje = "No hay datos disponibles con los filtros seleccionados."),
        options = list(pageLength = 10, dom = 't')
      ))
    }
    
    # Seleccionar columnas según el desglose elegido
    df <- datos$datos
    
    if (input$desglose == "Sexo") {
      # Mostrar totales por sexo
      if (all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(df))) {
        
        # Agregar datos
        resumen <- data.frame(
          Sexo = c("Hombres", "Mujeres", "No Binario", "Total"),
          `Padrón Electoral` = c(
            sum(df$padron_hombres, na.rm = TRUE),
            sum(df$padron_mujeres, na.rm = TRUE),
            sum(df$padron_nac_nobinario, na.rm = TRUE),
            sum(df$padron_electoral, na.rm = TRUE)
          ),
          `Lista Nominal` = c(
            sum(df$lista_hombres, na.rm = TRUE),
            sum(df$lista_mujeres, na.rm = TRUE),
            sum(df$lista_nac_nobinario, na.rm = TRUE),
            sum(df$lista_nominal, na.rm = TRUE)
          ),
          check.names = FALSE
        )
        
        # Calcular porcentajes
        resumen$`% del Padrón` <- sprintf("%.2f%%", (resumen$`Padrón Electoral` / resumen$`Padrón Electoral`[4]) * 100)
        resumen$`% de la Lista` <- sprintf("%.2f%%", (resumen$`Lista Nominal` / resumen$`Lista Nominal`[4]) * 100)
        resumen$`Tasa de Inclusión` <- sprintf("%.2f%%", (resumen$`Lista Nominal` / resumen$`Padrón Electoral`) * 100)
        
      } else {
        resumen <- data.frame(
          Mensaje = "Datos de sexo no disponibles para este corte."
        )
      }
      
    } else if (input$desglose == "Rango de Edad") {
      # Mostrar totales por grupo de edad
      cols_edad_padron <- grep("^padron_\\d+(_|y)", colnames(df), value = TRUE)
      cols_edad_lista <- grep("^lista_\\d+(_|y)", colnames(df), value = TRUE)
      
      if (length(cols_edad_padron) > 0 && length(cols_edad_lista) > 0) {
        
        # Extraer nombres de grupos de edad
        grupos <- gsub("padron_", "", cols_edad_padron)
        grupos <- gsub("_hombres|_mujeres|_nobinario", "", grupos)
        grupos <- unique(grupos)
        
        resumen <- data.frame(
          `Grupo de Edad` = character(),
          `Padrón Electoral` = numeric(),
          `Lista Nominal` = numeric(),
          check.names = FALSE
        )
        
        for (grupo in grupos) {
          cols_padron_grupo <- grep(paste0("^padron_", grupo, "(_|$)"), colnames(df), value = TRUE)
          cols_lista_grupo <- grep(paste0("^lista_", grupo, "(_|$)"), colnames(df), value = TRUE)
          
          total_padron <- sum(df[, cols_padron_grupo, drop = FALSE], na.rm = TRUE)
          total_lista <- sum(df[, cols_lista_grupo, drop = FALSE], na.rm = TRUE)
          
          # Formatear nombre del grupo
          nombre_grupo <- gsub("_", "-", grupo)
          nombre_grupo <- gsub("y-mas", "y más", nombre_grupo)
          nombre_grupo <- toupper(nombre_grupo)
          
          resumen <- rbind(resumen, data.frame(
            `Grupo de Edad` = nombre_grupo,
            `Padrón Electoral` = total_padron,
            `Lista Nominal` = total_lista,
            check.names = FALSE
          ))
        }
        
        # Agregar fila de totales
        resumen <- rbind(resumen, data.frame(
          `Grupo de Edad` = "TOTAL",
          `Padrón Electoral` = sum(resumen$`Padrón Electoral`),
          `Lista Nominal` = sum(resumen$`Lista Nominal`),
          check.names = FALSE
        ))
        
        # Calcular porcentajes
        total_padron <- resumen$`Padrón Electoral`[nrow(resumen)]
        total_lista <- resumen$`Lista Nominal`[nrow(resumen)]
        
        resumen$`% del Padrón` <- sprintf("%.2f%%", (resumen$`Padrón Electoral` / total_padron) * 100)
        resumen$`% de la Lista` <- sprintf("%.2f%%", (resumen$`Lista Nominal` / total_lista) * 100)
        resumen$`Tasa de Inclusión` <- sprintf("%.2f%%", (resumen$`Lista Nominal` / resumen$`Padrón Electoral`) * 100)
        
      } else {
        resumen <- data.frame(
          Mensaje = "Datos de edad no disponibles para este corte."
        )
      }
      
    } else if (input$desglose == "Entidad de Origen") {
      # Mostrar totales por entidad (solo si hay datos de origen)
      if ("nombre_entidad" %in% colnames(df)) {
        
        resumen <- df %>%
          group_by(`Entidad` = nombre_entidad) %>%
          summarise(
            `Padrón Electoral` = sum(padron_electoral, na.rm = TRUE),
            `Lista Nominal` = sum(lista_nominal, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(`Padrón Electoral`))
        
        # Calcular porcentajes
        total_padron <- sum(resumen$`Padrón Electoral`)
        total_lista <- sum(resumen$`Lista Nominal`)
        
        resumen$`% del Padrón` <- sprintf("%.2f%%", (resumen$`Padrón Electoral` / total_padron) * 100)
        resumen$`% de la Lista` <- sprintf("%.2f%%", (resumen$`Lista Nominal` / total_lista) * 100)
        resumen$`Tasa de Inclusión` <- sprintf("%.2f%%", (resumen$`Lista Nominal` / resumen$`Padrón Electoral`) * 100)
        
        resumen <- as.data.frame(resumen)
        
      } else {
        resumen <- data.frame(
          Mensaje = "Datos de origen no disponibles para este corte."
        )
      }
    }
    
    # Formatear números con comas
    if ("Padrón Electoral" %in% colnames(resumen)) {
      columnas_numericas <- c("Padrón Electoral", "Lista Nominal")
      indices_numericas <- which(colnames(resumen) %in% columnas_numericas) - 1  # 0-based para DT
      
      column_defs <- list(
        list(
          targets = indices_numericas,
          render = JS(
            "function(data, type, row) {",
            "  return type === 'display' && data != null ?",
            "    data.toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ',') : data;",
            "}"
          )
        )
      )
    } else {
      column_defs <- NULL
    }
    
    dt <- datatable(
      resumen,
      caption = htmltools::tags$caption(
        style = "caption-side: bottom; text-align: left; font-size: 10px; color: #666666; font-family: Arial, sans-serif;",
        "Fuente: INE. Padrón Electoral y Lista Nominal de Electores."
      ),
      options = list(
        pageLength = 15,
        lengthMenu = list(c(10, 15, 25, 50, -1), c("10", "15", "25", "50", "Todos")),
        dom = 'lfrtip',
        columnDefs = column_defs,
        language = list(
          search = "Buscar:",
          lengthMenu = "Mostrar _MENU_ registros",
          info = "Mostrando _START_ a _END_ de _TOTAL_ registros",
          infoEmpty = "Mostrando 0 a 0 de 0 registros",
          infoFiltered = "(filtrado de _MAX_ registros totales)",
          paginate = list(
            first = "Primero",
            last = "Último",
            `next` = "Siguiente",
            previous = "Anterior"
          )
        )
      ),
      rownames = FALSE,
      escape = FALSE
    )
    
    message("✅ Tabla de resumen renderizada correctamente")
    dt
  })
  
  # ========== GRÁFICO DE TENDENCIAS ==========
  
  output$trend_plot <- renderPlotly({
    req(combinacion_valida())
    datos <- datos_columnas()
    
    if (is.null(datos) || nrow(datos$datos) == 0) {
      return(NULL)
    }
    
    df <- datos$datos
    
    # Determinar qué graficar según el desglose
    if (input$desglose == "Sexo") {
      
      if (all(c("padron_hombres", "padron_mujeres", "lista_hombres", "lista_mujeres") %in% colnames(df))) {
        
        # Datos para gráfico de barras agrupadas
        datos_grafico <- data.frame(
          Categoría = rep(c("Hombres", "Mujeres", "No Binario"), 2),
          Tipo = rep(c("Padrón Electoral", "Lista Nominal"), each = 3),
          Cantidad = c(
            sum(df$padron_hombres, na.rm = TRUE),
            sum(df$padron_mujeres, na.rm = TRUE),
            sum(df$padron_nac_nobinario, na.rm = TRUE),
            sum(df$lista_hombres, na.rm = TRUE),
            sum(df$lista_mujeres, na.rm = TRUE),
            sum(df$lista_nac_nobinario, na.rm = TRUE)
          )
        )
        
        # Crear gráfico
        p <- plot_ly(
          data = datos_grafico,
          x = ~Categoría,
          y = ~Cantidad,
          color = ~Tipo,
          type = 'bar',
          colors = c("#44559B", "#C0311A"),
          text = ~paste0(Tipo, ": ", format(Cantidad, big.mark = ",")),
          hovertemplate = '%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = "Padrón Electoral y Lista Nominal por Sexo",
              font = list(size = 18, color = "black", family = "Arial, sans-serif")
            ),
            xaxis = list(title = ""),
            yaxis = list(title = "Cantidad de Electores"),
            barmode = 'group',
            margin = list(t = 100, b = 80, l = 80, r = 50),
            annotations = list(
              list(
                text = paste0("Fuente: INE. Corte: ", format(as.Date(input$date), "%d/%m/%Y")),
                xref = "paper", yref = "paper",
                x = 0, y = -0.15,
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif")
              )
            )
          )
        
      } else {
        return(NULL)
      }
      
    } else if (input$desglose == "Rango de Edad") {
      
      # Buscar columnas de edad
      cols_edad_lista <- grep("^lista_\\d+", colnames(df), value = TRUE)
      
      if (length(cols_edad_lista) > 0) {
        
        # Extraer grupos de edad únicos
        grupos <- gsub("lista_", "", cols_edad_lista)
        grupos <- gsub("_hombres|_mujeres|_nobinario", "", grupos)
        grupos <- unique(grupos)
        
        datos_grafico <- data.frame(
          Grupo = character(),
          Lista_Nominal = numeric(),
          stringsAsFactors = FALSE
        )
        
        for (grupo in grupos) {
          cols_grupo <- grep(paste0("^lista_", grupo, "(_|$)"), colnames(df), value = TRUE)
          total <- sum(df[, cols_grupo, drop = FALSE], na.rm = TRUE)
          
          nombre_grupo <- gsub("_", "-", grupo)
          nombre_grupo <- gsub("y-mas", "y más", nombre_grupo)
          
          datos_grafico <- rbind(datos_grafico, data.frame(
            Grupo = nombre_grupo,
            Lista_Nominal = total,
            stringsAsFactors = FALSE
          ))
        }
        
        # Ordenar por edad
        orden_edad <- c("18", "19", "20-24", "25-29", "30-34", "35-39", "40-44", 
                        "45-49", "50-54", "55-59", "60-64", "65-y-más")
        datos_grafico$Grupo <- factor(datos_grafico$Grupo, levels = orden_edad)
        datos_grafico <- datos_grafico[order(datos_grafico$Grupo), ]
        
        # Crear gráfico de barras
        p <- plot_ly(
          data = datos_grafico,
          x = ~Grupo,
          y = ~Lista_Nominal,
          type = 'bar',
          marker = list(color = "#C0311A"),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '%{x}<br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = "Lista Nominal por Grupo de Edad",
              font = list(size = 18, color = "black", family = "Arial, sans-serif")
            ),
            xaxis = list(title = "Grupo de Edad"),
            yaxis = list(title = "Cantidad de Electores"),
            margin = list(t = 100, b = 100, l = 80, r = 50),
            annotations = list(
              list(
                text = paste0("Fuente: INE. Corte: ", format(as.Date(input$date), "%d/%m/%Y")),
                xref = "paper", yref = "paper",
                x = 0, y = -0.2,
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif")
              )
            )
          )
        
      } else {
        return(NULL)
      }
      
    } else if (input$desglose == "Entidad de Origen") {
      
      if ("nombre_entidad" %in% colnames(df)) {
        
        datos_grafico <- df %>%
          group_by(Entidad = nombre_entidad) %>%
          summarise(
            Lista_Nominal = sum(lista_nominal, na.rm = TRUE),
            .groups = 'drop'
          ) %>%
          arrange(desc(Lista_Nominal)) %>%
          head(10)  # Top 10 entidades
        
        datos_grafico <- as.data.frame(datos_grafico)
        
        # Crear gráfico de barras horizontales
        p <- plot_ly(
          data = datos_grafico,
          y = ~reorder(Entidad, Lista_Nominal),
          x = ~Lista_Nominal,
          type = 'bar',
          orientation = 'h',
          marker = list(color = "#44559B"),
          text = ~paste0(format(Lista_Nominal, big.mark = ","), " electores"),
          hovertemplate = '%{y}<br>%{text}<extra></extra>'
        ) %>%
          layout(
            title = list(
              text = "Top 10 Entidades por Lista Nominal",
              font = list(size = 18, color = "black", family = "Arial, sans-serif")
            ),
            xaxis = list(title = "Cantidad de Electores"),
            yaxis = list(title = ""),
            margin = list(t = 100, b = 80, l = 150, r = 50),
            annotations = list(
              list(
                text = paste0("Fuente: INE. Corte: ", format(as.Date(input$date), "%d/%m/%Y")),
                xref = "paper", yref = "paper",
                x = 0, y = -0.15,
                xanchor = "left", yanchor = "top",
                showarrow = FALSE,
                font = list(size = 10, color = "#666666", family = "Arial, sans-serif")
              )
            )
          )
        
      } else {
        return(NULL)
      }
    }
    
    message("✅ Gráfico de tendencias renderizado correctamente")
    return(p)
  })
  
  # ========== BOTÓN RESTABLECER ==========
  
  observeEvent(input$reset_config, {
    message("🔍 Restableciendo configuración de Lista Nominal Electoral")
    
    # Restablecer año
    updateSelectInput(session, "year", selected = 2025)
    
    # Restablecer entidad
    updateSelectInput(session, "entidad", selected = "Nacional")
    
    # Restablecer distrito
    updateSelectInput(session, "distrito", selected = "Todos")
    
    # Restablecer municipio
    updateSelectInput(session, "municipio", selected = "Todos")
    
    # Restablecer sección
    updateSelectInput(session, "seccion", selected = "Todas")
    
    # Restablecer desglose
    updateSelectInput(session, "desglose", selected = "Sexo")
    
    message("✅ Configuración de Lista Nominal restablecida correctamente")
  })
  
  message("✅ Módulo lista_nominal_server_main inicializado correctamente")
}