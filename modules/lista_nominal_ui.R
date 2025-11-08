# modules/lista_nominal_ui.R
lista_nominal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar_panel"),
        width = 4,
        
        radioButtons(
          ns("tipo_corte"), 
          "Tipo de datos:",
          choices = c("Histórico (Mensual)" = "historico", 
                      "Semanal (Detallado)" = "semanal"),
          selected = "historico"
        ),
        uiOutput(ns("info_tipo_corte")),
        tags$hr(),
        
        # ========== NUEVO: SELECTOR NACIONAL/EXTRANJERO ==========
        radioButtons(
          ns("ambito_datos"), 
          "Ámbito de datos:",
          choices = c("Nacional" = "nacional", 
                      "Extranjero" = "extranjero"),
          selected = "nacional",
          inline = TRUE
        ),
        tags$hr(),
        # ========== FIN SELECTOR NACIONAL/EXTRANJERO ==========
        tags$small(
          style = "color: #000; display: block; font-weight: bold; margin-bottom: 8px; text-align: center;",
          "Configura los filtros y presiona Consultar para actualizar"
        ),
        selectInput(ns("year"), "Año:", choices = NULL, selected = NULL),
        selectInput(ns("date"), "Fecha de corte:", choices = NULL, selected = NULL),
        uiOutput(ns("info_fecha")),
        selectInput(ns("entidad"), "Entidad:", choices = c("Nacional"), selected = "Nacional"),
        conditionalPanel(
          condition = "input.entidad != 'Nacional'",
          ns = ns,
          selectInput(ns("distrito"), "Distrito Electoral:", choices = c("Todos"), selected = "Todos"),
          selectInput(ns("municipio"), "Municipio:", choices = c("Todos"), selected = "Todos"),
          selectizeInput(
            ns("seccion"), 
            "Sección Electoral:", 
            choices = c("Todas"), 
            selected = "Todas", 
            multiple = TRUE,
            options = list(
              placeholder = "Selecciona una o más secciones",
              plugins = list("remove_button"),
              maxItems = NULL
            )
          )
        ),
        tags$hr(),
        # Selector de desglose SOLO para datos semanales
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          uiOutput(ns("selector_desglose"))
        ),
        
        # ========== BOTÓN CONSULTAR ==========
        actionButton(
          ns("btn_consultar"), 
          "Consultar", 
          icon = icon("search"),
          class = "btn-primary",
          style = "width: 100%; margin-bottom: 10px; font-weight: bold; font-size: 16px;"
        ),
        tags$hr(),
        
        # ========== FIN BOTÓN CONSULTAR ==========
        
        actionButton(ns("reset_config"), "Restablecer consulta", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
        downloadButton(ns("download_csv"), "Descargar CSV", class = "btn-primary")
      ),
      
      mainPanel(
        width = 8,
        
        # ========== GRÁFICAS PARA HISTÓRICOS ==========
        conditionalPanel(
          condition = "input.tipo_corte == 'historico'",
          ns = ns,
          
          # ========== GRÁFICAS PARA AÑO ACTUAL (1, 2, 3) ==========
          # Mostrar cuando: btn_consultar == 0 (carga inicial) O cuando year == año actual
          conditionalPanel(
            condition = "input.btn_consultar == 0 || input.year == new Date().getFullYear().toString()",
            ns = ns,
            
            # Gráfica 1: Evolución mensual año actual + Proyección
            fluidRow(
              column(12, 
                     div(class = "plot-container",
                         style = "height: 450px; margin-bottom: 10px;",
                         shinycssloaders::withSpinner(
                           plotlyOutput(ns("grafico_evolucion_2025"), width = "100%", height = "450px"),
                           type = 6,
                           color = "#44559B",
                           size = 1
                         )
                     ),
                     div(
                       style = "display: flex; justify-content: center; align-items: center; gap: 10px; margin-bottom: 20px;",
                       tags$span(
                         style = "color: #666; font-size: 12px;",
                         "Información adicional:"
                       ),
                       actionButton(
                         ns("info_grafica1"),
                         label = "Metodología de Proyección",
                         icon = icon("info-circle"),
                         class = "btn-sm btn-outline-info",
                         style = "font-size: 12px; padding: 4px 12px; border-radius: 15px; cursor: pointer;",
                         title = "Ver metodología de proyección"
                       )
                     )
              )
            ),
            
            # Gráfica 2: Evolución anual (2017-2025)
            fluidRow(
              column(12, 
                     div(class = "plot-container",
                         style = "height: 450px; margin-bottom: 30px;",
                         shinycssloaders::withSpinner(
                           plotlyOutput(ns("grafico_evolucion_anual"), width = "100%", height = "450px"),
                           type = 6,
                           color = "#44559B",
                           size = 1
                         )
                     )
              )
            ),
            
            # Gráfica 3: Evolución anual + Desglose por sexo
            fluidRow(
              column(12, 
                     div(class = "plot-container",
                         style = "height: 450px; margin-bottom: 30px;",
                         shinycssloaders::withSpinner(
                           plotlyOutput(ns("grafico_evolucion_anual_sexo"), width = "100%", height = "450px"),
                           type = 6,
                           color = "#44559B",
                           size = 1
                         )
                     )
              )
            )
          ),
          
          # ========== GRÁFICAS PARA AÑO ANTERIOR (4, 5) ==========
          # Mostrar cuando: btn_consultar > 0 (consultó) Y year != año actual
          conditionalPanel(
            condition = "input.btn_consultar > 0 && input.year != new Date().getFullYear().toString()",
            ns = ns,
            
            # Gráfica 4: Evolución mensual del año seleccionado
            fluidRow(
              column(12, 
                     div(class = "plot-container",
                         style = "height: 450px; margin-bottom: 30px;",
                         shinycssloaders::withSpinner(
                           plotlyOutput(ns("grafico_evolucion_year"), width = "100%", height = "450px"),
                           type = 6,
                           color = "#44559B",
                           size = 1
                         )
                     )
              )
            ),
            
            # Gráfica 5: Evolución mensual del año seleccionado + Desglose por sexo
            fluidRow(
              column(12, 
                     div(class = "plot-container",
                         style = "height: 450px; margin-bottom: 30px;",
                         shinycssloaders::withSpinner(
                           plotlyOutput(ns("grafico_evolucion_year_sexo"), width = "100%", height = "450px"),
                           type = 6,
                           color = "#44559B",
                           size = 1
                         )
                     )
              )
            )
          )
        ),
        
        # ========== GRÁFICAS PARA SEMANALES (CON SPINNERS) ==========
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          
          # Gráfico de barras actual
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px;",
                       shinycssloaders::withSpinner(
                         uiOutput(ns("main-plot_container")),
                         type = 6,
                         color = "#C0311A",
                         size = 1
                       )
                   )
            )
          ),
          
          # Gráfico de participación actual
          fluidRow(
            column(12,
                   div(class = "participacion-container",
                       style = "height: 500px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                       shinycssloaders::withSpinner(
                         plotlyOutput(ns("main-tasa_inclusion_plot"), width = "100%", height = "432px"),
                         type = 6,
                         color = "#4CAF50",
                         size = 1
                       )
                   )
            )
          )
        ),
        
        # ========== DATATABLE (CON SPINNER) ==========
        fluidRow(
          column(12, 
                 h3("Data Table", align = "center", style = "margin-top: 40px;"),
                 shinycssloaders::withSpinner(
                   DTOutput(ns("main-table_data")),
                   type = 6,
                   color = "#44559B",
                   size = 0.8
                 )
          )
        )
      )
    ),
    
    div(class = "toggle-container",
        actionButton(
          inputId = ns("toggle-sidebar-lista"), 
          label = ">>", 
          class = "toggle-sidebar-btn", 
          `data-sidebar-id` = ns("sidebar-right-lista")
        )
    ),
    
    div(
      id = ns("sidebar-right-lista"), 
      class = "sidebar-right",
      uiOutput(ns("text_analysis-titulo_lista")),
      uiOutput(ns("text_analysis-alcance_lista")),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-resumen_general_lista"))),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-demografia_lista"))),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-comparacion_lista")))
    )
  )
}