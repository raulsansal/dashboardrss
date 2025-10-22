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
        selectInput(ns("year"), "Año:", choices = NULL, selected = NULL),
        selectInput(ns("date"), "Fecha de corte:", choices = NULL, selected = NULL),
        uiOutput(ns("info_fecha")),
        tags$hr(),
        selectInput(ns("entidad"), "Entidad:", choices = c("Nacional"), selected = "Nacional"),
        conditionalPanel(
          condition = "input.entidad != 'Nacional'",
          ns = ns,
          selectInput(ns("distrito"), "Distrito Electoral:", choices = c("Todos"), selected = "Todos"),
          selectInput(ns("municipio"), "Municipio:", choices = c("Todos"), selected = "Todos"),
          # ========== CORREGIDO: selectInput → selectizeInput ==========
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
          # ========== FIN DE LA CORRECCIÓN ==========
        ),
        tags$hr(),
        # Selector de desglose SOLO para datos semanales
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          uiOutput(ns("selector_desglose"))
        ),
        tags$hr(),
        actionButton(ns("reset_config"), "Restablecer consulta", class = "btn-primary", style = "width: 100%; margin-bottom: 10px;"),
        downloadButton(ns("download_csv"), "Descargar CSV", class = "btn-primary")
      ),
      
      mainPanel(
        width = 8,
        
        # ========== GRÁFICAS PARA HISTÓRICOS (5 GRÁFICAS) ==========
        conditionalPanel(
          condition = "input.tipo_corte == 'historico'",
          ns = ns,
          
          # Gráfica 1: Evolución mensual 2025 + Proyección
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px; margin-bottom: 30px;",
                       plotlyOutput(ns("grafico_evolucion_2025"), width = "100%", height = "450px")
                   )
            )
          ),
          
          # Gráfica 2: Evolución anual (2017-2025) - Último mes de cada año
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px; margin-bottom: 30px;",
                       plotlyOutput(ns("grafico_evolucion_anual"), width = "100%", height = "450px")
                   )
            )
          ),
          
          # Gráfica 3: Evolución anual + Desglose por sexo
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px; margin-bottom: 30px;",
                       plotlyOutput(ns("grafico_evolucion_anual_sexo"), width = "100%", height = "450px")
                   )
            )
          ),
          
          # Gráfica 4: Evolución mensual del año seleccionado
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px; margin-bottom: 30px;",
                       plotlyOutput(ns("grafico_evolucion_year"), width = "100%", height = "450px")
                   )
            )
          ),
          
          # Gráfica 5: Evolución mensual del año seleccionado + Desglose por sexo
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px; margin-bottom: 30px;",
                       plotlyOutput(ns("grafico_evolucion_year_sexo"), width = "100%", height = "450px")
                   )
            )
          )
        ),
        
        # ========== GRÁFICAS PARA SEMANALES (MANTENER LAS ACTUALES) ==========
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          
          # Gráfico de barras actual
          fluidRow(
            column(12, 
                   div(class = "plot-container",
                       style = "height: 450px;",
                       uiOutput(ns("main-plot_container"))
                   )
            )
          ),
          
          # Gráfico de participación actual
          fluidRow(
            column(12,
                   div(class = "participacion-container",
                       style = "height: 500px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                       plotlyOutput(ns("main-tasa_inclusion_plot"), width = "100%", height = "432px")
                   )
            )
          )
        ),
        
        # ========== DATATABLE (COMÚN PARA AMBOS TIPOS) ==========
        fluidRow(
          column(12, 
                 h3("Data Table", align = "center", style = "margin-top: 40px;"),
                 DTOutput(ns("main-table_data"))
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