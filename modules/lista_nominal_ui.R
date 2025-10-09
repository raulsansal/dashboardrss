# modules/lista_nominal_ui.R

lista_nominal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar_panel"),
        width = 3,
        
        # NUEVO: Selector de tipo de corte
        radioButtons(
          ns("tipo_corte"), 
          "Tipo de datos:",
          choices = c("Histórico (Mensual)" = "historico", 
                      "Semanal (Detallado)" = "semanal"),
          selected = "historico"
        ),
        
        # Información sobre el tipo seleccionado
        uiOutput(ns("info_tipo_corte")),
        
        tags$hr(),
        
        # Selector de año (filtrado por tipo de corte)
        selectInput(
          ns("year"), 
          "Año:", 
          choices = NULL,  # Se actualiza dinámicamente
          selected = NULL
        ),
        
        # Selector de fecha (filtrado por año y tipo)
        selectInput(
          ns("date"), 
          "Fecha de corte:", 
          choices = NULL,  # Se actualiza dinámicamente
          selected = NULL
        ),
        
        # Mostrar información de la fecha seleccionada
        uiOutput(ns("info_fecha")),
        
        tags$hr(),
        
        # Filtros geográficos
        selectInput(
          ns("entidad"), 
          "Entidad:", 
          choices = c("Nacional"), 
          selected = "Nacional"
        ),
        
        conditionalPanel(
          condition = "input.entidad != 'Nacional'",
          ns = ns,
          selectInput(
            ns("distrito"), 
            "Distrito Electoral:", 
            choices = c("Todos"), 
            selected = "Todos"
          ),
          selectInput(
            ns("municipio"), 
            "Municipio:", 
            choices = c("Todos"), 
            selected = "Todos"
          ),
          selectInput(
            ns("seccion"), 
            "Sección Electoral:", 
            choices = c("Todas"), 
            selected = "Todas"
          )
        ),
        
        tags$hr(),
        
        # Desglose (condicional según tipo de corte)
        uiOutput(ns("selector_desglose")),
        
        tags$hr(),
        
        # Botón restablecer
        actionButton(
          inputId = ns("reset_config"), 
          label = "Restablecer consulta", 
          class = "btn-primary",
          style = "width: 100%;"
        )
      ),
      
      mainPanel(
        width = 9,
        
        # Encabezado con información contextual
        fluidRow(
          column(12,
                 uiOutput(ns("encabezado_principal"))
          )
        ),
        
        tags$hr(),
        
        # Tabla de resumen
        fluidRow(
          column(12, 
                 h4("Resumen de Datos", style = "margin-top: 20px;"),
                 div(
                   class = "plot-container",
                   style = "height: 450px; overflow-y: auto;",
                   DTOutput(ns("summary_table"))
                 )
          )
        ),
        
        tags$hr(),
        
        # Gráfico de tendencias
        fluidRow(
          column(12,
                 h4("Visualización", style = "margin-top: 20px;"),
                 div(
                   class = "participacion-container",
                   style = "height: 500px; display: flex; flex-direction: column; align-items: center; justify-content: center;",
                   plotlyOutput(ns("trend_plot"), width = "100%", height = "432px")
                 )
          )
        )
      )
    ),
    
    # Botón para alternar sidebar derecho
    div(class = "toggle-container",
        actionButton(
          inputId = ns("toggle-sidebar-lista"), 
          label = ">>", 
          class = "toggle-sidebar-btn", 
          `data-sidebar-id` = ns("sidebar-right-lista")
        )
    ),
    
    # Sidebar derecho: análisis textual
    div(
      id = ns("sidebar-right-lista"), 
      class = "sidebar-right",
      uiOutput(ns("text_analysis-titulo_lista")),
      uiOutput(ns("text_analysis-alcance_lista")),
      div(class = "sidebar-section",
          uiOutput(ns("text_analysis-resumen_general_lista"))
      ),
      div(class = "sidebar-section",
          uiOutput(ns("text_analysis-demografia_lista"))
      ),
      div(class = "sidebar-section",
          uiOutput(ns("text_analysis-comparacion_lista"))
      )
    )
  )
}