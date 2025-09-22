# modules/elecciones_federales_ui

elecciones_federales_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar_panel"),
        selectInput(ns("year"), "Año de elección:", 
                    choices = c(2006, 2009, 2012, 2015, 2018, 2021, 2023), 
                    selected = 2021),
        selectInput(ns("cargo"), "Cargo de elección:", 
                    choices = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"), 
                    selected = "DIPUTACION FEDERAL"),
        tags$hr(),
        selectInput(ns("estado"), "Estado:", 
                    choices = c("Nacional")),
        selectizeInput(ns("partidos"), "Partidos, coaliciones o candidaturas:", 
                       choices = c("Todos"), 
                       multiple = TRUE, 
                       selected = "Todos",
                       options = list(
                         placeholder = "Selecciona partidos o coaliciones",
                         plugins = list("remove_button"),
                         maxItems = NULL
                       )),
        radioButtons(ns("tipo_eleccion"), "Tipo de elección:", 
                     choices = c("ORDINARIA", "EXTRAORDINARIA", "AMBAS"), 
                     selected = "AMBAS"),
        radioButtons(ns("principio_electoral"), "Principio electoral:", 
                     choices = c("MAYORÍA RELATIVA", "REPRESENTACIÓN PROPORCIONAL"), 
                     selected = "MAYORÍA RELATIVA"),
        selectInput(ns("cabecera"), "Distrito Electoral:", 
                    choices = c("Todos"), selected = "Todos"),
        selectizeInput(ns("municipio"), "Municipio:", 
                       choices = c("Todos"), selected = "Todos"),
        selectizeInput(ns("seccion"), "Sección Electoral:", 
                       choices = c("Todas"), selected = "Todas", multiple = TRUE),
        tags$hr(),
        actionButton(inputId = ns("reset_config"), 
                     label = "Restablecer consulta", 
                     class = "btn-primary"),
        downloadButton(ns("download_csv"), "Descargar CSV", class = "btn-primary")
      ),
      mainPanel(
        # Gráfico de barras
        fluidRow(
          column(12, uiOutput(ns("main-plot_container")))
        ),
        
        # Título del nuevo gráfico
        fluidRow(
          column(12, h3("Participación y Abstención Electoral", align = "center"))
        ),
        
        # Gráfico de participación y abstención
        fluidRow(
          column(12, 
                 div(style = "text-align: center; display: flex; justify-content: center;",
                     plotlyOutput(ns("main-participacion_plot"), width = "100%", height = "432px"))
          )
        ),
        
        # DataTable
        fluidRow(
          column(12, 
                 h3("Data Table", align = "center", style = "margin-top: 40px;"),
                 DTOutput(ns("main-table_data"))
          )
        )
      )
    ),
    
    # Botón para alternar sidebar derecho
    div(class = "toggle-container",
        actionButton(inputId = ns("toggle-sidebar-federales"), 
                     label = ">>", 
                     class = "toggle-sidebar-btn", 
                     `data-sidebar-id` = ns("sidebar-right-federales"))
    ),
    
    # Sidebar derecho: análisis textual
    div(id = ns("sidebar-right-federales"), class = "sidebar-right",
        uiOutput(ns("text_analysis-titulo_analisis_fed")),
        uiOutput(ns("text_analysis-alcance_analisis_fed")),
        div(class = "sidebar-section",
            uiOutput(ns("text_analysis-texto_resumen_general_fed"))
        ),
        div(class = "sidebar-section",
            uiOutput(ns("text_analysis-texto_resultados_partido_fed"))
        ),
        div(class = "sidebar-section",
            uiOutput(ns("text_analysis-texto_participacion_electoral_fed"))
        )
    )
  )
}