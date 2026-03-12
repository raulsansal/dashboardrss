# modules/lista_nominal_ui.R
# Versión: 3.7 — Ocultar main-table_data en vista semanal
#
# CAMBIOS vs v3.6:
#   - main-table_data y su encabezado (main-table_header) se envuelven en
#     conditionalPanel que los oculta cuando tipo_corte == 'semanal'.
#   - En semanal, la tabla de datos es propia de cada desglose (semanal_dt_edad,
#     semanal_dt_sexo, semanal_dt_origen) renderizada dentro de graficas_dinamicas.
#   - Sin otros cambios funcionales.

lista_nominal_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar_panel"),
        width = 4,
        
        # ── Tipo de corte ────────────────────────────────────────────────────
        radioButtons(
          ns("tipo_corte"), 
          "Tipo de datos:",
          choices = c("Histórico (Evolución anual y mensual)" = "historico", 
                      "Semanal (Detallado)" = "semanal"),
          selected = "historico"
        ),
        uiOutput(ns("info_tipo_corte")),
        tags$hr(),
        
        # ── Ámbito Nacional / Extranjero ─────────────────────────────────────
        radioButtons(
          ns("ambito_datos"), 
          "Ámbito de datos:",
          choices = c("Nacional" = "nacional", 
                      "Extranjero" = "extranjero"),
          selected = "nacional",
          inline = TRUE
        ),
        tags$hr(),
        
        # ── Aviso ─────────────────────────────────────────────────────────────
        tags$small(
          style = paste0(
            "color:#000; display:block; font-weight:medium; margin-bottom:8px;",
            " text-align:center; background-color:#CCE4B1; padding:8px; border-radius:4px;"
          ),
          "Configura los filtros y presiona 'Consultar' para actualizar"
        ),
        
        # ── Año ───────────────────────────────────────────────────────────────
        selectInput(ns("year"), "Año:", choices = NULL, selected = NULL),
        
        # ── Desglose (solo semanal) — ANTES de Entidad ────────────────────────
        conditionalPanel(
          condition = "input.tipo_corte == 'semanal'",
          ns = ns,
          uiOutput(ns("selector_desglose")),
          tags$hr()
        ),
        
        # ── Filtros geográficos ───────────────────────────────────────────────
        selectInput(ns("entidad"), "Entidad:", choices = c("Nacional"), selected = "Nacional"),
        conditionalPanel(
          condition = "input.entidad != 'Nacional'",
          ns = ns,
          selectInput(ns("distrito"), "Distrito Electoral:", choices = c("Todos"), selected = "Todos"),
          selectInput(ns("municipio"), "Municipio:", choices = c("Todos"), selected = "Todos"),
          selectizeInput(
            ns("seccion"), 
            "Sección Electoral:", 
            choices  = c("Todas"), 
            selected = "Todas", 
            multiple = TRUE,
            options  = list(
              placeholder = "Selecciona una o más secciones",
              plugins     = list("remove_button"),
              maxItems    = NULL
            )
          )
        ),
        tags$hr(),
        
        # ── Acciones ─────────────────────────────────────────────────────────
        actionButton(
          ns("btn_consultar"), 
          "Consultar", 
          icon  = icon("search"),
          class = "btn-success",
          style = "width:100%; margin-bottom:10px; font-weight:bold; font-size:16px;"
        ),
        tags$hr(),
        actionButton(
          ns("reset_config"), 
          "Restablecer consulta", 
          class = "btn-primary", 
          style = "width:100%; margin-bottom:10px;"
        ),
        downloadButton(
          ns("download_csv"), 
          "Descargar CSV", 
          class = "btn-primary", 
          style = "width:100%"
        )
      ),
      
      mainPanel(
        width = 8,
        
        # ── Gráficas (historico y semanal via graficas_ui_render.R) ───────────
        uiOutput(ns("graficas_dinamicas")),
        
        # ── DataTable histórica: oculta en semanal ────────────────────────────
        conditionalPanel(
          condition = "input.tipo_corte != 'semanal'",
          ns = ns,
          fluidRow(
            column(12, 
                   div(
                     class = "datatable-section",
                     h3("Tabla de Datos", 
                        align = "center", 
                        style = "margin-top:40px;",
                        class = "datatable-title"),
                     div(class = "datatable-header",
                         uiOutput(ns("main-table_header"))),
                     shinycssloaders::withSpinner(
                       DTOutput(ns("main-table_data")),
                       type  = 6,
                       color = "#44559B",
                       size  = 0.8
                     )
                   )
            )
          )
        ),
        
        # ── Descarga móvil ────────────────────────────────────────────────────
        fluidRow(
          column(12,
                 div(
                   class = "mobile-download-container mobile-only",
                   style = "margin-top:20px; margin-bottom:80px; padding:0 10px;",
                   downloadButton(
                     ns("download_csv_mobile"), 
                     "Descargar CSV", 
                     class = "btn-primary mobile-download-btn",
                     style = "width:100%; font-size:16px; padding:12px; font-weight:bold;"
                   )
                 )
          )
        )
      )
    ),
    
    # ── Toggle sidebar derecho ────────────────────────────────────────────────
    div(class = "toggle-container",
        actionButton(
          inputId = ns("toggle-sidebar-lista"), 
          label   = ">>", 
          class   = "toggle-sidebar-btn", 
          `data-sidebar-id` = ns("sidebar-right-lista")
        )
    ),
    
    # ── Sidebar derecho: análisis textual ─────────────────────────────────────
    div(
      id    = ns("sidebar-right-lista"), 
      class = "sidebar-right",
      uiOutput(ns("text_analysis-titulo_lista")),
      uiOutput(ns("text_analysis-alcance_lista")),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-resumen_general_lista"))),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-demografia_lista"))),
      div(class = "sidebar-section", uiOutput(ns("text_analysis-comparacion_lista")))
    )
  )
}
