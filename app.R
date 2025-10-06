# app.R

# Cargar las librerías necesarias
library(shiny)
library(DT)
library(readr)
library(dplyr)
library(shinyjs)
library(shinyWidgets)
library(ggplot2)
library(tidyr)
library(plotly)
library(scales)
library(here)
library(rlang)
library(bslib)
library(purrr)
library(sf)
library(tools)
library(jsonlite)
library(leaflet)
library(leaflet.providers)
library(tidyr)
library(mapview)

# Cargar archivos compartidos
source("server/partidos_mapping.R")
source("server/datos.R")
source("server/partidos_colores.R")

# Cargar módulos de UI y server para cada pestaña
source("modules/elecciones_federales_ui.R")
source("modules/elecciones_federales_server.R")
source("modules/viz_geografica_ui.R")
source("modules/viz_geografica_server.R")

# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  useShinyjs(),
  includeCSS("ui/styles/styles.css"),
  includeScript("ui/www/custom.js"),
  includeScript("ui/www/custom_select_input.js"),
  includeScript("ui/www/sidebar_toggle.js"),
  
  div(class = "main-container",
      
      div(class = "header",
          titlePanel(
            a(href = ".", "Sefix", id = "title-link", class = "clickable-title")
          )
      ),
      
      tabsetPanel(
        id = "main_tabs",
        selected = "lista",
        
        tabPanel("Lista Nominal Electoral", value = "lista",
                 h2("Proyección de Lista Nominal Electoral"),
                 div(class = "toggle-container",
                     actionButton(inputId = "toggle-sidebar-lista", 
                                  label = ">>", 
                                  class = "toggle-sidebar-btn", 
                                  `data-sidebar-id` = "sidebar-right-lista")
                 ),
                 div(id = "sidebar-right-lista", class = "sidebar-right",
                     uiOutput("text_analysis-titulo_lista"),
                     uiOutput("text_analysis-alcance_lista"),
                     div(class = "sidebar-section",
                         uiOutput("text_analysis-resumen_general_lista")
                     ),
                     div(class = "sidebar-section",
                         uiOutput("text_analysis-demografia_lista")
                     ),
                     div(class = "sidebar-section",
                         uiOutput("text_analysis-comparacion_lista")
                     )
                 ),
                 fluidRow(
                   column(12, 
                          h3("Próximamente: Dashboard interactivo de Lista Nominal Electoral", 
                             align = "center", style = "color: #666; margin-top: 50px;")
                   )
                 )
        ),
        
        tabPanel("Elecciones Federales", value = "federales",
                 elecciones_federales_ui("federales")
        ),
        
        tabPanel("Elecciones Locales", value = "locales",
                 h2("Resultados de Elecciones Locales"),
                 div(class = "toggle-container",
                     actionButton(inputId = "toggle-sidebar-locales", 
                                  label = ">>", 
                                  class = "toggle-sidebar-btn", 
                                  `data-sidebar-id` = "sidebar-right-locales")
                 ),
                 div(id = "sidebar-right-locales", class = "sidebar-right",
                     h3("Análisis de Texto Dinámico"),
                     p("Aquí va el análisis dinámico para Elecciones Locales.")
                 )
        ),
        
        tabPanel("Visualización Geográfica", value = "geografica",
                 viz_geografica_ui("geografica")
        ),
        
        tabPanel("Estadísticos Geoelectorales", value = "geo",
                 h2("Estadísticos Geoelectorales"),
                 div(class = "toggle-container",
                     actionButton(inputId = "toggle-sidebar-geo", 
                                  label = ">>", 
                                  class = "toggle-sidebar-btn", 
                                  `data-sidebar-id` = "sidebar-right-geo")
                 ),
                 div(id = "sidebar-right-geo", class = "sidebar-right",
                     h3("Análisis de Texto Dinámico"),
                     p("Aquí va el análisis dinámico para Estadísticos Geoelectorales.")
                 )
        ),
        
        tabPanel("Otros Estadísticos", value = "otros",
                 h2("Otros Estadísticos"),
                 div(class = "toggle-container",
                     actionButton(inputId = "toggle-sidebar-otros", 
                                  label = ">>", 
                                  class = "toggle-sidebar-btn", 
                                  `data-sidebar-id` = "sidebar-right-otros")
                 ),
                 div(id = "sidebar-right-otros", class = "sidebar-right",
                     h3("Análisis de Texto Dinámico"),
                     p("Aquí va el análisis dinámico para Otros Estadísticos.")
                 )
        )
      )
  )
)


# Definir la lógica del servidor (server)
server <- function(input, output, session) {
  # Verificación mínima de dependencias críticas
  if(!exists("cargar_datos") || !exists("partidos_mapping") || !exists("partidos_colores")) {
    stop("Faltan dependencias críticas. Verifica que los archivos de servidor estén cargados correctamente.")
  }
  
  # Definir combinacion_valida para elecciones federales y visualización geográfica
  combinacion_valida <- reactive({
    req(input$main_tabs)
    if (input$main_tabs == "federales") {
      req(input$federales_year, input$federales_cargo)
      year <- input$federales_year
      cargo <- input$federales_cargo
    } else if (input$main_tabs == "geografica") {
      req(input$geografica_year, input$geografica_cargo)
      year <- input$geografica_year
      cargo <- input$geografica_cargo
    } else {
      return(FALSE)
    }
    
    valid_combinations <- list(
      "2024" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
      "2023" = c("SENADURIA"),
      "2021" = c("DIPUTACION FEDERAL", "SENADURIA"),
      "2018" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
      "2015" = c("DIPUTACION FEDERAL"),
      "2012" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
      "2009" = c("DIPUTACION FEDERAL"),
      "2006" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA")
    )
    
    year_str <- as.character(year)
    if (!year_str %in% names(valid_combinations)) {
      message("Año inválido en combinacion_valida: ", year_str)
      return(FALSE)
    }
    
    valid_cargos <- valid_combinations[[year_str]]
    cargo %in% valid_cargos
  })
  
  # Definir datos_columnas
  datos_columnas <- reactive({
    req(input$main_tabs, combinacion_valida())
    if (input$main_tabs == "federales") {
      req(input$federales_year, input$federales_cargo, input$federales_tipo_eleccion,
          input$federales_estado, input$federales_cabecera, input$federales_municipio,
          input$federales_seccion)
      cargar_datos(
        input$federales_year,
        input$federales_cargo,
        input$federales_tipo_eleccion,
        input$federales_estado,
        input$federales_cabecera,
        input$federales_municipio,
        input$federales_seccion
      )
    } else if (input$main_tabs == "geografica") {
      req(input$geografica_year, input$geografica_cargo, input$geografica_tipo_eleccion,
          input$geografica_estado, input$geografica_cabecera, input$geografica_municipio,
          input$geografica_seccion)
      cargar_datos(
        input$geografica_year,
        input$geografica_cargo,
        input$geografica_tipo_eleccion,
        input$geografica_estado,
        input$geografica_cabecera,
        input$geografica_municipio,
        input$geografica_seccion
      )
    } else {
      NULL
    }
  })
  
  # Llamar a los módulos principales
  elecciones_federales_server("federales")
  viz_geografica_server("geografica", reactive({ 
    if (input$main_tabs == "geografica") datos_columnas() else NULL 
  }), combinacion_valida)
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)