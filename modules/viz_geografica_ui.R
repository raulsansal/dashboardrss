# modules/viz_geografica_ui.R

viz_geografica_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    tags$head(
      tags$style(HTML("
        #geografica-mapa_electoral {
          width: 100% !important;
          height: 600px !important;
          min-height: 600px !important;
          position: relative !important;
          z-index: 1 !important;
          display: block !important;
          visibility: visible !important;
        }
        .mapa-container {
          width: 100% !important;
          height: 600px !important;
          min-width: 300px !important;
          overflow: visible !important;
          background: #fff !important;
          border: 1px solid #ddd;
          border-radius: 4px;
        }
        .mapa-section {
          width: 100% !important;
          min-width: 300px !important;
          margin-bottom: 20px;
          padding: 15px;
          background: #f9f9f9;
          border: 1px solid #ddd;
          border-radius: 4px;
        }
        .main-panel {
          width: 100% !important;
          min-width: 300px !important;
        }
        .col-sm-9, .col-sm-12 {
          width: 100% !important;
          min-width: 300px !important;
        }
        .leaflet-container {
          background: #fff !important;
        }
      ")),
      tags$script(HTML("
        function debugMapContainer() {
          var debugInfo = {
            timestamp: new Date().toISOString(),
            map: null,
            container: null,
            mainContainer: null,
            mainPanel: null
          };
          
          var mapElement = document.getElementById('geografica-mapa_electoral');
          if (mapElement) {
            var rect = mapElement.getBoundingClientRect();
            debugInfo.map = {
              id: 'geografica-mapa_electoral',
              exists: true,
              width: rect.width,
              height: rect.height,
              display: window.getComputedStyle(mapElement).display,
              visibility: window.getComputedStyle(mapElement).visibility,
              zIndex: window.getComputedStyle(mapElement).zIndex,
              position: window.getComputedStyle(mapElement).position
            };
          } else {
            debugInfo.map = { id: 'geografica-mapa_electoral', exists: false };
          }
          
          var containerElement = mapElement ? mapElement.closest('.mapa-container') : null;
          if (containerElement) {
            var containerRect = containerElement.getBoundingClientRect();
            debugInfo.container = {
              exists: true,
              width: containerRect.width,
              height: containerRect.height,
              display: window.getComputedStyle(containerElement).display
            };
          }
          
          var mainContainerElement = mapElement ? mapElement.closest('.container-fluid') : null;
          if (mainContainerElement) {
            var mainRect = mainContainerElement.getBoundingClientRect();
            debugInfo.mainContainer = {
              exists: true,
              width: mainRect.width,
              height: mainRect.height,
              display: window.getComputedStyle(mainContainerElement).display
            };
          }
          
          var mainPanelElement = mapElement ? mapElement.closest('.main-panel') : null;
          if (mainPanelElement) {
            var panelRect = mainPanelElement.getBoundingClientRect();
            debugInfo.mainPanel = {
              exists: true,
              width: panelRect.width,
              height: panelRect.height,
              display: window.getComputedStyle(mainPanelElement).display
            };
          }
          
          console.log('Map Container Debug at ' + debugInfo.timestamp + ':', debugInfo);
          
          if (!mapElement) {
            console.error('Error: #geografica-mapa_electoral not found in DOM');
          } else if (rect.width === 0 || rect.height === 0) {
            console.warn('Warning: #geografica-mapa_electoral has zero size');
          }
          
          // Forzar redimensionamiento de Leaflet
          if (mapElement && typeof L !== 'undefined' && L.map) {
            var mapInstance = mapElement.__leaflet_map__ || null;
            if (mapInstance && rect.width > 0 && rect.height > 0) {
              mapInstance.invalidateSize();
              console.log('Leaflet map invalidated size');
            }
          }
        }
        
        // Ejecutar después de 2 segundos para esperar el renderizado del DOM
        document.addEventListener('DOMContentLoaded', function() {
          setTimeout(debugMapContainer, 2000);
        });
        
        // Re-ejecutar cada 2 segundos durante los primeros 12 segundos
        var debugInterval = setInterval(debugMapContainer, 2000);
        setTimeout(function() {
          clearInterval(debugInterval);
        }, 12000);
        
        // Re-ejecutar al redimensionar la ventana
        window.addEventListener('resize', debugMapContainer);
        
        // Observar cambios en el contenedor
        document.addEventListener('DOMContentLoaded', function() {
          var container = document.querySelector('.mapa-container');
          if (container) {
            var observer = new ResizeObserver(function() {
              debugMapContainer();
            });
            observer.observe(container);
          }
        });
      "))
    ),
    sidebarLayout(
      sidebarPanel(
        id = ns("sidebar_panel"),
        selectInput(ns("year"), "Año de elección:", 
                    choices = c(2006, 2009, 2012, 2015, 2018, 2021, 2023), 
                    selected = 2021),
        selectInput(ns("cargo"), "Cargo de elección:", 
                    choices = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"), 
                    selected = "DIPUTACION FEDERAL"),
        selectInput(ns("estado"), "Estado:", 
                    choices = c("Nacional", 
                                "Aguascalientes", "Baja California", "Baja California Sur",
                                "Campeche", "Coahuila", "Colima", "Chiapas", "Chihuahua",
                                "Ciudad de México", "Durango", "Guanajuato", "Guerrero",
                                "Hidalgo", "Jalisco", "México", "Michoacán", "Morelos",
                                "Nayarit", "Nuevo León", "Oaxaca", "Puebla", "Querétaro",
                                "Quintana Roo", "San Luis Potosí", "Sinaloa", "Sonora",
                                "Tabasco", "Tamaulipas", "Tlaxcala", "Veracruz", "Yucatán",
                                "Zacatecas")),
        selectizeInput(ns("partidos"), "Partidos, coaliciones o candidaturas:", 
                       choices = c("Todos"), multiple = TRUE, selected = "Todos"),
        radioButtons(ns("tipo_eleccion"), "Tipo de elección:", 
                     choices = c("ORDINARIA", "EXTRAORDINARIA", "AMBAS"), 
                     selected = "AMBAS"),
        selectInput(ns("cabecera"), "Distrito Electoral:", 
                    choices = c("Todos"), selected = "Todos"),
        selectizeInput(ns("municipio"), "Municipio:", 
                       choices = c("Todos"), selected = "Todos"),
        selectizeInput(ns("seccion"), "Sección Electoral:", 
                       choices = c("Todas"), selected = "Todas", multiple = TRUE)
      ),
      mainPanel(
        div(
          class = "mapa-section",
          h3("Visualización Geográfica de Resultados", class = "bg-primary text-white", style = "padding: 10px; margin: 0;"),
          div(
            style = "padding: 15px;",
            h4(textOutput(ns("titulo_mapa"))),
            uiOutput(ns("advertencia_distritacion")),
            p(textOutput(ns("instrucciones_mapa"))),
            fluidRow(
              column(
                width = 9,
                div(
                  class = "mapa-container",
                  leafletOutput(ns("mapa_electoral"), height = "600px")
                )
              ),
              column(
                width = 3,
                div(
                  class = "mapa-section",
                  h4("Opciones de visualización"),
                  uiOutput(ns("opciones_mapa")),
                  br(),
                  downloadButton(ns("descargar_mapa"), "Descargar mapa", 
                                 class = "btn-primary btn-block")
                )
              )
            )
          )
        )
      )
    ),
    div(class = "toggle-container",
        actionButton(inputId = ns("toggle-sidebar-geografica"), 
                     label = ">>", 
                     class = "toggle-sidebar-btn", 
                     `data-sidebar-id` = ns("sidebar-right-geografica"))
    ),
    div(id = ns("sidebar-right-geografica"), class = "sidebar-right",
        h3("Análisis de Texto Dinámico"),
        p("Aquí va el análisis dinámico para Visualización Geográfica.")
    )
  )
}