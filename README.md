# Dashboard Electoral | Raúl Sánchez Salgado

Visualización interactiva de resultados electorales federales en México (2006-2023), basada en datos oficiales del [Instituto Nacional Electoral (INE)](https://siceen21.ine.mx/home).

Este dashboard permite explorar, comparar y analizar los resultados de las elecciones federales desde 2006 hasta 2023, con filtros por año, cargo, estado, distrito, municipio y sección electoral.

## 📊 Funcionalidades principales

- **Consulta dinámica**: Filtra por año, cargo (Presidencia, Senaduría, Diputación Federal), tipo de elección (ordinaria, extraordinaria) y división geo-electoral.
- **Visualización de datos**: Gráficos de barras interactivos con `plotly` y tablas dinámicas con `DT`.
- **Análisis textual automático**: Narrativa contextual que resume resultados, fuerza partidista y participación ciudadana.
- **Descarga de datos**: Exporta los resultados filtrados en formato CSV.

## 🗂️ Estructura del proyecto

dashboard-electoral/
├── app.R # Punto de entrada de la aplicación Shiny
├── README.md # Este archivo
├── utils.R # Funciones auxiliares
├── data/ # Datos (ignorados en Git)
├── server/
│ ├── datos.R
│ ├── partidos_mapping.R
│ └── partidos_colores.R
├── modules/
│ ├── elecciones_federales_ui.R
│ ├── elecciones_federales_server.R
│ └── ...
├── ui/
│ ├── styles/
│ │ └── styles.css
│ └── www/
│ ├── custom.js
│ ├── custom_select_input.js
│ └── sidebar_toggle.js
└── .gitignore # Archivos excluidos del repositorio


## 📚 Fuente de datos

Los datos provienen del [Sistema de Consulta de la Estadística de las Elecciones del INE](https://siceen21.ine.mx/home), que presenta los resultados electorales federales desde 1991 hasta 2023.

## 🛠️ Tecnologías utilizadas

- [R](https://www.r-project.org/)
- [Shiny](https://shiny.rstudio.com/)
- `ggplot2`, `plotly`, `DT`, `dplyr`, `data.table`, `leaflet`, `shinyjs`, `shinyWidgets`
- HTML, CSS, JavaScript (para interactividad personalizada)

## 📂 Notas sobre privacidad y datos

La carpeta `data/` no se incluye en el repositorio por su tamaño y naturaleza. Para ejecutar la app localmente, asegúrate de tener los archivos CSV en `data/results/`.

---

> Desarrollado con RStudio por Raúl Sánchez Salgado | 2025
