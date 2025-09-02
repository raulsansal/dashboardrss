# modules/viz_geografica_server.R

# Cargar mapeos de partidos y colores
source("server/partidos_mapping.R")
source("server/partidos_colores.R")

viz_geografica_server <- function(id, datos_columnas, combinacion_valida) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Variable reactiva para estados
    todos_estados <- reactiveVal(character(0))
    
    # Actualizar estados disponibles
    observe({
      req(datos_columnas())  # Asegura que datos_columnas sea válido
      if (is.list(datos_columnas()) && !is.null(datos_columnas()$todos_estados)) {
        todos_estados(datos_columnas()$todos_estados)
      } else {
        message("datos_columnas() no contiene todos_estados válido")
      }
    })
    
    observeEvent(datos_columnas(), {
      req(datos_columnas())  # Asegura que datos_columnas sea válido
      updateSelectInput(
        session,
        "estado",
        choices = c("Nacional", todos_estados()),
        selected = input$estado %||% "Nacional"
      )
    })
    
    # Actualizar partidos disponibles
    observeEvent(list(input$geografica_year, input$geografica_cargo), {
      req(input$geografica_year, input$geografica_cargo)
      key <- paste(input$geografica_year, input$geografica_cargo, sep = "_")
      columnas_disponibles <- partidos_mapping[[key]]
      if (is.null(columnas_disponibles)) {
        columnas_disponibles <- character(0)
      }
      updateSelectizeInput(
        session,
        "partidos",
        choices = c("Todos", columnas_disponibles),
        selected = "Todos"
      )
    }, ignoreInit = TRUE)
    
    # Actualizar cargo según año
    observeEvent(input$geografica_year, {
      req(input$geografica_year)
      valid_combinations <- list(
        "2023" = c("SENADURIA"),
        "2021" = c("DIPUTACION FEDERAL", "SENADURIA"),
        "2018" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
        "2015" = c("DIPUTACION FEDERAL"),
        "2012" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA"),
        "2009" = c("DIPUTACION FEDERAL"),
        "2006" = c("DIPUTACION FEDERAL", "SENADURIA", "PRESIDENCIA")
      )
      
      year_str <- as.character(input$geografica_year)
      choices <- valid_combinations[[year_str]]
      if (is.null(choices)) {
        message("Año inválido en viz_geografica_server: ", year_str)
        choices <- character(0)
      }
      
      selected <- if (!is.null(input$geografica_cargo) && input$geografica_cargo %in% choices) {
        input$geografica_cargo
      } else {
        choices[1]
      }
      
      # Resetear estado a "Nacional" excepto en casos específicos
      if (!(input$geografica_year == "2021" && input$geografica_cargo == "SENADURIA") &&
          !(input$geografica_year == "2023")) {
        isolate({
          updateSelectInput(
            session,
            "estado",
            choices = c("Nacional", todos_estados()),
            selected = "Nacional"
          )
        })
      }
      
      updateSelectInput(
        session,
        "cargo",
        choices = choices,
        selected = selected
      )
      
      updateRadioButtons(
        session,
        "tipo_eleccion",
        selected = "AMBAS"
      )
    }, priority = 1)
    
    # Manejar casos especiales de elecciones
    observeEvent(list(input$geografica_year, input$geografica_cargo, input$estado), {
      req(input$geografica_year, input$geografica_cargo, input$estado)
      datos <- datos_columnas()
      
      # Validar que datos_columnas sea una lista válida
      if (!is.list(datos)) {
        message("datos_columnas() no es una lista válida")
        return()
      }
      
      if (input$geografica_year == "2023") {
        updateSelectInput(
          session,
          "estado",
          selected = "TAMAULIPAS"
        )
        updateSelectizeInput(
          session,
          "partidos",
          selected = "Todos"
        )
        updateRadioButtons(
          session,
          "tipo_eleccion",
          choices = c("EXTRAORDINARIA"),
          selected = "EXTRAORDINARIA"
        )
      } else if (input$geografica_year == "2021" && input$geografica_cargo == "SENADURIA") {
        updateSelectInput(
          session,
          "estado",
          selected = "NAYARIT"
        )
        updateSelectizeInput(
          session,
          "partidos",
          selected = "Todos"
        )
        updateRadioButtons(
          session,
          "tipo_eleccion",
          choices = c("EXTRAORDINARIA"),
          selected = "EXTRAORDINARIA"
        )
      } else {
        choices <- c()
        # Usar isTRUE para evitar errores con NULL
        if (isTRUE(datos$tiene_ordinaria)) choices <- c(choices, "ORDINARIA")
        if (isTRUE(datos$tiene_extraordinaria)) choices <- c(choices, "EXTRAORDINARIA")
        if (length(choices) > 1) choices <- c(choices, "AMBAS")
        
        if (length(choices) == 0) {
          message("No hay tipos de elección disponibles para year=", input$geografica_year, ", cargo=", input$geografica_cargo)
          choices <- c("ORDINARIA")
        }
        
        selected <- if ("AMBAS" %in% choices) "AMBAS" else choices[1]
        
        updateRadioButtons(
          session,
          "tipo_eleccion",
          choices = choices,
          selected = selected
        )
      }
    })
    
    # Actualizar distritos según estado
    observeEvent(input$estado, {
      req(input$estado)
      datos <- datos_columnas()
      if (is.list(datos) && !is.null(datos$todos_cabeceras)) {
        updateSelectInput(
          session,
          "cabecera",
          choices = c("Todos", datos$todos_cabeceras),
          selected = "Todos"
        )
      }
    })
    
    # Actualizar municipios según estado y distrito
    observeEvent(list(input$estado, input$cabecera), {
      req(input$estado, input$cabecera)
      datos <- datos_columnas()
      if (is.list(datos) && !is.null(datos$todos_municipios)) {
        updateSelectizeInput(
          session,
          "municipio",
          choices = c("Todos", datos$todos_municipios),
          selected = "Todos"
        )
      }
    })
    
    # Actualizar secciones según distrito y municipio
    observeEvent(list(input$cabecera, input$municipio), {
      req(input$cabecera, input$municipio)
      datos <- datos_columnas()
      if (is.list(datos) && !is.null(datos$todas_secciones)) {
        updateSelectizeInput(
          session,
          "seccion",
          choices = c("Todas", datos$todas_secciones),
          selected = "Todas"
        )
      }
    })
    
    # Manejar selección "Todas" en secciones
    observeEvent(input$seccion, {
      req(input$seccion)
      if ("Todas" %in% input$seccion) {
        updateSelectizeInput(
          session,
          "seccion",
          selected = "Todas"
        )
      }
    })
    
    # Manejar selección "Todos" en partidos
    observeEvent(input$partidos, {
      req(input$partidos)
      if ("Todos" %in% input$partidos) {
        updateSelectizeInput(
          session,
          "partidos",
          selected = "Todos"
        )
      }
    })
    
    # Función para cargar el shapefile adecuado
    cargar_shapefile <- function(nivel, estado = NULL, year = NULL, cabecera = NULL, municipio = NULL, seccion = NULL) {
      base_path <- if (!is.null(year) && as.numeric(year) < 2018) "data/shapefiles/2013/" else "data/shapefiles/2017/"
      
      estados_codigos <- list(
        "Aguascalientes" = "01", "Baja California" = "02", "Baja California Sur" = "03",
        "Campeche" = "04", "Coahuila" = "05", "Colima" = "06", "Chiapas" = "07",
        "Chihuahua" = "08", "Ciudad de México" = "09", "Durango" = "10",
        "Guanajuato" = "11", "Guerrero" = "12", "Hidalgo" = "13", "Jalisco" = "14",
        "México" = "15", "Michoacán" = "16", "Morelos" = "17", "Nayarit" = "18",
        "Nuevo León" = "19", "Oaxaca" = "20", "Puebla" = "21", "Querétaro" = "22",
        "Quintana Roo" = "23", "San Luis Potosí" = "24", "Sinaloa" = "25",
        "Sonora" = "26", "Tabasco" = "27", "Tamaulipas" = "28", "Tlaxcala" = "29",
        "Veracruz" = "30", "Yucatán" = "31", "Zacatecas" = "32"
      )
      
      estados_abreviaturas <- list(
        "Aguascalientes" = "Ags", "Baja California" = "BC", "Baja California Sur" = "BCS",
        "Campeche" = "Camp", "Coahuila" = "Coah", "Colima" = "Col", "Chiapas" = "Chis",
        "Chihuahua" = "Chih", "Ciudad de México" = "CDMX", "Durango" = "Dgo",
        "Guanajuato" = "Gto", "Guerrero" = "Gro", "Hidalgo" = "Hgo", "Jalisco" = "Jal",
        "México" = "Mex", "Michoacán" = "Mich", "Morelos" = "Mor", "Nayarit" = "Nay",
        "Nuevo León" = "NL", "Oaxaca" = "Oax", "Puebla" = "Pue", "Querétaro" = "Qro",
        "Quintana Roo" = "QRoo", "San Luis Potosí" = "SLP", "Sinaloa" = "Sin",
        "Sonora" = "Son", "Tabasco" = "Tab", "Tamaulipas" = "Tamps", "Tlaxcala" = "Tlax",
        "Veracruz" = "Ver", "Yucatán" = "Yuc", "Zacatecas" = "Zac"
      )
      
      if (nivel == "nacional") {
        shapefile_path <- paste0(base_path, "Nacional_Distritos.shp")
        if (file.exists(shapefile_path)) {
          shapefile <- sf::st_read(shapefile_path, quiet = TRUE)
          message("Shapefile distrital cargado: ", shapefile_path, " con columnas: ", paste(names(shapefile), collapse = ", "))
          
          if (!sf::st_is_longlat(shapefile)) {
            shapefile <- sf::st_transform(shapefile, 4326)
          }
          
          invalid_geoms <- !sf::st_is_valid(shapefile)
          if (any(invalid_geoms)) {
            message("Reparando ", sum(invalid_geoms), " geometrías inválidas")
            shapefile[invalid_geoms, ] <- sf::st_make_valid(shapefile[invalid_geoms, ])
          }
          
          if ("entidad" %in% names(shapefile) && "distrito" %in% names(shapefile)) {
            shapefile <- shapefile %>%
              dplyr::mutate(
                entidad_clean = as.character(entidad),
                distrito_clean = as.character(distrito),
                datos_id = ifelse(
                  !is.na(entidad_clean) & !is.na(distrito_clean) & grepl("^[0-9]+$", entidad_clean) & grepl("^[0-9]+$", distrito_clean),
                  paste0(sprintf("%02d", as.numeric(entidad_clean)), "_", sprintf("%02d", as.numeric(distrito_clean))),
                  NA_character_
                )
              )
            message("Columna datos_id creada con ", sum(!is.na(shapefile$datos_id)), " IDs válidos")
            if (all(is.na(shapefile$datos_id))) {
              showNotification("No se pudieron crear IDs válidos para el shapefile nacional.", type = "error")
              return(NULL)
            }
            return(shapefile)
          } else {
            showNotification("No se encontraron columnas 'entidad' o 'distrito' en el shapefile.", type = "error")
            return(NULL)
          }
        } else {
          showNotification("No se encontró el shapefile Nacional_Distritos.shp.", type = "error")
          return(NULL)
        }
      } else if (nivel == "estatal" && !is.null(estado)) {
        codigo_estado <- estados_codigos[[estado]]
        abreviatura <- estados_abreviaturas[[estado]]
        if (is.null(codigo_estado) || is.null(abreviatura)) {
          showNotification(paste("No se encontró código o abreviatura para el estado:", estado), type = "error")
          return(NULL)
        }
        
        shapefile_path <- paste0(base_path, "estados/", codigo_estado, "_Shape_", abreviatura, "/ENTIDAD.shp")
        if (file.exists(shapefile_path)) {
          shapefile <- sf::st_read(shapefile_path, quiet = TRUE)
          message("Shapefile cargado: ", shapefile_path, " con columnas: ", paste(names(shapefile), collapse = ", "))
          
          if (!sf::st_is_longlat(shapefile)) {
            shapefile <- sf::st_transform(shapefile, 4326)
          }
          
          invalid_geoms <- !sf::st_is_valid(shapefile)
          if (any(invalid_geoms)) {
            message("Reparando ", sum(invalid_geoms), " geometrías inválidas")
            shapefile[invalid_geoms, ] <- sf::st_make_valid(shapefile[invalid_geoms, ])
          }
          
          return(shapefile)
        } else {
          showNotification(paste("No se encontró shapefile ENTIDAD para el estado:", estado), type = "error")
          return(NULL)
        }
      } else if (nivel == "distrital" && !is.null(estado)) {
        codigo_estado <- estados_codigos[[estado]]
        abreviatura <- estados_abreviaturas[[estado]]
        if (is.null(codigo_estado) || is.null(abreviatura)) {
          showNotification(paste("No se encontró código o abreviatura para el estado:", estado), type = "error")
          return(NULL)
        }
        
        shapefile_path <- paste0(base_path, "estados/", codigo_estado, "_Shape_", abreviatura, "/DISTRITO.shp")
        if (file.exists(shapefile_path)) {
          shapefile <- sf::st_read(shapefile_path, quiet = TRUE)
          message("Shapefile cargado: ", shapefile_path, " con columnas: ", paste(names(shapefile), collapse = ", "))
          
          if (!sf::st_is_longlat(shapefile)) {
            shapefile <- sf::st_transform(shapefile, 4326)
          }
          
          invalid_geoms <- !sf::st_is_valid(shapefile)
          if (any(invalid_geoms)) {
            message("Reparando ", sum(invalid_geoms), " geometrías inválidas")
            shapefile[invalid_geoms, ] <- sf::st_make_valid(shapefile[invalid_geoms, ])
          }
          
          if (!is.null(cabecera) && cabecera != "Todos") {
            distrito_num <- as.numeric(gsub("\\D", "", cabecera))
            distrito_cols <- c("DISTRITO", "DISTRITO_FEDERAL", "DISTRITO_ID", "ID_DISTRITO", "NUMERO_DISTRITO")
            
            for (col in distrito_cols) {
              if (col %in% names(shapefile)) {
                shapefile <- shapefile %>% dplyr::filter(get(col) == distrito_num)
                break
              }
            }
          }
          
          return(shapefile)
        } else {
          showNotification(paste("No se encontró shapefile DISTRITO para el estado:", estado), type = "error")
          return(NULL)
        }
      } else if (nivel == "municipal" && !is.null(estado)) {
        codigo_estado <- estados_codigos[[estado]]
        abreviatura <- estados_abreviaturas[[estado]]
        if (is.null(codigo_estado) || is.null(abreviatura)) {
          showNotification(paste("No se encontró código o abreviatura para el estado:", estado), type = "error")
          return(NULL)
        }
        
        shapefile_path <- paste0(base_path, "estados/", codigo_estado, "_Shape_", abreviatura, "/MUNICIPIO.shp")
        if (file.exists(shapefile_path)) {
          shapefile <- sf::st_read(shapefile_path, quiet = TRUE)
          message("Shapefile cargado: ", shapefile_path, " con columnas: ", paste(names(shapefile), collapse = ", "))
          
          if (!sf::st_is_longlat(shapefile)) {
            shapefile <- sf::st_transform(shapefile, 4326)
          }
          
          invalid_geoms <- !sf::st_is_valid(shapefile)
          if (any(invalid_geoms)) {
            message("Reparando ", sum(invalid_geoms), " geometrías inválidas")
            shapefile[invalid_geoms, ] <- sf::st_make_valid(shapefile[invalid_geoms, ])
          }
          
          if (!is.null(municipio) && municipio != "Todos") {
            municipio_cols <- c("MUNICIPIO", "MUNICIPIO_ID", "ID_MUNICIPIO", "NOMBRE_MUNICIPIO")
            
            for (col in municipio_cols) {
              if (col %in% names(shapefile)) {
                shapefile <- shapefile %>% dplyr::filter(get(col) == municipio)
                break
              }
            }
          }
          
          return(shapefile)
        } else {
          showNotification(paste("No se encontró shapefile MUNICIPIO para el estado:", estado), type = "error")
          return(NULL)
        }
      } else if (nivel == "seccional" && !is.null(estado)) {
        codigo_estado <- estados_codigos[[estado]]
        abreviatura <- estados_abreviaturas[[estado]]
        if (is.null(codigo_estado) || is.null(abreviatura)) {
          showNotification(paste("No se encontró código o abreviatura para el estado:", estado), type = "error")
          return(NULL)
        }
        
        shapefile_path <- paste0(base_path, "estados/", codigo_estado, "_Shape_", abreviatura, "/SECCION.shp")
        if (file.exists(shapefile_path)) {
          shapefile <- sf::st_read(shapefile_path, quiet = TRUE)
          message("Shapefile cargado: ", shapefile_path, " con columnas: ", paste(names(shapefile), collapse = ", "))
          
          if (!sf::st_is_longlat(shapefile)) {
            shapefile <- sf::st_transform(shapefile, 4326)
          }
          
          invalid_geoms <- !sf::st_is_valid(shapefile)
          if (any(invalid_geoms)) {
            message("Reparando ", sum(invalid_geoms), " geometrías inválidas")
            shapefile[invalid_geoms, ] <- sf::st_make_valid(shapefile[invalid_geoms, ])
          }
          
          if (!is.null(seccion) && seccion != "Todas") {
            seccion_cols <- c("SECCION", "SECCION_ID", "ID_SECCION", "NUMERO_SECCION")
            
            for (col in seccion_cols) {
              if (col %in% names(shapefile)) {
                shapefile <- shapefile %>% dplyr::filter(get(col) %in% seccion)
                break
              }
            }
          }
          
          return(shapefile)
        } else {
          showNotification(paste("No se encontró shapefile SECCION para el estado:", estado), type = "error")
          return(NULL)
        }
      }
      
      return(NULL)
    }
    
    # Datos del mapa
    get_map_data <- reactive({
      req(combinacion_valida())
      
      message("Evaluando datos_columnas...")
      datos_col <- datos_columnas()
      message("Clase de datos_col: ", class(datos_col))
      message("Nombres de datos_col: ", paste(names(datos_col), collapse = ", "))
      
      if (!is.list(datos_col) || is.null(datos_col$datos)) {
        showNotification("No se pudieron cargar los datos para el mapa.", type = "error")
        return(NULL)
      }
      
      datos <- datos_col$datos
      message("Clase de datos: ", class(datos))
      message("Columnas de datos: ", paste(names(datos), collapse = ", "))
      
      if (!is.data.frame(datos) || nrow(datos) == 0) {
        showNotification("No se encontraron datos válidos para el mapa.", type = "error")
        return(NULL)
      }
      
      anio_eleccion <- unique(datos$anio)[1]
      message("Año de elección detectado: ", anio_eleccion)
      
      nivel <- if (input$estado == "Nacional") {
        "nacional"
      } else if (input$cabecera == "Todos" && input$municipio == "Todos" && input$seccion == "Todas") {
        "estatal"
      } else if (input$cabecera != "Todos" && input$municipio == "Todos" && input$seccion == "Todas") {
        "distrital"
      } else if (input$municipio != "Todos" && input$seccion == "Todas") {
        "municipal"
      } else {
        "seccional"
      }
      message("Nivel geográfico inferido: ", nivel)
      
      # Cargar el shapefile correspondiente
      map_data <- if (nivel == "nacional") {
        cargar_shapefile("nacional", year = anio_eleccion)
      } else if (nivel == "estatal") {
        cargar_shapefile("estatal", estado = input$estado, year = anio_eleccion)
      } else if (nivel == "distrital") {
        cargar_shapefile("distrital", estado = input$estado, year = anio_eleccion, cabecera = input$cabecera)
      } else if (nivel == "municipal") {
        cargar_shapefile("municipal", estado = input$estado, year = anio_eleccion, municipio = input$municipio)
      } else {
        cargar_shapefile("seccional", estado = input$estado, year = anio_eleccion, seccion = input$seccion)
      }
      
      if (is.null(map_data)) {
        showNotification("No se pudo cargar el shapefile.", type = "error")
        return(NULL)
      }
      
      # Obtener columnas de partidos según año y cargo
      key <- paste(anio_eleccion, input$geografica_cargo, sep = "_")
      partido_cols <- partidos_mapping[[key]]
      if (is.null(partido_cols)) {
        message("No se encontraron columnas de partidos para ", key)
        showNotification("No se encontraron datos de partidos para la combinación seleccionada.", type = "error")
        return(NULL)
      }
      message("Columnas de partidos para ", key, ": ", paste(partido_cols, collapse = ", "))
      
      if (nivel == "nacional") {
        message("Valores únicos de cve_estado: ", paste(unique(datos$cve_estado), collapse = ", "))
        message("Valores únicos de cabecera: ", paste(unique(datos$cabecera), collapse = ", "))
        
        datos_agg <- datos %>%
          dplyr::group_by(cve_estado, cabecera) %>%
          dplyr::summarise(
            across(any_of(c(partido_cols, "total_votos")), sum, na.rm = TRUE),
            part_ciud = mean(part_ciud, na.rm = TRUE),
            estado = first(estado),
            .groups = "drop"
          ) %>%
          dplyr::mutate(
            cve_estado_clean = as.character(cve_estado),
            cabecera_clean = as.character(cabecera),
            cve_estado_clean = gsub("[^0-9]", "", cve_estado_clean),
            cabecera_clean = substr(gsub("[^0-9]", "", substr(cabecera_clean, 1, 4)), 1, 2),
            datos_id = ifelse(
              !is.na(cve_estado_clean) & !is.na(cabecera_clean) & nchar(cve_estado_clean) > 0 & nchar(cabecera_clean) > 0,
              paste0(sprintf("%02d", as.numeric(cve_estado_clean)), "_", sprintf("%02d", as.numeric(cabecera_clean))),
              NA_character_
            )
          )
        
        # Calcular partido ganador y fuerzas
        votos_cols <- setdiff(partido_cols, c("no_reg", "vot_nul"))
        
        datos_agg$partido_ganador <- apply(datos_agg[, votos_cols, drop = FALSE], 1, function(x) {
          if (all(is.na(x))) return(NA_character_)
          votos_cols[which.max(x)]
        })
        
        datos_agg$votos_ganador <- apply(datos_agg[, votos_cols, drop = FALSE], 1, function(x) {
          if (all(is.na(x))) return(NA)
          max(x, na.rm = TRUE)
        })
        
        datos_agg$porcentaje_ganador <- ifelse(
          datos_agg$total_votos > 0,
          (datos_agg$votos_ganador / datos_agg$total_votos) * 100,
          NA_real_
        )
        
        datos_agg$partido_segundo <- apply(datos_agg[, votos_cols, drop = FALSE], 1, function(x) {
          if (all(is.na(x)) || sum(!is.na(x)) < 2) return(NA_character_)
          sorted <- sort(x, decreasing = TRUE, na.last = TRUE)
          if (length(sorted) < 2) return(NA_character_)
          votos_cols[which(x == sorted[2])[1]]
        })
        
        datos_agg$votos_segundo <- apply(datos_agg[, votos_cols, drop = FALSE], 1, function(x) {
          if (all(is.na(x)) || sum(!is.na(x)) < 2) return(NA_real_)
          sorted <- sort(x, decreasing = TRUE, na.last = TRUE)
          if (length(sorted) < 2) return(NA_real_)
          sorted[2]
        })
        
        datos_agg$porcentaje_segundo <- ifelse(
          datos_agg$total_votos > 0,
          (datos_agg$votos_segundo / datos_agg$total_votos) * 100,
          NA_real_
        )
        
        datos_agg$partido_tercero <- apply(datos_agg[, votos_cols, drop = FALSE], 1, function(x) {
          if (all(is.na(x)) || sum(!is.na(x)) < 3) return(NA_character_)
          sorted <- sort(x, decreasing = TRUE, na.last = TRUE)
          if (length(sorted) < 3) return(NA_character_)
          votos_cols[which(x == sorted[3])[1]]
        })
        
        datos_agg$votos_tercero <- apply(datos_agg[, votos_cols, drop = FALSE], 1, function(x) {
          if (all(is.na(x)) || sum(!is.na(x)) < 3) return(NA_real_)
          sorted <- sort(x, decreasing = TRUE, na.last = TRUE)
          if (length(sorted) < 3) return(NA_real_)
          sorted[3]
        })
        
        datos_agg$porcentaje_tercero <- ifelse(
          datos_agg$total_votos > 0,
          (datos_agg$votos_tercero / datos_agg$total_votos) * 100,
          NA_real_
        )
        
        datos_agg$participacion_promedio_ganador <- datos_agg$part_ciud
        
        message("Datos agregados generados con ", sum(!is.na(datos_agg$datos_id)), " IDs válidos")
        message("Primeros datos_id en datos_agg: ", paste(head(datos_agg$datos_id, 5), collapse = ", "))
        message("Partido ganador calculado para ", sum(!is.na(datos_agg$partido_ganador)), " distritos")
        message("Valores de participacion_promedio: ", paste(head(datos_agg$participacion_promedio_ganador, 5), collapse = ", "))
        
        map_data <- map_data %>%
          dplyr::left_join(datos_agg, by = "datos_id")
        
        message("Filas en map_data tras unión: ", nrow(map_data))
        message("Valores de total_votos: ", paste(head(map_data$total_votos, 5), collapse = ", "))
        message("NAs en total_votos: ", sum(is.na(map_data$total_votos)))
        
        if (nrow(map_data) == 0 || !("total_votos" %in% names(map_data)) || all(is.na(map_data$total_votos))) {
          showNotification("No se pudieron unir los datos con el shapefile nacional.", type = "error")
          return(NULL)
        }
      } else if (nivel == "estatal") {
        estado_cols <- names(map_data)[grep("ENTIDAD|ESTADO", toupper(names(map_data)))]
        if (length(estado_cols) > 0) {
          estado_col <- estado_cols[1]
          map_data$datos_id <- sprintf("%02d", map_data[[estado_col]])
          datos <- datos[tolower(datos$estado) == tolower(input$estado), ] %>%
            dplyr::mutate(datos_id = sprintf("%02d", cve_estado))
          
          map_data <- map_data %>%
            dplyr::left_join(datos, by = "datos_id")
          
          if (nrow(map_data) == 0 || all(is.na(map_data$total_votos))) {
            showNotification("No se pudieron unir los datos con el shapefile estatal.", type = "error")
            return(NULL)
          }
        } else {
          showNotification("No se encontraron columnas compatibles para el join estatal.", type = "error")
          return(NULL)
        }
      } else if (nivel == "distrital") {
        distrito_cols <- names(map_data)[grep("DISTR", toupper(names(map_data)))]
        if (length(distrito_cols) > 0) {
          distrito_col <- distrito_cols[1]
          datos_filtrados <- datos[tolower(datos$estado) == tolower(input$estado), ]
          
          if (input$cabecera != "Todos") {
            distrito_num <- as.numeric(gsub("\\D", "", input$cabecera))
            datos_filtrados <- datos_filtrados[as.numeric(datos_filtrados$cabecera) == distrito_num, ]
          }
          
          map_data$distrito_id <- as.numeric(map_data[[distrito_col]])
          datos_filtrados$distrito_id <- as.numeric(datos_filtrados$cabecera)
          
          map_data <- map_data %>%
            dplyr::left_join(datos_filtrados, by = "distrito_id")
          
          if (nrow(map_data) == 0 || all(is.na(map_data$total_votos))) {
            showNotification("No se pudieron unir los datos con el shapefile distrital.", type = "error")
            return(NULL)
          }
        } else {
          showNotification("No se encontraron columnas compatibles para el join distrital.", type = "error")
          return(NULL)
        }
      } else if (nivel == "municipal") {
        municipio_cols <- names(map_data)[grep("MUNICIPIO|MUN", toupper(names(map_data)))]
        if (length(municipio_cols) > 0) {
          municipio_col <- municipio_cols[1]
          
          datos_filtrados <- datos[tolower(datos$estado) == tolower(input$estado), ]
          
          if (input$municipio != "Todos") {
            datos_filtrados <- datos_filtrados[datos_filtrados$municipio == input$municipio, ]
          }
          
          map_data$municipio_id <- as.character(map_data[[municipio_col]])
          datos_filtrados$municipio_id <- as.character(datos_filtrados$municipio)
          
          map_data <- map_data %>%
            dplyr::left_join(datos_filtrados, by = "municipio_id")
          
          if (nrow(map_data) == 0 || all(is.na(map_data$total_votos))) {
            showNotification("No se pudieron unir los datos con el shapefile municipal.", type = "error")
            return(NULL)
          }
        } else {
          showNotification("No se encontraron columnas compatibles para el join municipal.", type = "error")
          return(NULL)
        }
      } else if (nivel == "seccional") {
        seccion_cols <- names(map_data)[grep("SECCION", toupper(names(map_data)))]
        if (length(seccion_cols) > 0) {
          seccion_col <- seccion_cols[1]
          
          datos_filtrados <- datos[tolower(datos$estado) == tolower(input$estado), ]
          
          if (input$seccion != "Todas") {
            datos_filtrados <- datos_filtrados[datos_filtrados$seccion %in% input$seccion, ]
          }
          
          map_data$seccion_id <- as.numeric(map_data[[seccion_col]])
          datos_filtrados$seccion_id <- as.numeric(datos_filtrados$seccion)
          
          map_data <- map_data %>%
            dplyr::left_join(datos_filtrados, by = "seccion_id")
          
          if (nrow(map_data) == 0 || all(is.na(map_data$total_votos))) {
            showNotification("No se pudieron integrar los datos con el shapefile seccional.", type = "error")
            return(NULL)
          }
        } else {
          showNotification("No se encontraron columnas compatibles para el join seccional.", type = "error")
          return(NULL)
        }
      }
      
      message("map_data generado con ", nrow(map_data), " filas")
      return(map_data)
    })
    
    # Renderizar el mapa
    output$mapa_electoral <- renderLeaflet({
      req(combinacion_valida())
      
      message("Intentando renderizar mapa_electoral")
      map_data <- get_map_data()
      
      if (is.null(map_data)) {
        showNotification("No se pudieron cargar los datos del mapa.", type = "warning")
        return(
          leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            setView(lng = -102, lat = 23, zoom = 5)
        )
      }
      
      # Validar geometrías
      valid_geometries <- sf::st_is_valid(map_data)
      if (any(!valid_geometries)) {
        message("Geometrías inválidas detectadas: ", sum(!valid_geometries), " casos")
        map_data <- map_data[valid_geometries, ]
        if (nrow(map_data) == 0) {
          showNotification("Todas las geometrías son inválidas.", type = "error")
          return(
            leaflet() %>%
              addProviderTiles(providers$OpenStreetMap) %>%
              setView(lng = -102, lat = 23, zoom = 5)
          )
        }
      }
      
      # Verificar partido ganador
      if (!("partido_ganador" %in% names(map_data)) || all(is.na(map_data$partido_ganador))) {
        showNotification("No se pudo determinar el partido ganador.", type = "warning")
        return(
          leaflet() %>%
            addProviderTiles(providers$OpenStreetMap) %>%
            setView(lng = -102, lat = 23, zoom = 5)
        )
      }
      
      # Depurar colores asignados
      unique_partidos <- unique(map_data$partido_ganador[!is.na(map_data$partido_ganador)])
      message("Partidos ganadores únicos: ", paste(unique_partidos, collapse = ", "))
      missing_colors <- setdiff(unique_partidos, names(partidos_colores))
      if (length(missing_colors) > 0) {
        message("Partidos sin color definido: ", paste(missing_colors, collapse = ", "))
      }
      
      # Crear vector de colores
      colores <- as.vector(unlist(partidos_colores[unique_partidos]))
      colores[is.na(colores)] <- partidos_colores[["default"]]
      message("Colores asignados: ", paste(unique_partidos, colores, sep = ": ", collapse = "; "))
      
      # Crear paleta de colores
      paleta <- colorFactor(
        palette = colores,
        levels = unique_partidos,
        na.color = partidos_colores[["default"]]
      )
      
      # Crear tooltips
      popup_text <- lapply(1:nrow(map_data), function(i) {
        row <- map_data[i, ]
        popup <- "<b>Información:</b><br/>"
        if ("estado" %in% names(map_data)) {
          popup <- paste0(popup, "<b>Estado:</b> ", row$estado, "<br/>")
        }
        if ("cabecera" %in% names(map_data)) {
          popup <- paste0(popup, "<b>Distrito:</b> ", row$cabecera, "<br/>")
        }
        popup <- paste0(
          popup,
          "<b>Total Votos:</b> ", format(row$total_votos, big.mark = ","), "<br/>",
          "<b>1ª Fuerza:</b> ", ifelse(is.na(row$partido_ganador), "N/A", row$partido_ganador), 
          " - ", sprintf("%.1f%%", ifelse(is.na(row$porcentaje_ganador), 0, row$porcentaje_ganador)), "<br/>",
          "<b>2ª Fuerza:</b> ", ifelse(is.na(row$partido_segundo), "N/A", row$partido_segundo), 
          " - ", sprintf("%.1f%%", ifelse(is.na(row$porcentaje_segundo), 0, row$porcentaje_segundo)), "<br/>",
          "<b>3ª Fuerza:</b> ", ifelse(is.na(row$partido_tercero), "N/A", row$partido_tercero), 
          " - ", sprintf("%.1f%%", ifelse(is.na(row$porcentaje_tercero), 0, row$porcentaje_tercero)), "<br/>",
          "<b>Participación electoral promedio:</b> ", sprintf("%.2f%%", ifelse(is.na(row$participacion_promedio_ganador), 0, row$participacion_promedio_ganador))
        )
        popup
      })
      
      # Filtrar partidos para la leyenda dinámicamente
      anio_election <- unique(datos_columnas()$datos$anio)[1]
      key <- paste(anio_election, input$geografica_cargo, sep = "_")
      partido_cols <- partidos_mapping[[key]]
      if (is.null(partido_cols)) {
        message("No se encontraron partidos para la leyenda")
        partidos_leyenda <- unique_partidos
      } else {
        partidos_leyenda <- partido_cols[!grepl("_", partido_cols) & 
                                           !partido_cols %in% c("no_reg", "_no_reg", "vot_nul", "_vot_nulo", "total_votos")]
        partidos_leyenda <- intersect(partidos_leyenda, unique_partidos)
      }
      message("Partidos en la leyenda: ", paste(partidos_leyenda, collapse = ", "))
      
      message("Renderizando map_data con ", nrow(map_data), " polígonos")
      
      mapa <- leaflet(map_data) %>%
        addProviderTiles(providers$OpenStreetMap) %>%
        setView(lng = -102, lat = 23, zoom = 5) %>%
        addPolygons(
          fillColor = ~paleta(partido_ganador),
          fillOpacity = 0.7,
          color = "#666666",
          weight = 1,
          opacity = 1,
          popup = popup_text
        ) %>%
        addLegend(
          position = "bottomright",
          pal = paleta,
          values = partidos_leyenda,
          title = "Partido Ganador",
          opacity = 1
        )
      
      message("Mapa renderizado exitosamente")
      return(mapa)
    })
    
    # Outputs del mapa adaptados desde elecciones_federales_server_text_analysis.R
    output$titulo_mapa <- renderUI({
      req(input$geografica_year, input$geografica_cargo, input$geografica_type_election)
      
      cargo_formateado <- switch(input$geografica_cargo,
                                 "DIPUTACION FEDERAL" = "Diputación Federal",
                                 "SENADURIA" = "Senaduría",
                                 "PRESIDENCIA" = "Presidencia",
                                 input$geografica_cargo)
      
      if (input$geografica_type_election == "EXTRAORDINARIA") {
        HTML(paste0(
          "<h3>Mapa Electoral - Elección Extraordinaria <span class='year-highlight'>", input$geografica_year, "</span></h3>",
          "<h4 class='cargo-subtitulo'>", cargo_formateado, "</h4>"
        ))
      } else {
        HTML(paste0(
          "<h3>Mapa Electoral - Elecciones Federales <span class='year-highlight'>", input$geografica_year, "</span></h3>",
          "<h4 class='cargo-subtitulo'>", cargo_formateado, "</h4>"
        ))
      }
    })
    
    output$advertencia_distritacion <- renderUI({
      req(input$geografica_year)
      if (as.numeric(input$geografica_year) < 2018) {
        HTML("<p class='sidebar-alert'>Advertencia: Se utiliza la distritación de 2013 para años anteriores a 2018.</p>")
      } else {
        HTML("<p class='sidebar-info'>Distritación de 2017 aplicada.</p>")
      }
    })
    
    output$opciones_mapa <- renderUI({
      req(combinacion_valida())
      tagList(
        actionButton(ns("reset_mapa"), "Restablecer Mapa", class = "btn-primary"),
        br(),
        checkboxInput(ns("show_legend"), "Mostrar Leyenda", value = TRUE)
      )
    })
    
    output$instrucciones_mapa <- renderUI({
      HTML(
        "<p class='sidebar-info'>Seleccione un año, cargo, estado, y otros filtros para visualizar los resultados electorales en el mapa interactivo. Haga clic en las regiones para ver detalles detallados de votación.</p>"
      )
    })
    
    output$descargar_mapa <- downloadHandler(
      filename = function() {
        paste(sprintf("mapa_electural_%s_%s_%s.pdf", input$geografica_year, input$geografica_cargo, Sys.Date()))
      },
      content = function(file) {
        mapview::mapshot(get_map_data(), file = file)
      }
    )
    
    # Observar cuando se eliminan partidos
    observeEvent(input$geografica_partidos_removed, {
      req(input$partidos)
      
      current_values <- input$partidos
      if (is.null(current_values) || length(current_values) == 0) {
        updateSelectizeInput(
          session,
          "partidos",
          selected = NULL
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Observar cuando se eliminan secciones electorales
    observeEvent(input$geografica_seccion_removed, {
      req(input$seccion)
      
      current_values <- input$seccion
      if (is.null(current_values) || length(current_values) == 0) {
        updateSelectizeInput(
          session,
          "seccion",
          selected = NULL
        )
      }
    }, ignoreNULL = TRUE, ignoreInit = TRUE)
    
    # Verificar tamaño del contenedor
    observe({
      session$sendCustomMessage(
        "checkContainerSize",
        list(id = ns("mapa_electoral"))
      )
    })
  })
}