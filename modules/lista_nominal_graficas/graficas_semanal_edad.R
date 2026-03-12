# modules/lista_nominal_graficas/graficas_semanal_edad.R
# Vista Semanal — Gráficas de Edad: E1, E2, E3, E4
# Versión: 1.13 — Fix E2/E4: incluir no_binario en suma de LNE y Padrón por grupo/rango
#
# CAMBIOS vs v1.8:
#   construir_fila_filtrada(): corregido bug de doble conteo de filas-extranjero.
#   Ahora excluye por firma estructural (cve_distrito=0, cve_municipio=0, seccion=0)
#   además de por nombre, y excluye también filas sin número de sección (subtotales).
#
# CAMBIOS vs v1.7:
#   E1/E3 — Ahora responden a filtros geográficos del usuario.
#   Se agrega reactive interno datos_serie_edad_efectiva() que decide:
#     · estado "restablecido" o filtros nacionales → usa datos_semanal_serie_edad()
#       (serie pre-cargada en caché, retorno inmediato)
#     · estado "consultado" con filtros geográficos → calcula la serie al vuelo
#       descargando cada semana con los filtros activos (sin cachear, igual que
#       el histórico consultado). Muestra spinner simple mientras carga.
#   E1 y E3 consumen datos_serie_edad_efectiva() en lugar de
#   datos_semanal_serie_edad() directamente.
#   Los bindEvent de E1 y E3 ya incluían input$btn_consultar — sin cambios.
#
# CAMBIOS vs v1.5:
#   E4 — título con fecha dinámica y ámbito, colores pad/lne de E1,
#        eje X sin título, márgenes y espaciado corregidos
#   E1/E3/E4 — orden leyenda/fuente corregido: leyenda y=-0.18, fuente y=-0.30
#   COLOR_GRUPOS_NAC — paleta fija c("#6BA4C6","#277592","#D10F3F")
#
# CAMBIOS vs v1.4:
#   Modal E1 y E3 — padding-left:24px en ol/ul para corregir sangría dentro del modal
#
# CAMBIOS vs v1.3:
#   Modal E1 y E3 — rediseño visual coherente con modal histórico:
#     icono chart-line, pasos con negrita, bloque fórmula, sección consideraciones,
#     botón Cerrar azul sólido
#
# CAMBIOS vs v1.2:
#   Fix E3 — observeEvent semanal_e3_btn_reset y semanal_e3_btn_metodologia:
#            eliminado req() redundante dentro del handler; session = session
#            explícito en updateCheckboxGroupInput (patrón idéntico a E1)
#
# CAMBIOS vs v1.1:
#   E3 (nueva) — Evolución y Proyección Semanal por Grupo Etario
#              — Misma lógica que E1 pero agrupando rangos en Jóvenes/Adultos/Mayores
#              — Widget: 3 checks en fila única + Restablecer + Metodología
#              — Paleta: mismos colores que E2 (COLOR_GRUPOS_NAC / COLOR_GRUPOS_EXT)
#   E4 (era E3) — Padrón y LNE por rango individual (barras agrupadas) — sin cambios
#
# Gráficas:
#   E1 (semanal_e1_proyeccion)  — Serie temporal + proyección por rango de edad (12 rangos)
#   E2 (semanal_e2_grupos)      — LNE por grupos etarios agregados (barras horizontales)
#   E3 (semanal_e3_proyeccion_grupos) — Serie temporal + proyección por grupo etario (3 grupos)
#   E4 (semanal_e4_barras)      — Padrón y LNE por rango individual (barras agrupadas)
#
# Dependencias del entorno padre (graficas_semanal.R):
#   COLORES, ORDEN_EDAD, FUENTE_INE
#   fmt_num(), fmt_pct(), etiq_ambito(), etiqueta_edad()
#   ann_fuente(), ann_alcance(), plot_vacio()
#   color_padron(), color_lista()
#   es_historico(), desglose_activo()
#   construir_df_edad()
#
# Dependencias de datos (pasadas como argumentos):
#   datos_semanal_edad()       ← corte único (E2, E3)
#   datos_semanal_serie_edad() ← serie temporal (E1)
#   anio_semanal(), fecha_semanal_efectiva()
#   texto_alcance(), ambito_reactivo(), estado_app()

graficas_semanal_edad <- function(input, output, session,
                                  datos_semanal_edad,
                                  datos_semanal_serie_edad,
                                  anio_semanal,
                                  fecha_semanal_efectiva,
                                  texto_alcance,
                                  ambito_reactivo,
                                  estado_app) {
  
  message("📊 Inicializando graficas_semanal_edad v1.7")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CONSTANTES LOCALES
  # ══════════════════════════════════════════════════════════════════════════
  
  RANGOS_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                   "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  ETIQ_RANGOS <- c(
    "18"       = "18 años",
    "19"       = "19 años",
    "20_24"    = "20–24 años",
    "25_29"    = "25–29 años",
    "30_34"    = "30–34 años",
    "35_39"    = "35–39 años",
    "40_44"    = "40–44 años",
    "45_49"    = "45–49 años",
    "50_54"    = "50–54 años",
    "55_59"    = "55–59 años",
    "60_64"    = "60–64 años",
    "65_y_mas" = "65+ años"
  )
  
  # ── Paletas coherentes: azules = Padrón, Rojos = LNE (Nacional) ───────────
  # Modo por rango: 12 tonos alternando azules (pad) y rojos (lne)
  # Los índices impares = padrón (azul), pares = lne (rojo)
  # Para que cada RANGO tenga su par de colores: tono_i para pad, tono_i para lne
  # usamos 9 tonos de cada familia y los 12 rangos consumen 9 → cicla
  
  AZULES  <- c("#B3CDE5","#90B8D6","#6BA4C6","#4891B3",
               "#277592","#015378","#003E66","#012A51","#00193B")
  ROJOS   <- c("#FFA0A0","#F87FA8","#F15F8E","#E05F7F",
               "#D10F3F","#D3103F","#AE0E35","#8B0A2B","#71001B")
  DORADOS <- c("#FFF2CC","#FFEAA9","#FFE48A","#FFDE6F",
               "#FFD14A","#F5CA45","#EAC43E","#E0BD39","#D6B632")
  VERDES  <- c("#CCE4B1","#B3D491","#99C374","#7EAF5A",
               "#71A251","#518033","#49732C","#3E6625","#37591F")
  
  # Función que devuelve el color de padrón y LNE para el rango i (1-indexed)
  # Nacional: padrón=azul[i], LNE=rojo[i]; Extranjero: pad=dorado[i], lne=verde[i]
  color_rango_pad <- function(i, ambito) {
    pal <- if (ambito == "extranjero") DORADOS else AZULES
    pal[((i - 1) %% length(pal)) + 1]
  }
  color_rango_lne <- function(i, ambito) {
    pal <- if (ambito == "extranjero") VERDES else ROJOS
    pal[((i - 1) %% length(pal)) + 1]
  }
  
  # Colores modo total
  COLOR_PAD_NAC_TOTAL <- "#277592"   # azul medio
  COLOR_LNE_NAC_TOTAL <- "#D10F3F"   # rojo medio
  COLOR_PAD_EXT_TOTAL <- "#EAC43E"   # dorado medio
  COLOR_LNE_EXT_TOTAL <- "#71A251"   # verde medio
  
  color_total_pad <- function(a) if (a == "extranjero") COLOR_PAD_EXT_TOTAL else COLOR_PAD_NAC_TOTAL
  color_total_lne <- function(a) if (a == "extranjero") COLOR_LNE_EXT_TOTAL else COLOR_LNE_NAC_TOTAL
  
  # Grupos etarios para E2
  GRUPOS_ETARIOS <- list(
    "Jóvenes\n(18–29)"    = c("18","19","20_24","25_29"),
    "Adultos\n(30–59)"    = c("30_34","35_39","40_44","45_49","50_54","55_59"),
    "Mayores\n(60+)"      = c("60_64","65_y_mas")
  )
  COLOR_GRUPOS_NAC <- c("#6BA4C6","#277592","#D10F3F")
  COLOR_GRUPOS_EXT <- c("#FFDE6F","#EAC43E","#7EAF5A")
  
  # Grupos etarios para E3 (widget + gráfica)
  GRUPOS_E3        <- c("jovenes", "adultos", "mayores")
  ETIQ_GRUPOS_E3   <- c(jovenes = "Jóvenes (18–29)", adultos = "Adultos (30–59)", mayores = "Mayores (60+)")
  RANGOS_GRUPOS_E3 <- list(
    jovenes = c("18","19","20_24","25_29"),
    adultos = c("30_34","35_39","40_44","45_49","50_54","55_59"),
    mayores = c("60_64","65_y_mas")
  )
  
  # ── Helper: formato de fecha en español ───────────────────────────────────
  fecha_es <- function(d) {
    meses <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
               "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
    paste(as.integer(format(d, "%d")),
          meses[as.integer(format(d, "%m"))],
          format(d, "%Y"))
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: datos_serie_edad_efectiva()
  #
  # Punto único de acceso a la serie temporal de edad para E1 y E3.
  # Decide qué fuente usar según el estado y los filtros activos:
  #
  #   · "restablecido" o filtros nacionales:
  #       → datos_semanal_serie_edad()  (pre-cargada en caché, instantáneo)
  #
  #   · "consultado" con filtros geográficos (entidad/distrito/municipio/sección):
  #       → calcula la serie al vuelo descargando cada semana con los filtros
  #         del usuario. No se cachea (igual que el histórico consultado).
  #         Muestra spinner simple mientras carga.
  #
  # Al cambiar de estado restablecido → consultado, el bindEvent de E1/E3
  # (que ya incluye input$btn_consultar) dispara el re-render con esta serie.
  # ══════════════════════════════════════════════════════════════════════════
  
  filtros_defecto_local <- list(entidad = "Nacional", distrito = "Todos",
                                municipio = "Todos", seccion = "Todas")
  
  es_nacional_sin_filtros_local <- function(f) {
    f$entidad == "Nacional" && f$distrito == "Todos" && f$municipio == "Todos" &&
      (is.null(f$seccion) || "Todas" %in% f$seccion || length(f$seccion) == 0)
  }
  
  datos_serie_edad_efectiva <- reactive({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    estado_actual <- estado_app()
    ambito        <- ambito_reactivo()
    
    # Estado restablecido → siempre la serie cacheada nacional/extranjero
    if (estado_actual != "consultado") {
      return(datos_semanal_serie_edad())
    }
    
    # Estado consultado: obtener filtros del usuario
    btn     <- input$btn_consultar
    filtros <- isolate({
      list(
        entidad   = input$entidad   %||% "Nacional",
        distrito  = input$distrito  %||% "Todos",
        municipio = input$municipio %||% "Todos",
        seccion   = input$seccion   %||% "Todas"
      )
    })
    
    # Si los filtros son nacionales → también usar la serie cacheada
    if (es_nacional_sin_filtros_local(filtros)) {
      message("📊 [serie_edad_efectiva] Consultado con filtros nacionales → caché")
      return(datos_semanal_serie_edad())
    }
    
    # Filtros geográficos activos → calcular serie al vuelo
    message("📊 [serie_edad_efectiva] Filtros geográficos: ",
            filtros$entidad, " / ", filtros$distrito, " / ", filtros$municipio)
    
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    fechas  <- sort(as.Date(catalog$semanal_comun, origin = "1970-01-01"))
    fechas  <- fechas[fechas <= Sys.Date()]
    if (length(fechas) == 0) return(NULL)
    
    ORDEN_EDAD_LOCAL <- c("18","19","20_24","25_29","30_34","35_39",
                          "40_44","45_49","50_54","55_59","60_64","65_y_mas")
    
    construir_fila_filtrada <- function(df_raw, fecha_d) {
      if (is.null(df_raw) || nrow(df_raw) == 0) return(NULL)
      
      # Identificador canónico de filas EXT
      es_ext <- function(df) {
        if (!"cabecera_distrital" %in% colnames(df)) return(rep(FALSE, nrow(df)))
        toupper(trimws(df$cabecera_distrital)) == "RESIDENTES EXTRANJERO"
      }
      
      # Seleccionar filas según ámbito y filtros
      fila <- if (ambito == "extranjero") {
        # EXT: sumar las 32 filas directamente
        filas <- df_raw[es_ext(df_raw), , drop = FALSE]
        if (nrow(filas) == 0) return(NULL)
        cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                      "cve_municipio","nombre_municipio","seccion")
        cols_num <- setdiff(colnames(filas), cols_id)
        cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE],
                                    function(x) is.numeric(x) ||
                                      suppressWarnings(!any(is.na(as.numeric(x)))))]
        ag <- as.data.frame(
          lapply(filas[, cols_num, drop = FALSE], function(x) sum(as.numeric(x), na.rm = TRUE)),
          stringsAsFactors = FALSE)
        ag$nombre_entidad <- "RESIDENTES EXTRANJERO"
        ag
        
      } else {
        # Nacional: secciones nacionales (sin EXT, sin fila TOTALES)
        # Si hay filtro geográfico activo, sumar solo las filas del filtro
        mask_excl <- es_ext(df_raw)
        if ("cve_entidad" %in% colnames(df_raw)) mask_excl <- mask_excl | is.na(df_raw$cve_entidad)
        filas <- df_raw[!mask_excl, , drop = FALSE]
        if (nrow(filas) == 0) return(NULL)
        
        if (nrow(filas) == 1) {
          filas
        } else {
          cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                        "cve_municipio","nombre_municipio","seccion")
          cols_num <- setdiff(colnames(filas), cols_id)
          cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE],
                                      function(x) is.numeric(x) ||
                                        suppressWarnings(!any(is.na(as.numeric(x)))))]
          ag <- as.data.frame(
            lapply(filas[, cols_num, drop = FALSE], function(x) sum(as.numeric(x), na.rm = TRUE)),
            stringsAsFactors = FALSE)
          ag$nombre_entidad <- "SECCIONES_NAC"
          ag
        }
      }
      
      if (is.null(fila) || nrow(fila) == 0) return(NULL)
      
      resultado <- list(fecha = fecha_d)
      for (rango in ORDEN_EDAD_LOCAL) {
        col_ph <- paste0("padron_", rango, "_hombres")
        col_pm <- paste0("padron_", rango, "_mujeres")
        col_pn <- paste0("padron_", rango, "_no_binario")
        col_lh <- paste0("lista_",  rango, "_hombres")
        col_lm <- paste0("lista_",  rango, "_mujeres")
        col_ln <- paste0("lista_",  rango, "_no_binario")
        resultado[[paste0("padron_", rango)]] <- sum(
          if (col_ph %in% colnames(fila)) as.numeric(fila[[col_ph]]) else 0,
          if (col_pm %in% colnames(fila)) as.numeric(fila[[col_pm]]) else 0,
          if (col_pn %in% colnames(fila)) as.numeric(fila[[col_pn]]) else 0,
          na.rm = TRUE)
        resultado[[paste0("lista_", rango)]] <- sum(
          if (col_lh %in% colnames(fila)) as.numeric(fila[[col_lh]]) else 0,
          if (col_lm %in% colnames(fila)) as.numeric(fila[[col_lm]]) else 0,
          if (col_ln %in% colnames(fila)) as.numeric(fila[[col_ln]]) else 0,
          na.rm = TRUE)
      }
      resultado$padron_total <- sum(unlist(resultado[paste0("padron_", ORDEN_EDAD_LOCAL)]), na.rm = TRUE)
      resultado$lista_total  <- sum(unlist(resultado[paste0("lista_",  ORDEN_EDAD_LOCAL)]), na.rm = TRUE)
      as.data.frame(resultado, stringsAsFactors = FALSE)
    }
    filas_serie <- list()
    n_ok <- 0L; n_err <- 0L
    
    cargar_loop <- function() {
      for (f in fechas) {
        fecha_d <- as.Date(f, origin = "1970-01-01")
        df_raw  <- tryCatch({
          res <- cargar_lne(tipo_corte = "semanal", fecha = fecha_d,
                            dimension = "edad",
                            estado     = filtros$entidad,
                            distrito   = filtros$distrito,
                            municipio  = filtros$municipio,
                            seccion    = filtros$seccion,
                            incluir_extranjero = TRUE)
          if (!is.null(res) && !is.null(res$datos)) res$datos else NULL
        }, error = function(e) NULL)
        
        fila <- construir_fila_filtrada(df_raw, fecha_d)
        if (!is.null(fila)) {
          filas_serie[[length(filas_serie) + 1]] <<- fila
          n_ok <<- n_ok + 1L
        } else {
          n_err <<- n_err + 1L
        }
      }
    }
    
    shiny::withProgress(
      message = "Cargando datos semanales...",
      value   = NULL,
      cargar_loop()
    )
    
    if (length(filas_serie) == 0) {
      message("⚠️ [serie_edad_efectiva] Sin datos para filtros geográficos")
      return(NULL)
    }
    
    serie <- do.call(rbind, filas_serie)
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL
    message("✅ [serie_edad_efectiva] ", nrow(serie), " semanas (OK=", n_ok,
            " ERR=", n_err, ") — ", filtros$entidad)
    return(serie)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # E1 — WIDGET: selector de rangos + botones Restablecer y Metodología
  # Layout: [checks fila 1 | checks fila 2] [btn_reset] [btn_metodologia]
  # Los checks se distribuyen en 2 filas de 6 con lógica responsive
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e1_rangos_ui <- renderUI({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    tagList(
      # CSS para dividir el checkboxGroupInput en 2 filas de 6
      tags$style(HTML(paste0(
        "#", session$ns("semanal_e1_rangos"),
        " .shiny-options-group { display:flex; flex-wrap:wrap; gap:4px 0; }",
        "#", session$ns("semanal_e1_rangos"),
        " .shiny-options-group .checkbox { width:calc(100%/6); min-width:90px;",
        " margin:0; padding:1px 4px; box-sizing:border-box; }",
        "#", session$ns("semanal_e1_rangos"),
        " label { font-size:12px; color:#333; cursor:pointer; }",
        "#", session$ns("semanal_e1_rangos"),
        " input[type='checkbox'] { accent-color:#277592; cursor:pointer; }"
      ))),
      
      div(
        style = paste(
          "background:#f4f6f8;border:1px solid #d0d7de;border-radius:7px;",
          "padding:10px 14px 8px 14px;margin-bottom:10px;"
        ),
        div(
          style = "display:flex;align-items:flex-start;gap:12px;",
          
          # Izquierda: label + checks nativos (2 filas × 6 via CSS)
          div(
            style = "flex:1;min-width:0;",
            div(
              style = "margin-bottom:5px;",
              tags$span(
                style = "font-size:11px;font-weight:600;color:#555;",
                "Rangos de edad:"
              )
            ),
            checkboxGroupInput(
              inputId  = session$ns("semanal_e1_rangos"),
              label    = NULL,
              choices  = setNames(RANGOS_EDAD, ETIQ_RANGOS[RANGOS_EDAD]),
              selected = RANGOS_EDAD,
              inline   = FALSE   # inline=FALSE para que el CSS de columnas funcione
            )
          ),
          
          # Derecha: botones apilados
          div(
            style = "display:flex;flex-direction:column;gap:5px;flex-shrink:0;padding-top:16px;",
            actionButton(
              inputId = session$ns("semanal_e1_btn_reset"),
              label   = tagList(icon("undo"), " Restablecer"),
              class   = "btn btn-sm",
              style   = paste(
                "background-color:#6c757d;color:#fff;border:none;",
                "font-size:11px;padding:4px 10px;border-radius:4px;",
                "white-space:nowrap;width:110px;"
              )
            ),
            actionButton(
              inputId = session$ns("semanal_e1_btn_metodologia"),
              label   = tagList(icon("info-circle"), " Metodología"),
              class   = "btn btn-sm btn-outline-info",
              style   = paste(
                "font-size:11px;padding:4px 10px;border-radius:4px;",
                "white-space:nowrap;width:110px;"
              )
            )
          )
        )
      )
    )
  })
  
  # Restablecer → todos los rangos seleccionados
  observeEvent(input$semanal_e1_btn_reset, {
    updateCheckboxGroupInput(
      session = session,
      inputId = "semanal_e1_rangos",
      selected = RANGOS_EDAD
    )
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # Metodología → showModal directo con el mismo contenido que gráfica histórica 1
  # (el botón info_grafica1 no existe en el DOM durante la vista semanal)
  observeEvent(input$semanal_e1_btn_metodologia, {
    showModal(modalDialog(
      title     = tagList(icon("chart-line"), " Metodología de Proyección"),
      size      = "l",
      easyClose = TRUE,
      footer    = tagList(
        tags$button(
          type = "button", class = "btn btn-primary",
          style = "background-color:#277592;border-color:#277592;padding:8px 32px;font-size:14px;border-radius:6px;",
          `data-dismiss` = "modal", "Cerrar"
        )
      ),
      tagList(
        div(
          style = "margin-bottom:16px;",
          h4(tags$span(style = "color:#1a3a5c;font-weight:700;",
                       "¿Cómo se calcula la proyección?"),
             style = "margin-top:0;"),
          p("La proyección utiliza un ",
            tags$strong("modelo de tasa de crecimiento mensual promedio"),
            " basado en los cortes semanales disponibles del año en curso.")
        ),
        h5(tags$span(style = "color:#1a3a5c;font-weight:700;", "Pasos del cálculo:"),
           style = "margin-bottom:8px;"),
        tags$ol(
          style = "padding-left:24px;margin-bottom:12px;",
          tags$li(tagList(tags$strong("Datos base:"),       " Cortes semanales del año actual.")),
          tags$li(tagList(tags$strong("Tasa de crecimiento:"), " Promedio mensual entre el primer y último corte disponible.")),
          tags$li(tagList(tags$strong("Proyección:"),       " Se aplica la tasa compuesta hasta diciembre.")),
          tags$li(tagList(tags$strong("Visualización:"),    " Líneas punteadas = valores proyectados."))
        ),
        tags$hr(),
        h5(tags$span(style = "color:#1a3a5c;font-weight:700;", "Fórmula:"),
           style = "margin-bottom:8px;"),
        div(
          style = "background:#eef4fb;border-left:4px solid #277592;padding:10px 16px;border-radius:4px;font-family:monospace;font-size:13px;color:#D10F3F;margin-bottom:16px;",
          div("Tasa = (Valor_final / Valor_inicial)^(1/(n-1)) — 1"),
          div("Proyección(i) = Último_valor × (1 + tasa)^i")
        ),
        div(style = "color:#c0392b;font-weight:700;margin-bottom:6px;",
            icon("triangle-exclamation"), " Consideraciones:"),
        tags$ul(
          style = "padding-left:24px;margin-bottom:12px;",
          tags$li(tagList("Asume crecimiento ", tags$strong("constante"), ".")),
          tags$li(tagList("Es una ", tags$strong("estimación estadística"), ".")),
          tags$li(tagList("Proyecta hasta ", tags$strong("diciembre"), " del año.")),
          tags$li("Los datos oficiales del INE prevalecen.")
        ),
        tags$hr(),
        div(
          style = "text-align:center;font-size:12px;color:#666;",
          icon("circle-info"),
          " Esta es una herramienta de referencia. Los datos oficiales son los publicados por el INE."
        )
      )
    ))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # ══════════════════════════════════════════════════════════════════════════
  # E1 — GRÁFICA: evolución y proyección semanal por rango de edad
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e1_proyeccion <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    serie <- datos_serie_edad_efectiva()
    if (is.null(serie) || nrow(serie) < 2) {
      return(plot_vacio("Sin datos de serie temporal para proyección"))
    }
    
    # Fecha de la última semana disponible en la serie
    ultima_fecha <- max(serie$fecha)
    etiq_fecha   <- fecha_es(ultima_fecha)
    
    # Rangos seleccionados
    rangos_sel <- input$semanal_e1_rangos %||% RANGOS_EDAD
    if (length(rangos_sel) == 0) rangos_sel <- RANGOS_EDAD
    todos_sel  <- setequal(sort(rangos_sel), sort(RANGOS_EDAD))
    
    message("📊 [E1 v1.1] Rangos: ", paste(rangos_sel, collapse=","),
            " | Ámbito: ", ambito, " | Última fecha: ", etiq_fecha)
    
    # Meses restantes para proyección
    ultimo_mes <- as.integer(format(ultima_fecha, "%m"))
    meses_rest <- 12L - ultimo_mes
    
    p <- plot_ly()
    
    if (todos_sel) {
      # ── Modo total: Padrón Total (azul) + LNE Total (rojo) ─────────────────
      col_pad <- color_total_pad(ambito)
      col_lne <- color_total_lne(ambito)
      
      proy <- NULL
      if (meses_rest > 0) {
        sp                   <- serie
        sp$lista_nominal     <- sp$lista_total
        sp$padron_electoral  <- sp$padron_total
        proy <- tryCatch(proyectar_con_tasa_crecimiento(sp, meses_rest),
                         error = function(e) NULL)
      }
      
      p <- p %>%
        add_trace(
          data = serie, x = ~fecha, y = ~padron_total,
          type = "scatter", mode = "lines+markers", name = "Padrón Total",
          line   = list(color = col_pad, width = 3),
          marker = list(size = 7, color = col_pad, symbol = "circle"),
          hovertemplate = "<b>%{x|%d %b %Y}</b><br>Padrón Total: %{y:,.0f}<extra></extra>"
        ) %>%
        add_trace(
          data = serie, x = ~fecha, y = ~lista_total,
          type = "scatter", mode = "lines+markers", name = "LNE Total",
          line   = list(color = col_lne, width = 3),
          marker = list(size = 7, color = col_lne, symbol = "square"),
          hovertemplate = "<b>%{x|%d %b %Y}</b><br>LNE Total: %{y:,.0f}<extra></extra>"
        )
      
      if (!is.null(proy) && nrow(proy) > 0) {
        p <- p %>%
          add_trace(
            data = proy, x = ~fecha, y = ~padron_proyectado,
            type = "scatter", mode = "lines", name = "Proyección Padrón",
            line = list(color = col_pad, width = 2, dash = "dash"),
            showlegend = TRUE,
            hovertemplate = "<b>%{x|%d %b %Y}</b><br>Proy. Padrón: %{y:,.0f}<extra></extra>"
          ) %>%
          add_trace(
            data = proy, x = ~fecha, y = ~lista_proyectada,
            type = "scatter", mode = "lines", name = "Proyección LNE",
            line = list(color = col_lne, width = 2, dash = "dash"),
            showlegend = TRUE,
            hovertemplate = "<b>%{x|%d %b %Y}</b><br>Proy. LNE: %{y:,.0f}<extra></extra>"
          )
      }
      
    } else {
      # ── Modo por rango: tono azul i para padrón, tono rojo i para LNE ──────
      for (i in seq_along(rangos_sel)) {
        rango   <- rangos_sel[i]
        etiq_r  <- ETIQ_RANGOS[rango] %||% rango
        col_pad <- color_rango_pad(i, ambito)
        col_lne <- color_rango_lne(i, ambito)
        col_pad_pad <- paste0("padron_", rango)
        col_lne_lne <- paste0("lista_",  rango)
        
        if (!col_pad_pad %in% colnames(serie)) next
        
        proy_r <- NULL
        if (meses_rest > 0) {
          sr                   <- serie
          sr$lista_nominal     <- sr[[col_lne_lne]]
          sr$padron_electoral  <- sr[[col_pad_pad]]
          proy_r <- tryCatch(proyectar_con_tasa_crecimiento(sr, meses_rest),
                             error = function(e) NULL)
        }
        
        p <- p %>%
          add_trace(
            data = serie, x = ~fecha, y = serie[[col_pad_pad]],
            type = "scatter", mode = "lines+markers",
            name = paste0("Padrón ", etiq_r),
            line   = list(color = col_pad, width = 2.5),
            marker = list(size = 6, color = col_pad, symbol = "circle"),
            hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Padrón ", etiq_r,
                                   ": %{y:,.0f}<extra></extra>")
          ) %>%
          add_trace(
            data = serie, x = ~fecha, y = serie[[col_lne_lne]],
            type = "scatter", mode = "lines+markers",
            name = paste0("LNE ", etiq_r),
            line   = list(color = col_lne, width = 2.5, dash = "dot"),
            marker = list(size = 6, color = col_lne, symbol = "square"),
            hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>LNE ", etiq_r,
                                   ": %{y:,.0f}<extra></extra>")
          )
        
        if (!is.null(proy_r) && nrow(proy_r) > 0) {
          p <- p %>%
            add_trace(
              data = proy_r, x = ~fecha, y = ~padron_proyectado,
              type = "scatter", mode = "lines",
              name = paste0("Proy. Padrón ", etiq_r),
              line = list(color = col_pad, width = 1.5, dash = "dash"),
              showlegend = FALSE,
              hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Proy. Padrón ",
                                     etiq_r, ": %{y:,.0f}<extra></extra>")
            ) %>%
            add_trace(
              data = proy_r, x = ~fecha, y = ~lista_proyectada,
              type = "scatter", mode = "lines",
              name = paste0("Proy. LNE ", etiq_r),
              line = list(color = col_lne, width = 1.5, dash = "dashdot"),
              showlegend = FALSE,
              hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Proy. LNE ",
                                     etiq_r, ": %{y:,.0f}<extra></extra>")
            )
        }
      }
    }
    
    # Título con fecha de último corte
    titulo_graf <- paste0(
      "Evolución y Proyección Semanal del Padrón y LNE — ",
      etiq_fecha, " — ", etiq
    )
    
    p %>% layout(
      title = list(
        text = titulo_graf,
        font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis = list(
        title      = "",
        type       = "date",
        tickformat = "%d %b",
        range      = c(min(serie$fecha) - 3,
                       as.Date(paste0(anio, "-12-31")))
      ),
      yaxis = list(
        title             = "Número de electores",
        separatethousands = TRUE
      ),
      legend = list(
        orientation = "h",
        xanchor     = "center",
        x           = 0.5,
        y           = -0.10,
        font        = list(size = 11)
      ),
      margin    = list(t = 120, b = 80, l = 90, r = 40),
      hovermode = "x unified",
      annotations = list(
        ann_alcance(alcance, y_pos = 1.10)
      )
    )
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      input$semanal_e1_rangos,
      ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # E2 — GRÁFICA: LNE por grupos etarios (Jóvenes / Adultos / Mayores)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e2_grupos <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)   # "Nacional" o "Extranjero"
    
    # Fecha del último corte disponible (misma lógica que E1)
    serie <- datos_semanal_serie_edad()
    etiq_fecha <- if (!is.null(serie) && nrow(serie) > 0)
      fecha_es(max(serie$fecha))
    else
      as.character(anio)
    
    datos <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())
    
    df_rango <- construir_df_edad(datos, ambito)
    if (is.null(df_rango) || nrow(df_rango) == 0)
      return(plot_vacio("Sin datos de grupos de edad"))
    
    nombres_grupos <- names(GRUPOS_ETARIOS)
    colores_grupos <- if (ambito == "extranjero") COLOR_GRUPOS_EXT else COLOR_GRUPOS_NAC
    
    df_grupos <- do.call(rbind, lapply(seq_along(GRUPOS_ETARIOS), function(i) {
      rangos      <- GRUPOS_ETARIOS[[i]]
      etiq_rangos <- sapply(rangos, etiqueta_edad)
      filas       <- df_rango[df_rango$grupo %in% etiq_rangos, ]
      lne         <- sum(filas$lista_hombres + filas$lista_mujeres + filas$lista_no_binario, na.rm = TRUE)
      data.frame(grupo = nombres_grupos[i], lne = lne,
                 color = colores_grupos[i], stringsAsFactors = FALSE)
    }))
    
    total_lne     <- sum(df_grupos$lne, na.rm = TRUE)
    df_grupos$pct <- if (total_lne > 0) round(df_grupos$lne / total_lne * 100, 1) else 0
    
    x_max <- max(df_grupos$lne, na.rm = TRUE) * 1.32
    
    # Título con fecha dinámica y ámbito
    titulo_e2 <- paste0(
      "Lista Nominal Electoral por Grupo Etario — ",
      etiq_fecha, " — ", etiq
    )
    
    # Posiciones Y normalizadas para las 3 barras (de abajo a arriba: 0, 0.5, 1)
    # yref="paper" dentro del domain [0.20, 0.78]:
    #   barra 0 (Jóvenes)  → y_paper = 0.20 + 0/2 * 0.58 = 0.20
    #   barra 1 (Adultos)  → y_paper = 0.20 + 1/2 * 0.58 = 0.49
    #   barra 2 (Mayores)  → y_paper = 0.20 + 2/2 * 0.58 = 0.78
    # El factor rev() en el eje invierte el orden: Mayores arriba, Jóvenes abajo
    n_grupos   <- length(nombres_grupos)
    dom_lo     <- 0.20; dom_hi <- 0.78
    y_paper_pos <- dom_lo + ((seq_len(n_grupos) - 1) / (n_grupos - 1)) * (dom_hi - dom_lo)
    # rev: Mayores=index 1 está abajo (dom_lo), Jóvenes=index 3 arriba (dom_hi)
    # pero factor(rev(nombres_grupos)) pone Jóvenes arriba en el plot
    # entonces: nombres_grupos[1]=Jóvenes → y más alto → dom_hi
    y_paper_pos <- rev(y_paper_pos)
    
    etiq_y_anns <- lapply(seq_len(n_grupos), function(i) {
      list(
        text      = gsub("\n", "<br>", nombres_grupos[i]),
        x         = 0.155,              # justo a la izquierda del domain (0.18)
        y         = y_paper_pos[i],
        xref      = "paper",
        yref      = "paper",
        xanchor   = "right",
        yanchor   = "middle",
        showarrow = FALSE,
        font      = list(size = 12, color = "#333", family = "Arial, sans-serif"),
        align     = "right"
      )
    })
    
    plot_ly(
      data         = df_grupos,
      y            = ~factor(grupo, levels = rev(nombres_grupos)),
      x            = ~lne,
      type         = "bar",
      orientation  = "h",
      marker       = list(color = ~color),
      text         = ~paste0(format(lne, big.mark = ","), "  (", pct, "%)"),
      textposition = "outside",
      cliponaxis   = FALSE,
      hovertemplate = "<b>%{y}</b><br>LNE: %{x:,.0f}<extra></extra>"
    ) %>%
      layout(
        title = list(
          text = titulo_e2,
          font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
          x    = 0.5, xanchor = "center"
        ),
        xaxis = list(
          title    = "",
          range    = c(0, x_max),
          tickfont = list(size = 11),
          domain   = c(0.18, 1)
        ),
        yaxis = list(
          title          = "",
          showticklabels = FALSE,    # ocultamos ticks nativos; usamos anotaciones
          domain         = c(0.20, 0.78)
        ),
        showlegend = FALSE,
        margin     = list(t = 60, b = 60, l = 20, r = 90),
        hovermode  = "closest",
        annotations = c(
          etiq_y_anns,
          list(
            list(
              text    = alcance,
              x = 0.5, y = 0.96,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "top",
              showarrow = FALSE,
              font = list(size = 13, color = "#555555", family = "Arial, sans-serif"),
              align = "center"
            ),
            list(
              text = FUENTE_INE,
              x = 0.5, y = 0.05,
              xref = "paper", yref = "paper",
              xanchor = "center", yanchor = "top",
              showarrow = FALSE,
              font = list(size = 10, color = "#666666", family = "Arial, sans-serif"),
              align = "center"
            )
          )
        )
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # E3 — WIDGET: selector de grupos etarios + botones Restablecer y Metodología
  # Layout: 3 checks en fila única + botones al lado derecho
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e3_grupos_ui <- renderUI({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    tagList(
      tags$style(HTML(paste0(
        "#", session$ns("semanal_e3_grupos"),
        " .shiny-options-group { display:flex; flex-wrap:nowrap; gap:4px 0; }",
        "#", session$ns("semanal_e3_grupos"),
        " .shiny-options-group .checkbox { margin:0; padding:1px 12px 1px 4px;",
        " box-sizing:border-box; }",
        "#", session$ns("semanal_e3_grupos"),
        " label { font-size:12px; color:#333; cursor:pointer; }",
        "#", session$ns("semanal_e3_grupos"),
        " input[type='checkbox'] { accent-color:#277592; cursor:pointer; }"
      ))),
      
      div(
        style = paste(
          "background:#f4f6f8;border:1px solid #d0d7de;border-radius:7px;",
          "padding:10px 14px 8px 14px;margin-bottom:10px;"
        ),
        div(
          style = "display:flex;align-items:center;gap:12px;",
          
          # Izquierda: label + 3 checks en fila única
          div(
            style = "flex:1;min-width:0;",
            div(
              style = "margin-bottom:5px;",
              tags$span(
                style = "font-size:11px;font-weight:600;color:#555;",
                "Grupos etarios:"
              )
            ),
            checkboxGroupInput(
              inputId  = session$ns("semanal_e3_grupos"),
              label    = NULL,
              choices  = setNames(GRUPOS_E3, ETIQ_GRUPOS_E3[GRUPOS_E3]),
              selected = GRUPOS_E3,
              inline   = FALSE
            )
          ),
          
          # Derecha: botones apilados
          div(
            style = "display:flex;flex-direction:column;gap:5px;flex-shrink:0;",
            actionButton(
              inputId = session$ns("semanal_e3_btn_reset"),
              label   = tagList(icon("undo"), " Restablecer"),
              class   = "btn btn-sm",
              style   = paste(
                "background-color:#6c757d;color:#fff;border:none;",
                "font-size:11px;padding:4px 10px;border-radius:4px;",
                "white-space:nowrap;width:110px;"
              )
            ),
            actionButton(
              inputId = session$ns("semanal_e3_btn_metodologia"),
              label   = tagList(icon("info-circle"), " Metodología"),
              class   = "btn btn-sm btn-outline-info",
              style   = paste(
                "font-size:11px;padding:4px 10px;border-radius:4px;",
                "white-space:nowrap;width:110px;"
              )
            )
          )
        )
      )
    )
  })
  
  observeEvent(input$semanal_e3_btn_reset, {
    updateCheckboxGroupInput(
      session = session,
      inputId = "semanal_e3_grupos",
      selected = GRUPOS_E3
    )
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  observeEvent(input$semanal_e3_btn_metodologia, {
    showModal(modalDialog(
      title     = tagList(icon("chart-line"), " Metodología de Proyección"),
      size      = "l",
      easyClose = TRUE,
      footer    = tagList(
        tags$button(
          type = "button", class = "btn btn-primary",
          style = "background-color:#277592;border-color:#277592;padding:8px 32px;font-size:14px;border-radius:6px;",
          `data-dismiss` = "modal", "Cerrar"
        )
      ),
      tagList(
        div(
          style = "margin-bottom:16px;",
          h4(tags$span(style = "color:#1a3a5c;font-weight:700;",
                       "¿Cómo se calcula la proyección?"),
             style = "margin-top:0;"),
          p("La proyección utiliza un ",
            tags$strong("modelo de tasa de crecimiento mensual promedio"),
            " basado en los cortes semanales disponibles del año en curso.")
        ),
        h5(tags$span(style = "color:#1a3a5c;font-weight:700;", "Pasos del cálculo:"),
           style = "margin-bottom:8px;"),
        tags$ol(
          style = "padding-left:24px;margin-bottom:12px;",
          tags$li(tagList(tags$strong("Datos base:"),       " Cortes semanales del año actual.")),
          tags$li(tagList(tags$strong("Tasa de crecimiento:"), " Promedio mensual entre el primer y último corte disponible.")),
          tags$li(tagList(tags$strong("Proyección:"),       " Se aplica la tasa compuesta hasta diciembre.")),
          tags$li(tagList(tags$strong("Visualización:"),    " Líneas punteadas = valores proyectados."))
        ),
        tags$hr(),
        h5(tags$span(style = "color:#1a3a5c;font-weight:700;", "Fórmula:"),
           style = "margin-bottom:8px;"),
        div(
          style = "background:#eef4fb;border-left:4px solid #277592;padding:10px 16px;border-radius:4px;font-family:monospace;font-size:13px;color:#D10F3F;margin-bottom:16px;",
          div("Tasa = (Valor_final / Valor_inicial)^(1/(n-1)) — 1"),
          div("Proyección(i) = Último_valor × (1 + tasa)^i")
        ),
        div(style = "color:#c0392b;font-weight:700;margin-bottom:6px;",
            icon("triangle-exclamation"), " Consideraciones:"),
        tags$ul(
          style = "padding-left:24px;margin-bottom:12px;",
          tags$li(tagList("Asume crecimiento ", tags$strong("constante"), ".")),
          tags$li(tagList("Es una ", tags$strong("estimación estadística"), ".")),
          tags$li(tagList("Proyecta hasta ", tags$strong("diciembre"), " del año.")),
          tags$li("Los datos oficiales del INE prevalecen.")
        ),
        tags$hr(),
        div(
          style = "text-align:center;font-size:12px;color:#666;",
          icon("circle-info"),
          " Esta es una herramienta de referencia. Los datos oficiales son los publicados por el INE."
        )
      )
    ))
  }, ignoreNULL = TRUE, ignoreInit = TRUE)
  
  # ══════════════════════════════════════════════════════════════════════════
  # E3 — GRÁFICA: evolución y proyección semanal por grupo etario
  # Agrega rangos en: Jóvenes / Adultos / Mayores
  # Misma mecánica que E1; paleta = COLOR_GRUPOS_NAC / COLOR_GRUPOS_EXT
  # ══════════════════════════════════════════════════════════════════════════
  
  # Colores de línea para E3: cada grupo tiene un color para Padrón y otro para LNE
  # Nacional: Jóvenes=azul claro, Adultos=azul oscuro, Mayores=rojo
  # Extranjero: Jóvenes=dorado claro, Adultos=dorado oscuro, Mayores=verde
  COLOR_E3_PAD_NAC <- c(jovenes = "#6BA4C6", adultos = "#277592", mayores = "#D10F3F")
  COLOR_E3_LNE_NAC <- c(jovenes = "#4891B3", adultos = "#003E66", mayores = "#8B0A2B")
  COLOR_E3_PAD_EXT <- c(jovenes = "#FFDE6F", adultos = "#EAC43E", mayores = "#7EAF5A")
  COLOR_E3_LNE_EXT <- c(jovenes = "#F5CA45", adultos = "#D6B632", mayores = "#518033")
  
  output$semanal_e3_proyeccion_grupos <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    serie <- datos_serie_edad_efectiva()
    if (is.null(serie) || nrow(serie) < 2)
      return(plot_vacio("Sin datos de serie temporal para proyección"))
    
    ultima_fecha <- max(serie$fecha)
    etiq_fecha   <- fecha_es(ultima_fecha)
    ultimo_mes   <- as.integer(format(ultima_fecha, "%m"))
    meses_rest   <- 12L - ultimo_mes
    
    grupos_sel <- input$semanal_e3_grupos %||% GRUPOS_E3
    if (length(grupos_sel) == 0) grupos_sel <- GRUPOS_E3
    
    pal_pad <- if (ambito == "extranjero") COLOR_E3_PAD_EXT else COLOR_E3_PAD_NAC
    pal_lne <- if (ambito == "extranjero") COLOR_E3_LNE_EXT else COLOR_E3_LNE_NAC
    
    p <- plot_ly()
    
    for (grupo_key in grupos_sel) {
      rangos   <- RANGOS_GRUPOS_E3[[grupo_key]]
      etiq_g   <- ETIQ_GRUPOS_E3[grupo_key]
      col_pad  <- pal_pad[grupo_key]
      col_lne  <- pal_lne[grupo_key]
      
      # Sumar columnas de la serie para este grupo
      cols_pad <- paste0("padron_", rangos)
      cols_lne <- paste0("lista_",  rangos)
      cols_pad_ok <- cols_pad[cols_pad %in% colnames(serie)]
      cols_lne_ok <- cols_lne[cols_lne %in% colnames(serie)]
      
      if (length(cols_pad_ok) == 0) next
      
      serie_g <- serie
      serie_g$padron_grupo <- rowSums(serie[, cols_pad_ok, drop = FALSE], na.rm = TRUE)
      serie_g$lista_grupo  <- if (length(cols_lne_ok) > 0)
        rowSums(serie[, cols_lne_ok, drop = FALSE], na.rm = TRUE) else 0
      
      proy_g <- NULL
      if (meses_rest > 0) {
        sg                  <- serie_g
        sg$padron_electoral <- sg$padron_grupo
        sg$lista_nominal    <- sg$lista_grupo
        proy_g <- tryCatch(proyectar_con_tasa_crecimiento(sg, meses_rest),
                           error = function(e) NULL)
      }
      
      p <- p %>%
        add_trace(
          data = serie_g, x = ~fecha, y = ~padron_grupo,
          type = "scatter", mode = "lines+markers",
          name = paste0("Padrón ", etiq_g),
          line   = list(color = col_pad, width = 3),
          marker = list(size = 7, color = col_pad, symbol = "circle"),
          hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Padrón ", etiq_g,
                                 ": %{y:,.0f}<extra></extra>")
        ) %>%
        add_trace(
          data = serie_g, x = ~fecha, y = ~lista_grupo,
          type = "scatter", mode = "lines+markers",
          name = paste0("LNE ", etiq_g),
          line   = list(color = col_lne, width = 3, dash = "dot"),
          marker = list(size = 7, color = col_lne, symbol = "square"),
          hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>LNE ", etiq_g,
                                 ": %{y:,.0f}<extra></extra>")
        )
      
      if (!is.null(proy_g) && nrow(proy_g) > 0) {
        p <- p %>%
          add_trace(
            data = proy_g, x = ~fecha, y = ~padron_proyectado,
            type = "scatter", mode = "lines",
            name = paste0("Proy. Padrón ", etiq_g),
            line = list(color = col_pad, width = 2, dash = "dash"),
            showlegend = FALSE,
            hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Proy. Padrón ", etiq_g,
                                   ": %{y:,.0f}<extra></extra>")
          ) %>%
          add_trace(
            data = proy_g, x = ~fecha, y = ~lista_proyectada,
            type = "scatter", mode = "lines",
            name = paste0("Proy. LNE ", etiq_g),
            line = list(color = col_lne, width = 2, dash = "dashdot"),
            showlegend = FALSE,
            hovertemplate = paste0("<b>%{x|%d %b %Y}</b><br>Proy. LNE ", etiq_g,
                                   ": %{y:,.0f}<extra></extra>")
          )
      }
    }
    
    p %>% layout(
      title = list(
        text = paste0("Evolución y Proyección Semanal por Grupo Etario — ",
                      etiq_fecha, " — ", etiq),
        font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
        x = 0.5, xanchor = "center"
      ),
      xaxis = list(
        title      = "",
        type       = "date",
        tickformat = "%d %b",
        range      = c(min(serie$fecha) - 3,
                       as.Date(paste0(anio, "-12-31")))
      ),
      yaxis = list(
        title             = "Número de electores",
        separatethousands = TRUE
      ),
      legend = list(
        orientation = "h",
        xanchor     = "center",
        x           = 0.5,
        y           = -0.10,
        font        = list(size = 11)
      ),
      margin    = list(t = 120, b = 80, l = 90, r = 40),
      hovermode = "x unified",
      annotations = list(
        ann_alcance(alcance, y_pos = 1.10)
      )
    )
  }) %>%
    bindEvent(
      estado_app(),
      input$btn_consultar,
      input$semanal_e3_grupos,
      ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  # ══════════════════════════════════════════════════════════════════════════
  # E4 — GRÁFICA: Padrón y LNE por rango individual (barras agrupadas)
  # ══════════════════════════════════════════════════════════════════════════
  
  output$semanal_e4_barras <- renderPlotly({
    if (es_historico() || desglose_activo() != "edad") return(NULL)
    
    ambito  <- ambito_reactivo()
    alcance <- isolate(texto_alcance())
    anio    <- anio_semanal()
    etiq    <- etiq_ambito(ambito)
    
    # Fecha del último corte disponible (mismo patrón que E1/E3)
    serie      <- datos_semanal_serie_edad()
    etiq_fecha <- if (!is.null(serie) && nrow(serie) > 0)
      fecha_es(max(serie$fecha))
    else
      as.character(anio)
    
    datos <- datos_semanal_edad()
    if (is.null(datos)) return(plot_vacio())
    
    df <- construir_df_edad(datos, ambito)
    if (is.null(df) || nrow(df) == 0) return(plot_vacio("Sin datos de edad"))
    
    df$padron_total <- df$padron_hombres + df$padron_mujeres + df$padron_no_binario
    df$lista_total  <- df$lista_hombres  + df$lista_mujeres  + df$lista_no_binario
    
    col_pad <- color_total_pad(ambito)
    col_lne <- color_total_lne(ambito)
    
    plot_ly() %>%
      add_trace(
        data = df, x = ~grupo, y = ~padron_total,
        type = "bar", name = "Padrón Electoral",
        marker = list(color = col_pad),
        hovertemplate = "<b>%{x}</b><br>Padrón: %{y:,.0f}<extra></extra>"
      ) %>%
      add_trace(
        data = df, x = ~grupo, y = ~lista_total,
        type = "bar", name = "Lista Nominal",
        marker = list(color = col_lne),
        hovertemplate = "<b>%{x}</b><br>LNE: %{y:,.0f}<extra></extra>"
      ) %>%
      layout(
        title   = list(
          text = paste0("Padrón y LNE por Rango de Edad — ", etiq_fecha, " — ", etiq),
          font = list(size = 16, color = "#333", family = "Arial, sans-serif"),
          x = 0.5, xanchor = "center"
        ),
        barmode = "group",
        xaxis   = list(title = "", tickangle = 0, tickfont = list(size = 11)),
        yaxis   = list(title = "Número de electores", separatethousands = TRUE),
        legend  = list(orientation = "h", xanchor = "center", x = 0.5, y = -0.10,
                       font = list(size = 11)),
        margin  = list(t = 120, b = 80, l = 90, r = 40),
        hovermode = "closest",
        annotations = list(
          ann_alcance(alcance, y_pos = 1.10)
        )
      )
  }) %>%
    bindEvent(
      estado_app(), input$btn_consultar, ambito_reactivo(),
      ignoreNULL = FALSE, ignoreInit = FALSE
    )
  
  message("✅ graficas_semanal_edad v1.8 inicializado")
  message("   E1: evolución/proyección por rango de edad — responde a filtros geográficos")
  message("   E2: LNE por grupos etarios (barras horizontales)")
  message("   E3: evolución/proyección por grupo etario — responde a filtros geográficos")
  message("   E4: Padrón y LNE por rango individual (barras agrupadas)")
}
