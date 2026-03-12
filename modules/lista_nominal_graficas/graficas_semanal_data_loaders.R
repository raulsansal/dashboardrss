# modules/lista_nominal_graficas/graficas_semanal_data_loaders.R
# Versión: 3.8 — Fix definitivo: Nacional=suma secciones; Extranjero=suma 32 EXT
#
# CAMBIOS vs v3.0:
#   - extraer_fila_agregada(): corregido bug donde filas de RESIDENTES EXTRANJERO
#     con nombre_entidad = nombre de la entidad (no el texto especial) se incluían
#     en la suma nacional/estatal. Ahora se excluyen también por firma estructural:
#     cve_distrito=0, cve_municipio=0, seccion=0 (y no es TOTALES).
#     Esto eliminaba ~81k electores fantasma en consultas por entidad (ej. CDMX)
#     y causaba desfase entre E1/E3 y el DataTable/E2/E4.
#   - Para ámbito "extranjero": se agrega detección por firma si no hay texto explícito.
#   - Resto sin cambios vs v3.0.
#
# CAMBIOS vs v2.1:
#   - PRE-CARGA EN BACKGROUND: las 3 series (edad, sexo, origen) se inician
#     silenciosamente mediante later::later() con retardo escalonado (3s / 5s / 7s)
#     mientras el usuario está en la vista Histórico (1-3 min típicos).
#     Al cambiar a Semanal o cambiar de desglose, los datos ya están en caché
#     → carga instantánea (cero espera).
#   - SPINNER SIMPLE: si el usuario llega a un desglose antes de que su serie
#     termine de pre-cargarse, el reactive espera con un withProgress de mensaje
#     fijo ("Cargando datos semanales...") sin detalle incremental.
#   - FIX FECHAS FUTURAS: obtener_todas_fechas_semanal() filtra fechas > Sys.Date()
#     para evitar intentos de descarga de archivos aún no publicados en Firebase
#     (404s silenciosos que consumían tiempo en el loop).
#   - Sin cambios en: corte único (datos_semanal_edad/sexo/origen), helpers de
#     agregación, fecha_semanal_efectiva, anio_semanal, ni interfaz de retorno.

graficas_semanal_data_loaders <- function(input, output, session,
                                          estado_app,
                                          filtros_usuario,
                                          ambito_reactivo) {
  
  message("📥 Inicializando graficas_semanal_data_loaders v3.0")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CONSTANTES
  # ══════════════════════════════════════════════════════════════════════════
  
  ORDEN_EDAD <- c("18","19","20_24","25_29","30_34","35_39",
                  "40_44","45_49","50_54","55_59","60_64","65_y_mas")
  
  # ══════════════════════════════════════════════════════════════════════════
  # CACHÉ GLOBAL
  # Claves de series: serie_edad_nacional, serie_edad_extranjero,
  #                   serie_sexo_nacional, serie_sexo_extranjero,
  #                   serie_origen_nacional, serie_origen_extranjero
  # ══════════════════════════════════════════════════════════════════════════
  
  if (!exists("LNE_CACHE_SEMANAL", envir = .GlobalEnv)) {
    assign("LNE_CACHE_SEMANAL",
           list(edad       = NULL,
                sexo       = NULL,
                origen     = NULL,
                serie_edad_nacional    = NULL,
                serie_edad_extranjero  = NULL,
                serie_sexo_nacional    = NULL,
                serie_sexo_extranjero  = NULL,
                serie_origen_nacional  = NULL,
                serie_origen_extranjero = NULL,
                fecha      = NULL,
                timestamp  = NULL),
           envir = .GlobalEnv)
    message("📦 Caché semanal inicializado (v3.0)")
  } else {
    # Asegurar que las claves nuevas existan si el caché fue creado por v<3.0
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    for (clave in c("serie_edad_nacional","serie_edad_extranjero",
                    "serie_sexo_nacional","serie_sexo_extranjero",
                    "serie_origen_nacional","serie_origen_extranjero")) {
      if (is.null(cache[[clave]])) cache[[clave]] <- NULL
    }
    assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPERS
  # ══════════════════════════════════════════════════════════════════════════
  
  cache_semanal_valido <- function(fecha_requerida) {
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (is.null(cache$timestamp) || is.null(cache$fecha)) return(FALSE)
    if (!identical(cache$fecha, fecha_requerida)) return(FALSE)
    difftime(Sys.time(), cache$timestamp, units = "hours") < 24
  }
  
  cache_serie_valido <- function(clave) {
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    if (is.null(cache[[clave]]) || is.null(cache$timestamp)) return(FALSE)
    difftime(Sys.time(), cache$timestamp, units = "hours") < 24
  }
  
  obtener_fecha_semanal_reciente <- function() {
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    if (length(catalog$semanal_comun) == 0) return(NULL)
    as.Date(max(catalog$semanal_comun), origin = "1970-01-01")
  }
  
  # ✅ v3.0 FIX: filtra fechas futuras para evitar 404s en Firebase
  obtener_todas_fechas_semanal <- function() {
    if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
    catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
    if (length(catalog$semanal_comun) == 0) return(NULL)
    fechas <- sort(as.Date(catalog$semanal_comun, origin = "1970-01-01"))
    fechas <- fechas[fechas <= Sys.Date()]   # ← fix: no intentar fechas futuras
    if (length(fechas) == 0) return(NULL)
    fechas
  }
  
  es_nacional_sin_filtros <- function(f) {
    f$entidad == "Nacional" && f$distrito == "Todos" && f$municipio == "Todos" &&
      (is.null(f$seccion) || "Todas" %in% f$seccion || length(f$seccion) == 0)
  }
  
  # Devuelve list(datos=df, totales=list_o_NULL)
  # totales viene de cargar_lne()$totales — fila TOTALES del CSV ya procesada,
  # que cargar_lne() extrae y normaliza antes de devolver el df sin esa fila.
  cargar_tipo_semanal <- function(fecha, tipo, filtros) {
    tryCatch({
      res <- cargar_lne(tipo_corte = "semanal", fecha = fecha, dimension = tipo,
                        estado     = filtros$entidad   %||% "Nacional",
                        distrito   = filtros$distrito  %||% "Todos",
                        municipio  = filtros$municipio %||% "Todos",
                        seccion    = filtros$seccion   %||% "Todas",
                        incluir_extranjero = TRUE)
      if (!is.null(res) && !is.null(res$datos) && nrow(res$datos) > 0) {
        return(list(datos = res$datos, totales = res$totales))
      }
      return(NULL)
    }, error = function(e) {
      message("❌ Error semanal/", tipo, " (", format(fecha, "%Y%m%d"), "): ", e$message)
      return(NULL)
    })
  }
  
  filtros_defecto <- list(entidad = "Nacional", distrito = "Todos",
                          municipio = "Todos",  seccion  = "Todas")
  
  # ══════════════════════════════════════════════════════════════════════════
  # HELPERS DE AGREGACIÓN
  # ══════════════════════════════════════════════════════════════════════════
  
  # Identificador canónico de fila RESIDENTES EXTRANJERO:
  #   cabecera_distrital == "RESIDENTES EXTRANJERO"
  # (independiente de nombre_entidad, que conserva el nombre del estado)
  es_fila_extranjero <- function(df) {
    if (!"cabecera_distrital" %in% colnames(df)) return(rep(FALSE, nrow(df)))
    grepl("RESIDENTES EXTRANJERO", toupper(trimws(df$cabecera_distrital)), fixed = TRUE)
  }
  
  # extraer_fila_agregada(): devuelve UNA fila agregada para la serie
  #   Nacional   → suma las secciones (sin EXT, sin TOTALES)
  #   Extranjero → suma las 32 filas EXT
  #
  # NOTA: totales_norm (parámetro heredado) ya no se usa — se ignora.
  # La lógica es siempre sumar las filas correctas del df.
  extraer_fila_agregada <- function(df, ambito, totales_norm = NULL) {
    if (is.null(df) || nrow(df) == 0) return(NULL)
    
    cols_id  <- c("cve_entidad","nombre_entidad","cve_distrito","cabecera_distrital",
                  "cve_municipio","nombre_municipio","seccion")
    
    if (ambito == "extranjero") {
      filas <- df[es_fila_extranjero(df), , drop = FALSE]
    } else {
      # Secciones nacionales: excluir EXT y fila TOTALES (cve_entidad=NA)
      mask_excl <- es_fila_extranjero(df)
      if ("cve_entidad" %in% colnames(df)) mask_excl <- mask_excl | is.na(df$cve_entidad)
      filas <- df[!mask_excl, , drop = FALSE]
    }
    
    if (is.null(filas) || nrow(filas) == 0) return(NULL)
    
    cols_num <- setdiff(colnames(filas), cols_id)
    cols_num <- cols_num[sapply(filas[, cols_num, drop = FALSE], function(x)
      is.numeric(x) || suppressWarnings(!any(is.na(as.numeric(x)))))]
    
    ag <- as.data.frame(
      lapply(filas[, cols_num, drop = FALSE], function(x) sum(as.numeric(x), na.rm = TRUE)),
      stringsAsFactors = FALSE)
    ag$nombre_entidad <- if (ambito == "extranjero") "RESIDENTES EXTRANJERO" else "SECCIONES_NAC"
    ag
  }
  construir_fila_serie_edad <- function(df, fecha, ambito, totales_norm = NULL) {
    fila <- extraer_fila_agregada(df, ambito, totales_norm)
    if (is.null(fila)) return(NULL)
    
    resultado <- list(fecha = fecha)
    
    for (rango in ORDEN_EDAD) {
      col_ph <- paste0("padron_", rango, "_hombres")
      col_pm <- paste0("padron_", rango, "_mujeres")
      col_pn <- paste0("padron_", rango, "_no_binario")
      col_lh <- paste0("lista_", rango, "_hombres")
      col_lm <- paste0("lista_", rango, "_mujeres")
      col_ln <- paste0("lista_", rango, "_no_binario")
      
      val_pad <- sum(
        if (col_ph %in% colnames(fila)) as.numeric(fila[[col_ph]]) else 0,
        if (col_pm %in% colnames(fila)) as.numeric(fila[[col_pm]]) else 0,
        if (col_pn %in% colnames(fila)) as.numeric(fila[[col_pn]]) else 0,
        na.rm = TRUE
      )
      val_lst <- sum(
        if (col_lh %in% colnames(fila)) as.numeric(fila[[col_lh]]) else 0,
        if (col_lm %in% colnames(fila)) as.numeric(fila[[col_lm]]) else 0,
        if (col_ln %in% colnames(fila)) as.numeric(fila[[col_ln]]) else 0,
        na.rm = TRUE
      )
      
      resultado[[paste0("padron_", rango)]] <- val_pad
      resultado[[paste0("lista_",  rango)]] <- val_lst
    }
    
    resultado$padron_total <- sum(unlist(resultado[paste0("padron_", ORDEN_EDAD)]), na.rm = TRUE)
    resultado$lista_total  <- sum(unlist(resultado[paste0("lista_",  ORDEN_EDAD)]), na.rm = TRUE)
    
    as.data.frame(resultado, stringsAsFactors = FALSE)
  }
  
  construir_fila_serie_sexo <- function(df, fecha, ambito, totales_norm = NULL) {
    fila <- extraer_fila_agregada(df, ambito, totales_norm)
    if (is.null(fila)) return(NULL)
    
    get_col <- function(col) {
      if (col %in% colnames(fila)) {
        v <- as.numeric(fila[[col]])
        if (is.na(v)) 0 else v
      } else 0
    }
    
    data.frame(
      fecha             = fecha,
      padron_hombres    = get_col("padron_hombres"),
      padron_mujeres    = get_col("padron_mujeres"),
      padron_no_binario = get_col("padron_no_binario"),
      lista_hombres     = get_col("lista_hombres"),
      lista_mujeres     = get_col("lista_mujeres"),
      lista_no_binario  = get_col("lista_no_binario"),
      stringsAsFactors  = FALSE
    )
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # FUNCIÓN INTERNA: cargar_serie_con_progreso()
  #
  # Carga todas las semanas para un tipo dado (edad/sexo/origen) mostrando
  # una barra de progreso incremental. Guarda el resultado en caché global.
  # Retorna el data.frame de la serie o NULL si no hay datos.
  #
  # Parámetros:
  #   tipo        "edad" | "sexo" | "origen"
  #   ambito      "nacional" | "extranjero"
  #   con_progreso TRUE = withProgress (llamada desde reactive en sesión activa)
  #                FALSE = sin UI de progreso (pre-carga en background)
  # ══════════════════════════════════════════════════════════════════════════
  
  # ══════════════════════════════════════════════════════════════════════════
  # FUNCIÓN INTERNA: cargar_serie_con_progreso()
  #
  # Carga todas las semanas para un tipo dado (edad/sexo/origen).
  # Verifica caché primero → si válido, retorno inmediato.
  # Si hay que cargar, ejecuta el loop:
  #   con_progreso = TRUE  → withProgress con spinner simple y mensaje fijo
  #                          (llamada desde reactive con sesión activa)
  #   con_progreso = FALSE → sin UI de progreso (pre-carga en background)
  # Guarda el resultado en caché global al terminar.
  # ══════════════════════════════════════════════════════════════════════════
  
  cargar_serie_con_progreso <- function(tipo, ambito, con_progreso = TRUE) {
    
    clave_cache <- paste0("serie_", tipo, "_", ambito)
    
    # Verificar caché primero → retorno inmediato si válido
    if (cache_serie_valido(clave_cache)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      message("✅ [CACHÉ] serie_", tipo, " (", ambito, ") — ",
              nrow(cache[[clave_cache]]), " semanas")
      return(cache[[clave_cache]])
    }
    
    fechas <- obtener_todas_fechas_semanal()
    if (is.null(fechas) || length(fechas) == 0) {
      message("⚠️ [serie_", tipo, "] Sin fechas disponibles")
      return(NULL)
    }
    
    n_total <- length(fechas)
    message("📥 [serie_", tipo, "] Cargando ", n_total,
            " semanas para ámbito=", ambito, "...")
    
    filas <- list()
    n_ok  <- 0L
    n_err <- 0L
    
    # Helper para construir cada fila según tipo
    construir_fila <- switch(tipo,
                             "edad"   = construir_fila_serie_edad,
                             "sexo"   = construir_fila_serie_sexo,
                             "origen" = construir_fila_serie_origen
    )
    
    cargar_loop <- function() {
      for (i in seq_along(fechas)) {
        fecha_d <- as.Date(fechas[[i]], origin = "1970-01-01")
        res_sem <- cargar_tipo_semanal(fecha_d, tipo, filtros_defecto)
        if (is.null(res_sem)) { n_err <<- n_err + 1L; next }
        # Desempaquetar: res_sem es list(datos, totales)
        df      <- res_sem$datos
        totales <- res_sem$totales   # lista normalizada o NULL
        if (is.null(df)) { n_err <<- n_err + 1L; next }
        fila <- construir_fila(df, fecha_d, ambito, totales)
        if (!is.null(fila)) {
          filas[[length(filas) + 1]] <<- fila
          n_ok <<- n_ok + 1L
        } else {
          n_err <<- n_err + 1L
        }
      }
    }
    
    # Spinner simple con mensaje fijo (sin detalle por semana)
    if (con_progreso) {
      shiny::withProgress(
        message = "Cargando datos semanales...",
        value   = NULL,
        cargar_loop()
      )
    } else {
      cargar_loop()
    }
    
    if (length(filas) == 0) {
      message("❌ [serie_", tipo, "] Sin datos válidos en ninguna semana")
      return(NULL)
    }
    
    serie <- if (tipo == "origen") {
      tryCatch(
        dplyr::bind_rows(filas),
        error = function(e) do.call(rbind, filas)
      )
    } else {
      do.call(rbind, filas)
    }
    
    if (tipo == "origen") serie[is.na(serie)] <- 0
    serie <- serie[order(serie$fecha), ]
    rownames(serie) <- NULL
    
    # Guardar en caché
    cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
    cache[[clave_cache]] <- serie
    cache$timestamp      <- Sys.time()
    assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    
    message("✅ [serie_", tipo, "] ", nrow(serie), " semanas (OK=", n_ok,
            " ERR=", n_err, ")")
    message("   Rango: ", format(min(serie$fecha), "%Y-%m-%d"),
            " → ", format(max(serie$fecha), "%Y-%m-%d"))
    return(serie)
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # Helper para serie de origen — igual que v2.1, necesario para construir_fila
  # ══════════════════════════════════════════════════════════════════════════
  
  construir_fila_serie_origen <- function(df, fecha, ambito, totales_norm = NULL) {
    fila <- extraer_fila_agregada(df, ambito, totales_norm)
    if (is.null(fila)) return(NULL)
    
    resultado <- list(fecha = fecha)
    
    cols_pad <- grep("^pad_\\d{2}$|^pad8[78]$", colnames(fila),
                     value = TRUE, ignore.case = TRUE)
    cols_ln  <- grep("^ln_\\d{2}$|^ln8[78]$",  colnames(fila),
                     value = TRUE, ignore.case = TRUE)
    
    if (length(cols_pad) == 0)
      cols_pad <- grep("^padron_\\d{2}$|^padron_8[78]$", colnames(fila),
                       value = TRUE, ignore.case = TRUE)
    if (length(cols_ln) == 0)
      cols_ln  <- grep("^lista_\\d{2}$|^lista_8[78]$|^ln_\\d", colnames(fila),
                       value = TRUE, ignore.case = TRUE)
    
    for (col in cols_pad) {
      resultado[[col]] <- as.numeric(fila[[col]])
      if (is.na(resultado[[col]])) resultado[[col]] <- 0
    }
    for (col in cols_ln) {
      resultado[[col]] <- as.numeric(fila[[col]])
      if (is.na(resultado[[col]])) resultado[[col]] <- 0
    }
    
    if (length(resultado) == 1) return(NULL)
    as.data.frame(resultado, stringsAsFactors = FALSE)
  }
  
  # ══════════════════════════════════════════════════════════════════════════
  # ══════════════════════════════════════════════════════════════════════════
  # CARGA BAJO DEMANDA — sin pre-carga en background
  #
  # Las series semanales se cargan cuando el usuario entra a la pestaña:
  #   · datos_semanal_serie_edad   → al cambiar tipo_corte a "semanal"
  #   · datos_semanal_serie_sexo   → al seleccionar desglose Sexo
  #   · datos_semanal_serie_origen → al seleccionar desglose Origen
  # Esto garantiza que Histórico esté disponible de inmediato al arrancar.
  # ══════════════════════════════════════════════════════════════════════════
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVE: fecha_semanal_efectiva — sin cambios vs v2.1
  # ══════════════════════════════════════════════════════════════════════════
  
  fecha_semanal_efectiva <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    estado_actual <- estado_app()
    if (estado_actual %in% c("restablecido", "inicial")) {
      fecha <- obtener_fecha_semanal_reciente()
      message("📅 [semanal] estado=", estado_actual, " → fecha: ", fecha)
      return(fecha)
    }
    if (estado_actual == "consultado") {
      btn      <- input$btn_consultar
      year_sel <- isolate(input$year)
      if (is.null(year_sel) || is.na(year_sel)) return(obtener_fecha_semanal_reciente())
      if (!exists("LNE_CATALOG", envir = .GlobalEnv)) return(NULL)
      catalog <- get("LNE_CATALOG", envir = .GlobalEnv)
      fechas  <- catalog$semanal_comun[
        format(catalog$semanal_comun, "%Y") == as.character(year_sel)]
      if (length(fechas) == 0) { message("⚠️ Sin fechas para año ", year_sel); return(NULL) }
      fecha <- as.Date(max(fechas), origin = "1970-01-01")
      message("📅 [semanal] CONSULTADO → ", fecha)
      return(fecha)
    }
    return(NULL)
  })
  
  anio_semanal <- reactive({
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(as.integer(format(Sys.Date(), "%Y")))
    as.integer(format(fecha, "%Y"))
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVES: corte único — sin cambios vs v2.1
  # ══════════════════════════════════════════════════════════════════════════
  
  datos_semanal_edad <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") {
      btn <- input$btn_consultar; isolate(filtros_usuario())
    } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$edad)) { message("✅ [CACHÉ] edad semanal"); return(cache$edad) }
    }
    message("📥 [semanal_edad] ", format(fecha, "%Y%m%d"))
    res_sem <- cargar_tipo_semanal(fecha, "edad", filtros)
    datos <- if (!is.null(res_sem)) res_sem$datos else NULL
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$edad <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_edad] ", nrow(datos), " filas")
    return(datos)
  })
  
  datos_semanal_sexo <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") {
      btn <- input$btn_consultar; isolate(filtros_usuario())
    } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$sexo)) { message("✅ [CACHÉ] sexo semanal"); return(cache$sexo) }
    }
    message("📥 [semanal_sexo] ", format(fecha, "%Y%m%d"))
    res_sem <- cargar_tipo_semanal(fecha, "sexo", filtros)
    datos <- if (!is.null(res_sem)) res_sem$datos else NULL
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$sexo <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_sexo] ", nrow(datos), " filas")
    return(datos)
  })
  
  datos_semanal_origen <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    fecha <- fecha_semanal_efectiva()
    if (is.null(fecha)) return(NULL)
    estado_actual <- estado_app()
    filtros <- if (estado_actual == "consultado") {
      btn <- input$btn_consultar; isolate(filtros_usuario())
    } else filtros_defecto
    if (es_nacional_sin_filtros(filtros) && cache_semanal_valido(fecha)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      if (!is.null(cache$origen)) { message("✅ [CACHÉ] origen semanal"); return(cache$origen) }
    }
    message("📥 [semanal_origen] ", format(fecha, "%Y%m%d"))
    res_sem <- cargar_tipo_semanal(fecha, "origen", filtros)
    datos <- if (!is.null(res_sem)) res_sem$datos else NULL
    if (!is.null(datos) && es_nacional_sin_filtros(filtros)) {
      cache <- get("LNE_CACHE_SEMANAL", envir = .GlobalEnv)
      cache$origen <- datos; cache$fecha <- fecha; cache$timestamp <- Sys.time()
      assign("LNE_CACHE_SEMANAL", cache, envir = .GlobalEnv)
    }
    if (!is.null(datos)) message("✅ [semanal_origen] ", nrow(datos), " filas")
    return(datos)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # REACTIVES: series temporales — con spinner simple (v3.0)
  #
  # Cada reactive verifica caché primero (resultado de pre-carga en background).
  # Si los datos ya están → retorno inmediato (cero espera).
  # Si aún no están (usuario llegó antes de que terminara la pre-carga) →
  # spinner simple con mensaje fijo mientras termina la carga.
  # ══════════════════════════════════════════════════════════════════════════
  
  datos_semanal_serie_edad <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    ambito <- ambito_reactivo()
    cargar_serie_con_progreso("edad", ambito, con_progreso = TRUE)
  })
  
  datos_semanal_serie_sexo <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    ambito <- ambito_reactivo()
    cargar_serie_con_progreso("sexo", ambito, con_progreso = TRUE)
  })
  
  datos_semanal_serie_origen <- reactive({
    tipo_corte_val <- input$tipo_corte %||% "historico"
    if (tipo_corte_val != "semanal") return(NULL)
    ambito <- ambito_reactivo()
    cargar_serie_con_progreso("origen", ambito, con_progreso = TRUE)
  })
  
  # ══════════════════════════════════════════════════════════════════════════
  # RETORNO — v3.0: misma interfaz que v2.1, sin cambios para compatibilidad
  # ══════════════════════════════════════════════════════════════════════════
  
  message("✅ graficas_semanal_data_loaders v3.5 inicializado")
  message("   ✅ Carga bajo demanda: edad al entrar Semanal, sexo/origen al seleccionarlos")
  message("   ✅ Histórico disponible de inmediato — sin bloqueo de Firebase al arrancar")
  message("   ✅ Caché LNE_CACHE_SEMANAL evita recargas en sesiones sucesivas")
  
  return(list(
    datos_semanal_edad          = datos_semanal_edad,
    datos_semanal_sexo          = datos_semanal_sexo,
    datos_semanal_origen        = datos_semanal_origen,
    datos_semanal_serie_edad    = datos_semanal_serie_edad,
    datos_semanal_serie_sexo    = datos_semanal_serie_sexo,
    datos_semanal_serie_origen  = datos_semanal_serie_origen,
    fecha_semanal_efectiva      = fecha_semanal_efectiva,
    anio_semanal                = anio_semanal
  ))
}
