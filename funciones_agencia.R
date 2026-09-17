# Funciones compartidas para el análisis del presupuesto de la Agencia I+D+i.
# Extraídas de analiza_pres_agencia.R para reutilizar la misma metodología
# (lectura robusta de JSON, cotización oficial del dólar, agregación mensual
# en USD y anualización a partir del promedio mensual).
#
# Requiere: jsonlite, dplyr, tidyr, readxl, zoo.
# Los JSON anuales (agencia/YYYY.json) provienen de la API de Presupuesto
# Abierto (https://www.presupuestoabierto.gob.ar/api/v1/credito).
# Los archivos 2017-2026 ya vienen filtrados por programa 44 + programa_desc
# de la Agencia; los archivos 2009-2016 contienen todo el programa 44 y se
# filtran localmente con filtrar_agencia_ciencia().

# Lee un JSON tolerando BOM y distintas codificaciones (UTF-8 / latin1).
read_json_robust <- function(path) {
  # Try UTF-8 first
  txt <- tryCatch(readLines(path, encoding = "UTF-8", warn = FALSE), error = function(e) NULL)
  if (!is.null(txt) && length(txt) > 0) {
    s <- paste(txt, collapse = "\n")
    s <- sub('^\ufeff', '', s)
    res <- tryCatch(jsonlite::fromJSON(s), error = function(e) NULL)
    if (!is.null(res)) return(res)
  }
  # Fallback: read as Latin1 and convert to UTF-8
  txt <- readLines(path, encoding = "latin1", warn = FALSE)
  s <- paste(txt, collapse = "\n")
  s <- iconv(s, from = "latin1", to = "UTF-8")
  s <- sub('^\ufeff', '', s)
  jsonlite::fromJSON(s)
}

# Cotización oficial diaria (BCRA com3500, promedio compra+venta ya publicado).
# Completa días faltantes por interpolación lineal. Para fechas anteriores al
# inicio de la serie com3500 (2002-03-04) asume 1 ARS = 1 USD (convertibilidad).
# Solo cubre el período observado: no incluye proyecciones (REM/dólar futuro).
cargar_cotizacion_observada <- function(xls = "com3500.xls") {
  cot <- readxl::read_excel(xls, col_names = FALSE, skip = 4, .name_repair = "minimal")
  cot <- data.frame(
    fecha = as.Date(cot[[1]]),
    dolar = as.numeric(cot[[2]])
  )
  cot <- cot[!is.na(cot$fecha) & !is.na(cot$dolar), ]
  cot <- cot %>%
    dplyr::arrange(fecha) %>%
    tidyr::complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) %>%
    dplyr::mutate(dolar = zoo::na.approx(dolar, x = fecha, xout = fecha, na.rm = FALSE))
  pre <- data.frame(
    fecha = seq.Date(as.Date("2001-01-01"), min(cot$fecha) - 1, by = "day"),
    dolar = 1.0
  )
  dplyr::bind_rows(pre, cot)
}

# Lee y combina los JSON anuales de la Agencia. Agrega columna fecha
# (primer día del mes de impacto presupuestario).
cargar_agencia <- function(anios) {
  lst <- lapply(anios, function(y) read_json_robust(sprintf("agencia/%d.json", y)))
  data <- as.data.frame(dplyr::bind_rows(lst))
  data$fecha <- as.Date(paste(data$impacto_presupuestario_anio,
                              data$impacto_presupuestario_mes, "01", sep = "-"),
                        format = "%Y-%m-%d")
  data
}

# Conserva solo el programa de la Agencia ("...Actividades de Ciencia...").
# Necesario para 2009-2016 (descargas sin filtro de programa_desc);
# no altera 2017-2026 (ya filtrados en origen).
filtrar_agencia_ciencia <- function(data) {
  data %>% dplyr::filter(grepl("ciencia", programa_desc, ignore.case = TRUE))
}

# Serie mensual del crédito devengado en USD del inciso "Transferencias"
# (financiamiento de proyectos científicos). Convierte con el dólar oficial
# del primer día de cada mes, igual que analiza_pres_agencia.R.
# Devuelve una fila por mes con columnas fecha, impacto_presupuestario_anio,
# credito_devengado (millones de ARS), dolar y credito_devengado_usd
# (millones de USD).
mensual_transferencias_usd <- function(data, cotizacion) {
  transf <- data %>%
    filtrar_agencia_ciencia() %>%
    dplyr::filter(inciso_desc == "Transferencias")
  mensual <- transf %>%
    dplyr::group_by(fecha, impacto_presupuestario_anio) %>%
    dplyr::summarise(credito_devengado = sum(credito_devengado),
                     .groups = "drop")
  mensual <- merge(mensual, cotizacion, by = "fecha", all.x = TRUE)
  mensual$credito_devengado_usd <- mensual$credito_devengado / mensual$dolar
  mensual %>% dplyr::arrange(fecha)
}

# Anualiza la serie mensual: completa meses faltantes dentro del tramo
# observado de cada año con 0 (p. ej. meses sin ejecución) y calcula
# valor anual = promedio mensual x 12. Así los años parciales (como el año
# en curso, solo con meses observados) son comparables con años completos.
# Si se provee `span` (data.frame con columnas impacto_presupuestario_anio,
# desde y hasta como Date), completa cada año hasta ese tramo observado
# (p. ej. el tramo con actividad del programa en cualquier inciso);
# si es NULL usa el tramo de `mensual`.
# Devuelve: impacto_presupuestario_anio, n_meses, mensual_usd, anualizado_usd.
anualizar_mensual <- function(mensual, span = NULL) {
  mensual <- mensual %>% dplyr::arrange(fecha)
  por_anio <- split(mensual, mensual$impacto_presupuestario_anio)
  completa <- lapply(names(por_anio), function(a) {
    m <- por_anio[[a]]
    if (!is.null(span)) {
      s <- span[span$impacto_presupuestario_anio == as.integer(a), ]
      desde <- min(s$desde); hasta <- max(s$hasta)
    } else {
      desde <- min(m$fecha); hasta <- max(m$fecha)
    }
    rango <- data.frame(fecha = seq.Date(desde, hasta, by = "month"))
    rango$impacto_presupuestario_anio <- unique(m$impacto_presupuestario_anio)
    merged <- merge(rango, m[, c("fecha", "credito_devengado_usd")],
                    by = "fecha", all.x = TRUE)
    merged$credito_devengado_usd[is.na(merged$credito_devengado_usd)] <- 0
    merged
  })
  lleno <- dplyr::bind_rows(completa)
  lleno %>%
    dplyr::group_by(impacto_presupuestario_anio) %>%
    dplyr::summarise(n_meses = dplyr::n(),
                     mensual_usd = mean(credito_devengado_usd),
                     anualizado_usd = mensual_usd * 12,
                     .groups = "drop")
}
