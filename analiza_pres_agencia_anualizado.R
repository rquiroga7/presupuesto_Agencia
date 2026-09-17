library(dplyr)
library(ggplot2)

# Reutiliza las funciones de analiza_pres_agencia.R
# (lector robusto de JSON, cotización observada, agregación mensual en USD,
# anualización a partir del promedio mensual).
source("funciones_agencia.R")

# Serie mensual 2009-2026 del programa de la Agencia.
# 2009-2016: programa 44 sin filtro de programa_desc (se filtra localmente
#   por "ciencia"); 2017-2026: ya filtrados en origen. El filtro local es
#   no-op para 2017-2026.
# 2001-2008: la Agencia no es identificable en la API de Presupuesto Abierto
#   (2001-2004 sin registros; 2005-2008 solo fragmentos Tesoro Nacional del
#   inciso Activos Financieros, sin Transferencias), por lo que la serie
#   comparable arranca en 2009 con la creación del programa 44
#   "Promoción y Financiamiento de Actividades de Ciencia...".
data <- cargar_agencia(2009:2026)

# Cotización oficial observada (BCRA com3500). Sin proyecciones: solo meses
# observados (a septiembre de 2026).
cotizacion <- cargar_cotizacion_observada()

# Crédito mensual devengado en USD del inciso "Transferencias"
# (financiamiento de proyectos científicos), con el dólar oficial del
# primer día de cada mes, igual que en analiza_pres_agencia.R.
mensual <- mensual_transferencias_usd(data, cotizacion)
mensual <- mensual %>% filter(!is.na(credito_devengado_usd))

# Crédito anual = promedio mensual observado x 12 (años parciales comparables).
# El tramo observado de cada año se toma del programa completo (cualquier
# inciso): meses del tramo sin filas de Transferencias se cuentan como 0
# (p. ej. sep-2026 tiene ejecución del programa pero sin Transferencias).
span <- data %>%
  filtrar_agencia_ciencia() %>%
  group_by(impacto_presupuestario_anio) %>%
  summarise(desde = min(fecha), hasta = max(fecha), .groups = "drop")
anual <- anualizar_mensual(mensual, span = span)
print(as.data.frame(anual))

# Colores por período: 2009-2015 celeste claro; resto consistente con
# analiza_pres_agencia.R (2016-2019 amarillo, 2020-2023 cian, 2024+ magenta).
mes_es <- c("ene", "feb", "mar", "abr", "may", "jun",
            "jul", "ago", "sep", "oct", "nov", "dic")
anual <- anual %>% mutate(
  color_periodo = case_when(
    impacto_presupuestario_anio <= 2015 ~ "#ADD8E6",
    impacto_presupuestario_anio <= 2019 ~ "#d4d400",
    impacto_presupuestario_anio <= 2023 ~ "#31ffff",
    TRUE ~ "#a8009d"
  )
)
ult <- anual %>% filter(impacto_presupuestario_anio == max(impacto_presupuestario_anio))
span_ult <- span %>% filter(impacto_presupuestario_anio == max(impacto_presupuestario_anio))
etiqueta_ult <- sprintf("%d parcial (%s-%s, %d meses)",
                        ult$impacto_presupuestario_anio,
                        mes_es[as.integer(format(span_ult$desde, "%m"))],
                        mes_es[as.integer(format(span_ult$hasta, "%m"))],
                        ult$n_meses)

ggplot(anual, aes(x = as.factor(impacto_presupuestario_anio),
                  y = anualizado_usd,
                  fill = color_periodo)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(anualizado_usd, 0)),
            vjust = -0.5, size = 4, show.legend = FALSE) +
  labs(title = "Agencia I+D+i: Crédito anual devengado para financiamiento de proyectos científicos",
       subtitle = sprintf("Anualizado a partir del promedio mensual observado (promedio x 12).\nEn millones de dólares a cotización oficial del primer día de cada mes. %s.",
                          etiqueta_ult),
       x = "Año",
       y = "Crédito anual devengado\n(en millones de dólares)") +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::comma,
                     limits = c(NA, max(anual$anualizado_usd) * 1.15)) +
  theme_light(base_size = 14) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(caption = paste0("Inciso \"Transferencias\" del programa de Promoción y Financiamiento de Actividades de Ciencia, Tecnología e Innovación (programa 44).\n",
                        "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia"))
ggsave("plots/presupuesto_agencia_proyectos_usd_anualizado_2009-2026.png",
       width = 10, height = 6, units = "in", dpi = 300)
