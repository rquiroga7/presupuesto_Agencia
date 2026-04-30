library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(readxl)
library(zoo)
library(lubridate)

#cotizacion dolar is from
#https://www.bcra.gob.ar/pdfs/publicacionesestadisticas/com3500.xls


# Read json files into table (2017-2026) with a robust reader that handles BOM and common encodings
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

data2017 <- read_json_robust("agencia/2017.json")  %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2018 <- read_json_robust("agencia/2018.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2019 <- read_json_robust("agencia/2019.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2020 <- read_json_robust("agencia/2020.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2021 <- read_json_robust("agencia/2021.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2022 <- read_json_robust("agencia/2022.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2023 <- read_json_robust("agencia/2023.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2024 <- read_json_robust("agencia/2024.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2025 <- read_json_robust("agencia/2025.json")
data2026 <- read_json_robust("agencia/2026.json")


#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024, data2025, data2026))
data$fecha <- as.Date(paste(data$impacto_presupuestario_anio, data$impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")
data %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado=sum(credito_devengado))

#Lee cotizacion dolar
#cotizacion <- read.csv("cotizacion_dolar.csv", sep=",")
#cotizacion$fecha <- as.Date(cotizacion$fecha)
#calcula promedio entre compra y venta
#cotizacion$dolar <- (cotizacion$compra + cotizacion$venta) / 2
#interpolate dolar for all missing dates in cotizacion
#cotizacion <- cotizacion %>% complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) %>% fill(dolar)

#Lee cotizacion dolar
cotizacion <- read_excel("com3500.xls", col_names = FALSE, skip = 4, .name_repair = "minimal")
cotizacion <- data.frame(
  fecha = as.Date(cotizacion[[1]]),
  dolar = as.numeric(cotizacion[[2]])
)

#merge with agencia/dolar_futuro.csv for future projections
dolar_futuro <- read.csv("agencia/dolar_futuro.csv", sep=",")
dolar_futuro$fecha <- as.Date(dolar_futuro$fecha)

#Combine cotizacion with dolar_futuro, keeping cotizacion values where available
cotizacion <- rbind(cotizacion, dolar_futuro %>% filter(fecha > max(cotizacion$fecha)))

#interpolate dolar for all missing dates in cotizacion
cotizacion <- cotizacion %>% 
  complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) %>%
  mutate(dolar = na.approx(dolar, x = fecha, xout = fecha, na.rm = FALSE))

last_day <- max(cotizacion$fecha)
last_month <- as.Date(paste0(year(last_day), "-", sprintf("%02d", month(last_day)), "-01"))
#Tengo que proyectar la cotizacion del dolar
#reemplazar por valores dolar futuro 
cot_proy <- data.frame(fecha = seq.Date(last_day, as.Date("2026-12-31"), by = "day"))

#merge with agencia/dolar_futuro.csv
dolar_futuro <- read.csv("agencia/dolar_futuro.csv", sep=",")
dolar_futuro$fecha <- as.Date(dolar_futuro$fecha)

# Add last historical data point to dolar_futuro to ensure continuity
last_dolar <- cotizacion$dolar[cotizacion$fecha == last_day]
bridge_point <- data.frame(fecha = last_day, dolar = last_dolar)
dolar_futuro <- rbind(bridge_point, dolar_futuro) %>% arrange(fecha)

cot_proy <- merge(cot_proy, dolar_futuro, by = "fecha", all.x = TRUE) %>% 
  arrange(fecha) %>%
  mutate(dolar = na.approx(dolar, x = fecha, xout = fecha, na.rm = FALSE))
cotizacion<-rbind(cotizacion %>% select(fecha,dolar), cot_proy) 

#Join data and cotizacion
data <- merge(data, cotizacion, by.x="fecha", by.y="fecha", all.x=TRUE)
data$credito_devengado_usd <- data$credito_devengado / data$dolar

#For each year in data, analyze what percentage of the total credito_devengado_usd is spent in each month
perc_mes<-data %>% group_by(impacto_presupuestario_anio) %>% mutate(total=sum(credito_devengado_usd))  %>% ungroup() %>% group_by(impacto_presupuestario_mes,impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd),total=mean(total)) %>% mutate(perc=credito_devengado_usd/total) %>% group_by(impacto_presupuestario_mes) %>% summarise(perc=mean(perc))
#normalizo
perc_mes$perc<-perc_mes$perc/sum(perc_mes$perc)
#Tengo el gasto promedio por mes como % del total en perc_mes
#Ahora proyecto el gasto para lo que queda de 2024

#t2026 es el presupuesto vigente 2026
View(data %>% filter(impacto_presupuestario_anio==2026 & impacto_presupuestario_mes==3) )
t2026<-data %>% filter(impacto_presupuestario_anio==2026) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd)) %>% pull(credito_devengado_usd)
meses<-seq.Date(last_month, as.Date("2026-12-31"), by = "month")
cdusd<-perc_mes %>% filter(impacto_presupuestario_mes>=month(last_month)) %>% pull(perc) *t2026
anio<-rep(2026,length(meses))
proy2026_mensual<-data.frame(impacto_presupuestario_anio=anio,fecha=meses,credito_devengado_usd=cdusd)

anual_data<- data%>% filter(fecha < last_month) %>% group_by(fecha, impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
anual_data_proy<-rbind(anual_data,proy2026_mensual)

anual_data_proy %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))


mensual <- data %>% filter(fecha < last_month) %>% group_by(fecha,impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
todo<- rbind(mensual, proy2026_mensual %>% select(impacto_presupuestario_anio,fecha,credito_devengado_usd))
anual <- todo %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
View(anual)


todo<-merge(todo, cotizacion,by ="fecha") %>% mutate(credito_devengado=credito_devengado_usd*dolar)
# Assign color by date ranges for monthly plots
todo <- todo %>% mutate(color_period = case_when(
  fecha <= as.Date("2019-11-30") ~ "#d4d400",
  fecha <= as.Date("2023-11-30") ~ "#31ffff",
  fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "#a8009d",
  TRUE ~ "#a8009d"
))
#check
#todo %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado=sum(credito_devengado))

#Barplot
colors9=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#a8009d", "#a8009d", "#a8009d")  
ggplot(anual, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_usd, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
    labs(title = "Agencia I+D+i: Presupuesto anual devengado",subtitle="En millones de dólares a cotización oficial al momento de devengar.\nSe proyecta la ejecución para 2026.",
       x = "Año",
       y = "Credito anual devengado\n(en millones de dólares)") +
    scale_fill_manual(values=colors9) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_usd, label = round(credito_devengado_usd, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(anual$credito_devengado_usd) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Agencia I+D+i financia proyectos de Investigación, Tecnología e Innovación, principalmente con financiamiento BID.\nSe ajustó el crédito devengado en cada mes por la cotización oficial del dólar del BCRA.\nSe proyecta la ejecución presupuestaria para el resto de 2026. Se toma cotización futura del REM.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia "))
  ggsave("plots/presupuesto_agencia_usd_anual_2017-2026.png",width = 10, height = 6, units = "in",dpi=300)

# Calculate quarterly averages
mensual <- mensual %>% mutate(quarter = lubridate::quarter(fecha, with_year = TRUE))
quarterly_avg <- mensual %>% 
  group_by(quarter, impacto_presupuestario_anio) %>% 
  summarise(avg_credito_devengado_usd = mean(credito_devengado_usd)) %>%
  ungroup() %>%
  mutate(
    date = lubridate::yq(quarter),
    color_period = case_when(
      date <= as.Date("2019-11-30") ~ "#d4d400",
      date <= as.Date("2023-11-30") ~ "#31ffff",
      date >= as.Date("2023-12-01") & date <= as.Date("2027-11-30") ~ "#a8009d",
      TRUE ~ "#a8009d"
    )
  )


# Plot credito_devengado_usd quarterly average 2023-2026
# Use actual quarter start dates on the x-axis and format as YYYY-mm
ggplot(quarterly_avg, aes(x = date, y = avg_credito_devengado_usd, color = color_period, group = 1)) +
  geom_line(size = 1.2) +
  labs(title = "Agencia I+D+i: Presupuesto trimestral promedio devengado", subtitle = "En millones de dólares a cotización oficial al momento de devengar.\nSe proyecta la ejecución para 2024, y se toma el presupuesto para 2026.",
    x = "Trimestre",
    y = "Credito promedio trimestral devengado\n(en millones de dólares)") +
  scale_color_identity() +
  theme_light(base_size = 14) +
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(quarterly_avg$avg_credito_devengado_usd) * 1.1)) +
  scale_x_date(
    breaks = function(x) seq.Date(from = floor_date(min(x), "month"), to = ceiling_date(max(x), "month"), by = "3 months"),
    date_labels = "%Y-%m",
    expand = expansion(mult = c(0.02, 0.02))
  ) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5), axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  labs(caption = paste0("Agencia I+D+i financia proyectos de Investigación, Tecnología e Innovación, principalmente con financiamiento BID.\nSe ajustó el crédito devengado en cada mes por la cotización oficial del dólar del BNA, tomando promedio venta+compra.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia "))
ggsave("plots/presupuesto_agencia_usd_2017-2026.png", width = 10, height = 6, units = "in", dpi = 300)



#Filter data for <= last_month
todo <- todo %>% filter(fecha <= last_month) %>% select(impacto_presupuestario_anio, fecha, credito_devengado_usd)
# Calculate quarterly averages for all data including projections
trimester_monthly <- todo %>% 
  # First calculate the quarter for each date
  mutate(quarter = lubridate::quarter(fecha, with_year = TRUE)) %>%
  # Calculate quarterly averages
  group_by(quarter, impacto_presupuestario_anio) %>%
  mutate(avg_credito_devengado_usd = mean(credito_devengado_usd)) %>%
  # Keep all original months with their corresponding quarterly average
  select(fecha, impacto_presupuestario_anio, avg_credito_devengado_usd) %>%
  # Add a grouping variable that changes at 2019/2020 and 2023/2024 transitions
  mutate(line_group = case_when(
    fecha <= as.Date("2019-12-31") ~ 1,
    fecha >= as.Date("2020-01-01") & fecha <= as.Date("2023-12-31") ~ 2,
    fecha >= as.Date("2024-01-01") ~ 3
  ))

# Ensure quarterly average lines have color_period for plotting (mapped from date)
trimester_monthly <- trimester_monthly %>% mutate(color_period = case_when(
  fecha <= as.Date("2019-11-30") ~ "#d4d400",
  fecha <= as.Date("2023-11-30") ~ "#31ffff",
  fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "#a8009d",
  TRUE ~ "#a8009d"
))


# Plot with both monthly dots and quarterly averages
ggplot() +
  # Add monthly data points
  geom_point(data = todo, 
             aes(x = fecha, y = credito_devengado_usd), 
             color = "black", size = 1, alpha = 0.5) +
  # Add connected quarterly average lines
geom_line(data = trimester_monthly,
          aes(x = fecha,
              y = avg_credito_devengado_usd,
              color = color_period,
              group = line_group),  # Changed from group = 1
          size = 1.2) +
  labs(title = "Agencia I+D+i: Presupuesto mensual devengado",
       subtitle = "En millones de dólares a cotización oficial al momento de devengar.\nPuntos: valores mensuales. Líneas: promedios trimestrales",
       x = "Fecha",
       y = "Credito mensual devengado\n(en millones de dólares)") +
  scale_color_identity() +
  theme_light(base_size = 14) +
  scale_y_continuous(labels = scales::comma, 
                    limits = c(NA, max(todo$credito_devengado_usd) * 1.1)) +
  scale_x_date(breaks = function(x) seq.Date(from = floor_date(min(x), "year"),
                                           to = ceiling_date(max(x), "year"),
                                           by = "6 months"),
               date_labels = "%Y-%m",
               expand = expansion(mult = c(0.02, 0.02))) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  labs(caption = paste0("Agencia I+D+i financia proyectos de Investigación, Tecnología e Innovación, principalmente con financiamiento BID.\n",
                       "Se ajustó el crédito devengado en cada mes por la cotización oficial del dólar del BCRA.\n",
                       "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia"))

ggsave("plots/presupuesto_agencia_usd_mensual_2017-2026.png", 
       width = 10, height = 6, units = "in", dpi = 300)


#Check incisos
data %>% filter(credito_devengado > 0 & inciso_desc == "Gastos en personal") %>% 
  group_by(impacto_presupuestario_anio, inciso_desc) %>% 
  summarise(credito_devengado_usd = sum(credito_devengado_usd)) %>% 
  View()

  transf<- data %>% filter(credito_devengado > 0 & inciso_desc == "Transferencias") %>% 
  group_by(impacto_presupuestario_anio, inciso_desc) %>% 
  summarise(credito_devengado_usd = sum(credito_devengado_usd)) %>% 
  View()
    data %>% filter(credito_devengado > 0) %>% 
  group_by(impacto_presupuestario_anio) %>% 
  summarise(credito_devengado_usd = sum(credito_devengado_usd)) %>% 
  View()



transf <- data %>% filter(inciso_desc == "Transferencias")

# Get the full date range from the original data
date_range <- seq.Date(min(data$fecha), max(data$fecha), by = "month")

mensual_t <- transf %>% 
  filter(fecha < last_month) %>% 
  group_by(fecha, impacto_presupuestario_anio) %>% 
  summarise(credito_devengado_usd = sum(credito_devengado_usd)) %>%
  ungroup()

todo_t <- rbind(mensual_t, proy2026_mensual %>% 
                  select(impacto_presupuestario_anio, fecha, credito_devengado_usd))

# Fill missing months with 0 using the complete date range
todo_t <- todo_t %>% 
  complete(fecha = date_range, 
           fill = list(credito_devengado_usd = 0)) %>%
  mutate(impacto_presupuestario_anio = year(fecha)) %>%
  arrange(fecha)

todo_t <- merge(todo_t, cotizacion, by = "fecha") %>% 
  mutate(credito_devengado = credito_devengado_usd * dolar)

# Assign color by date ranges for transfer monthly plots
todo_t <- todo_t %>% mutate(color_period = case_when(
  fecha <= as.Date("2019-11-30") ~ "#d4d400",
  fecha <= as.Date("2023-11-30") ~ "#31ffff",
  fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "#a8009d",
  TRUE ~ "#a8009d"
))

#Filter data for <= last_month
todo_t <- todo_t %>% filter(fecha <= last_month) %>% select(impacto_presupuestario_anio, fecha, credito_devengado_usd)
# Calculate quarterly averages for all data including projections
trimester_monthly_t <- todo_t %>% 
  # First calculate the quarter for each date
  mutate(quarter = lubridate::quarter(fecha, with_year = TRUE)) %>%
  # Calculate quarterly averages
  group_by(quarter, impacto_presupuestario_anio) %>%
  mutate(avg_credito_devengado_usd = mean(credito_devengado_usd)) %>%
  # Keep all original months with their corresponding quarterly average
  select(fecha, impacto_presupuestario_anio, avg_credito_devengado_usd) %>%
  # Add a grouping variable that changes at 2019/2020 and 2023/2024 transitions
  mutate(line_group = case_when(
    fecha <= as.Date("2019-12-31") ~ 1,
    fecha >= as.Date("2020-01-01") & fecha <= as.Date("2023-12-31") ~ 2,
    fecha >= as.Date("2024-01-01") ~ 3
  ))

# Ensure transfer quarterly average lines have color_period for plotting
trimester_monthly_t <- trimester_monthly_t %>% mutate(color_period = case_when(
  fecha <= as.Date("2019-11-30") ~ "#d4d400",
  fecha <= as.Date("2023-11-30") ~ "#31ffff",
  fecha >= as.Date("2023-12-01") & fecha <= as.Date("2027-11-30") ~ "#a8009d",
  TRUE ~ "#a8009d"
))


# Plot with both monthly dots and quarterly averages
ggplot() +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  # Add monthly data points
    geom_point(data = todo_t, 
         aes(x = fecha, y = credito_devengado_usd), 
         color = "black", size = 1, alpha = 0.5) +
  # Add connected quarterly average lines
    geom_line(data = trimester_monthly_t,
        aes(x = fecha,
          y = avg_credito_devengado_usd,
          color = color_period,
          group = line_group),  # Changed from group = 1
          size = 1.2) +
  labs(title = "Agencia I+D+i: Crédito mensual devengado para financiar proyectos",
       subtitle = "En millones de dólares a cotización oficial al momento de devengar.\nPuntos: valores mensuales. Líneas: promedios trimestrales",
       x = "Fecha",
       y = "Credito mensual devengado\n(en millones de dólares)") +
  scale_color_identity() +
  theme_light(base_size = 14) +
  scale_y_continuous(labels = scales::comma, 
                    limits = c(NA, max(todo$credito_devengado_usd) * 1.1)) +
  scale_x_date(breaks = function(x) seq.Date(from = floor_date(min(x), "year"),
                                           to = ceiling_date(max(x), "year"),
                                           by = "6 months"),
               date_labels = "%Y-%m",
               expand = expansion(mult = c(0.02, 0.02))) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.caption = element_text(size = 8)) +  # Add this line to reduce caption size)+
  labs(caption = paste0("Agencia I+D+i financia proyectos de Investigación, Tecnología e Innovación, principalmente con financiamiento BID.\n",
                       "Se ajustó el crédito devengado en cada mes por la cotización oficial del dólar del BCRA.\n",
                       "Se tomaron todas las ejecuciones de crédito de la API de Presupuesto Abierto, para el programa 44, inciso \"Transferencias\".\n",
                       "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia"))

ggsave("plots/presupuesto_agencia_proyectos_usd_mensual_2017-2026.png", 
       width = 10, height = 6, units = "in", dpi = 300)



#Sin puntos


# Plot with both monthly dots and quarterly averages
ggplot() +
  geom_hline(yintercept = 0, color = "black", size = 0.5) +
  # Add connected quarterly average lines
  geom_line(data = trimester_monthly_t,
          aes(x = fecha,
              y = avg_credito_devengado_usd,
              color = as.factor(impacto_presupuestario_anio),
              group = line_group),  # Changed from group = 1
          size = 1.2) +
  labs(title = "Agencia I+D+i: Crédito mensual devengado para financiar proyectos",
       subtitle = "En millones de dólares a cotización oficial al momento de devengar.\nPromedios trimestrales",
       x = "Fecha",
       y = "Credito mensual devengado\n(en millones de dólares)") +
  scale_color_manual(values = colors9) +
  theme_light(base_size = 14) +
  scale_y_continuous(labels = scales::comma, 
                    limits = c(0, 11), expand=c(0,0), minor_breaks = NULL, breaks = seq(0,11,1)) +
  scale_x_date(breaks = function(x) seq.Date(from = floor_date(min(x), "year"),
                                           to = ceiling_date(max(x), "year"),
                                           by = "6 months"),
               date_labels = "%Y-%m",
               expand = expansion(mult = c(0.02, 0.02))) +
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        plot.caption = element_text(size = 8)) +  # Add this line to reduce caption size)+
  labs(caption = paste0("Agencia I+D+i financia proyectos de Investigación, Tecnología e Innovación, principalmente con financiamiento BID.\n",
                       "Se ajustó el crédito devengado en cada mes por la cotización oficial del dólar del BCRA.\n",
                       "Se tomaron todas las ejecuciones de crédito de la API de Presupuesto Abierto, para el programa 44, inciso \"Transferencias\".\n",
                       "Por Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia"))

ggsave("plots/presupuesto_agencia_proyectos_usd_mensual_nodots_2017-2026.png", 
       width = 10, height = 6, units = "in", dpi = 300)