library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)

#Read json files into table (2017-2024)
data2017 <- fromJSON("agencia/2017.json")  %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2018 <- fromJSON("agencia/2018.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2019 <- fromJSON("agencia/2019.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2020 <- fromJSON("agencia/2020.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2021 <- fromJSON("agencia/2021.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2022 <- fromJSON("agencia/2022.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2023 <- fromJSON("agencia/2023.json") %>% mutate(impacto_presupuestario_fecha=as.Date(paste0(impacto_presupuestario_anio,"-",impacto_presupuestario_mes,"-01")))
data2024 <- fromJSON("agencia/2024.json") 


#Join into data
data <- as.data.frame(rbind(data2017, data2018, data2019, data2020, data2021, data2022, data2023, data2024))
data$fecha <- as.Date(paste(data$impacto_presupuestario_anio, data$impacto_presupuestario_mes, "01", sep = "-"), format = "%Y-%m-%d")
data %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado=sum(credito_devengado))

#Lee cotizacion dolar
cotizacion <- read.csv("cotizacion_dolar.csv", sep=",")
cotizacion$fecha <- as.Date(cotizacion$fecha)
#calcula promedio entre compra y venta
cotizacion$dolar <- (cotizacion$compra + cotizacion$venta) / 2
#interpolate dolar for all missing dates in cotizacion
cotizacion <- cotizacion %>% complete(fecha = seq.Date(min(fecha), max(fecha), by = "day")) %>% fill(dolar)




#Tengo que proyectar la cotizacion del dolar
#Utilizo los valores de presupuesto 2025
#que dicen 1019.9 a fines de 2024 y 1207 a fines de 2025
cot_proy <- data.frame(fecha = seq.Date(as.Date("2024-09-19"), as.Date("2025-12-31"), by = "day"))
# Define the start and end dates for the two growth periods
start_date_1 <- as.Date("2024-09-19"); end_date_1 <- as.Date("2024-12-31"); start_date_2 <- as.Date("2025-01-01"); end_date_2 <- as.Date("2025-12-31")
# Define the initial and final values for each period
initial_value_1 <- 966; final_value_1 <- 1019.9 ; final_value_2 <- 1207
# Calculate the number of days in each period
num_days_1 <- as.numeric(end_date_1 - start_date_1) ; num_days_2 <- as.numeric(end_date_2 - start_date_2)
# Calculate the daily growth rate for each period
daily_growth_rate_1 <- (final_value_1 - initial_value_1) / num_days_1 ; daily_growth_rate_2 <- (final_value_2 - final_value_1) / num_days_2
# Calculate the dollar value for each day
cot_proy$dolar <- ifelse(
  cot_proy$fecha <= end_date_1,
  initial_value_1 + daily_growth_rate_1 * as.numeric(cot_proy$fecha - start_date_1),
  final_value_1 + daily_growth_rate_2 * as.numeric(cot_proy$fecha - start_date_2)
)
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

#t2024 es el presupuesto vigente 2024
View(data %>% filter(impacto_presupuestario_anio==2024 & impacto_presupuestario_mes==8) )
t2024<-data %>% filter(impacto_presupuestario_anio==2024) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd)) %>% pull(credito_devengado_usd)
meses<-seq.Date(as.Date("2024-09-01"), as.Date("2024-12-31"), by = "month")
cdusd<-perc_mes %>% filter(impacto_presupuestario_mes>=9) %>% pull(perc) *t2024
anio<-rep(2024,length(meses))
proy2024_mensual<-data.frame(impacto_presupuestario_anio=anio,fecha=meses,credito_devengado_usd=cdusd)

anual_data<- data%>% filter(fecha < as.Date("2024-09-01")) %>% group_by(fecha, impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
anual_data_proy<-rbind(anual_data,proy2024)

anual_data_proy %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
#Ahora proyecto el gasto para 2025
#Presupuesto agencia 2025: 36087 millones
fecha_2025<-seq.Date(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "month")
cd_2025<-perc_mes %>% pull(perc) * 36087
anio<-rep(2025,length(fecha_2025))
proy2025<-data.frame(impacto_presupuestario_anio=anio,fecha=fecha_2025,credito_devengado=cd_2025)
proy2025_mensual<-merge(proy2025,cotizacion, by.x="fecha", by.y="fecha", all.x=TRUE) %>% mutate(credito_devengado_usd=credito_devengado/dolar) 



mensual <- data %>% filter(fecha < as.Date("2024-09-01")) %>% group_by(fecha,impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
todo<- rbind(mensual, proy2024_mensual, proy2025_mensual %>% select(impacto_presupuestario_anio,fecha,credito_devengado_usd))
anual <- todo %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado_usd=sum(credito_devengado_usd))
View(anual)


todo<-merge(todo, cotizacion,by ="fecha") %>% mutate(credito_devengado=credito_devengado_usd*dolar)
#check
#todo %>% group_by(impacto_presupuestario_anio) %>% summarise(credito_devengado=sum(credito_devengado))

#Barplot
colors9=c("#d4d400","#d4d400","#d4d400", "#31ffff", "#31ffff", "#31ffff", "#31ffff", "#a8009d", "#a8009d")  
ggplot(anual, aes(x=as.factor(impacto_presupuestario_anio), y=credito_devengado_usd, fill=as.factor(impacto_presupuestario_anio))) +
  geom_bar(stat="identity") +
    labs(title = "Agencia I+D+i: Presupuesto anual devengado",subtitle="En millones de dólares a cotización oficial al momento de devengar.\nSe considera ejecución del 100% del presupuesto para 2024 y 2025",
       x = "Año",
       y = "Credito anual devengado\n(en millones de dólares)") +
    scale_fill_manual(values=colors9) +
  theme_light(base_size=14) +
    geom_text(aes(y = credito_devengado_usd, label = round(credito_devengado_usd, 0)), vjust = -0.5,size=5) +
  #scale y axis to show values in millions
  scale_y_continuous(labels = scales::comma, limits = c(NA, max(anual$credito_devengado_usd) * 1.1)) +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5), plot.subtitle = element_text(hjust = 0.5))+
  labs(caption = paste0("Agencia I+D+i financia proyectos de Investigación, Tecnología e Innovación, principalmente con financiamiento BID.\nSe ajustó el crédito devengado en cada mes por la cotización oficial del dólar del BNA, tomando promedio venta+compra.\nSe asume ejecución presupuestaria del 100% para 2024 y 2025. Se toma cotización futura del dólar del presupuesto 2025.\nEs decir, un incremento lineal de la cotización del dólar hasta $1019,9 a fines de 2024 y $1207 a fines de 2025.\nPor Rodrigo Quiroga. Ver https://github.com/rquiroga7/presupuesto_Agencia "))
  ggsave("plots/presupuesto_agencia_usd_2017-2025.png",width = 10, height = 6, units = "in",dpi=300)

