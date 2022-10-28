#https://www.ecobici.cdmx.gob.mx/es/informacion-del-servicio/open-data

library(tidyverse)
library(lubridate)
#leer csv
#file.choose()
raw_data<-read.csv(paste(getwd(),"/R/archivos/2020-02.csv", sep = ""))
head(raw_data)

#procesamiento de datos
viajes_diarios<-raw_data %>% 
  mutate(fecha_hora = dmy_hms(paste(Fecha_Retiro, Hora_Retiro, sep=""))) %>% 
  filter(fecha_hora >= as.Date('2020-02-24'),
         fecha_hora <= as.Date('2020-02-27')) %>% 
  group_by(horas=floor_date(fecha_hora, unit = 'hour')) %>% 
  summarise(conteo=n())

View(viajes_diarios)

#hacer secuencia de horas para que no existan saltos
horas_completas<-data.frame(
  horas = seq(
    floor_date(min(viajes_diarios$horas), unit='hour'),
    floor_date(max(viajes_diarios$horas), unit='hour'),
    by='hour'
  )
)

#si se utiliza <- en horas el encabezado toma otro nombre por eso se usa =
View(horas_completas)
View(viajes_diarios)

#LEAFT JOIN CON HORAS
viajes_horas <- horas_completas %>% 
  group_by(horas_redondeadas=floor_date(horas, unit = 'hour')) %>% 
  left_join(viajes_diarios) %>% 
  mutate(conteo=ifelse(is.na(conteo),0,conteo))

View(viajes_horas)
#grafico inicial
ggplot(data = viajes_horas, aes(x=horas, y=conteo))+geom_line()


##inicia el modelo de datos predictivo
#modelo ARIMA estacional---lo que pasa ahora en base lo que paso ayer
#AR-regresion sobre si mismo, I-diferencias, MA-tiene que ver con errores, S-fenomenos que se repiten

#creando objeto ts para modelo
conteo_ts<-ts(viajes_horas$conteo,start = 1, frequency = 24)

library(forecast)
#ajuste del modelo---todo
ajuste<-auto.arima(y=conteo_ts)

summary(ajuste)

predicciones <-forecast(ajuste)
min(predicciones[['lower']])
max(predicciones[['upper']])

#graficando
p_predict<-autoplot(predicciones) #viene en forecast
p_predict

#graficando toda la semana################################## 
#copia y pega linea 10 hasta 41
#procesamiento de datos
viajes_diarios<-raw_data %>% 
  mutate(fecha_hora = dmy_hms(paste(Fecha_Retiro, Hora_Retiro, sep=""))) %>% 
  filter(fecha_hora >= as.Date('2020-02-24'),
         fecha_hora <= as.Date('2020-02-29')) %>% 
  group_by(horas=floor_date(fecha_hora, unit = 'hour')) %>% 
  summarise(conteo=n())

#View(viajes_diarios)

#hacer secuencia de horas para que no existan saltos
horas_completas<-data.frame(
  horas = seq(
    floor_date(min(viajes_diarios$horas), unit='hour'),
    floor_date(max(viajes_diarios$horas), unit='hour'),
    by='hour'
  )
)

#si se utiliza <- en horas el encabezado toma otro nombre por eso se usa =
#View(horas_completas)
#View(viajes_diarios)

#LEAFT JOIN CON HORAS
viajes_horas <- horas_completas %>% 
  group_by(horas_redondeadas=floor_date(horas, unit = 'hour')) %>% 
  left_join(viajes_diarios) %>% 
  mutate(conteo=ifelse(is.na(conteo),0,conteo))

#View(viajes_horas)
#grafico inicial
ggplot(data = viajes_horas, aes(x=horas, y=conteo))+geom_line() + 
  ylim(-551.5396, 4103.783)+
  labs(title = "Realidad")

########comparar resultados


