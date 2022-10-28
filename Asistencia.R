library(tidyverse)
library(jsonlite)
library(lubridate)

DataA<-fromJSON("https://tecnologica.utec.edu.sv/sistema/services/vistaasistencias.php")

perc_redondeado<-function(x)
{
  paste(round(x,2),"%",sep = "")
}

DataA %>% 
  mutate(Fecha=ymd(fecha)) %>% 
  filter(Fecha >= as.Date('2022-06-13') && Fecha <= as.Date('2022-06-25')) %>% 
  group_by(Fecha) %>% 
  mutate(Inscritos=sum(as.numeric(Inscritos)), Asistencia=sum(as.numeric(Asistencia))) %>% 
  mutate(Porcentaje=perc_redondeado((Asistencia/Inscritos)*100)) %>% 
  distinct(Fecha, Porcentaje) %>% 
  arrange(Fecha) %>%   
  ggplot(aes(x=Fecha, y=Porcentaje, group=Fecha)) +
  geom_point()+
  geom_line()+
  xlab("Fecha de asistencia")+
  ylab("Porcentaje de asistencia")+
  ggtitle(paste("Reporte de asistencia del periodo: ", 
                periodo="2022-06-13 al 2022-06-25"))+
  geom_text(aes(x=Fecha, y= Porcentaje,label=Porcentaje),
            vjust=0,angle=0,size=3.5)

x<-DataA %>% 
  filter(fecha >= as.Date('2022-06-13') && fecha <= as.Date('2022-06-25')) %>% 
  group_by(facultad, fecha) %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(Asistencia)/as.numeric(Inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(Inscritos)), Asistencia=sum(as.numeric(Asistencia))) %>% 
  distinct(fecha, facultad, Porcentaje, Grupos_reportados, Inscritos, Asistencia) %>% 
  arrange(fecha)

DataA %>% 
  mutate(Fecha=ymd(fecha)) %>% 
  filter(Fecha >= as.Date('2022-06-13') && Fecha <= as.Date('2022-06-25')) %>% 
  group_by(Fecha, Modalidad) %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(Asistencia)/as.numeric(Inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(Inscritos)), Asistencia=sum(as.numeric(Asistencia))) %>% 
  distinct(Fecha, Grupos_reportados, Porcentaje, Inscritos, Asistencia) %>% 
  arrange(Fecha) %>%   
  ggplot(mapping = aes(x=Fecha, y=Porcentaje,fill=Modalidad, shape=Modalidad))+
  geom_bar(position = "dodge", stat = "identity")+
  xlab("Fecha de asistencia")+
  ylab("Porcentaje de asistencia")+
  ggtitle(paste("Reporte de asistencia por modalidad"))+
  geom_text(aes(x=Fecha, y= Porcentaje, 
                label=Porcentaje),
            vjust=0,angle=0,size=3.5)



DataA %>%
  filter(Fecha >= ymd('2022-06-21') && Fecha <= ymd('2022-06-29'))

%>% 
  mutate(Porcentaje=perc_redondeado(sum(as.numeric(Asistencia))/sum(as.numeric(Inscritos))*100)) %>%
  distinct(Porcentaje)

tibble(DataA)
class(DataA[11])
x<-as.tibble(DataA)
x %>%
  filter(Fecha >= as.Date('2022-06-21') && Fecha <= as.Date('2022-06-29'))


x<-interval(ymd('2022-06-21'), ymd('2022-06-29'))
DataA %>% 
  filter(Fecha %within% interval(ymd('2022-06-21'), ymd('2022-06-29')))
