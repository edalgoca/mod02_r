library(tidyverse)

perc_redondeado<-function(x)
{
  paste(round(x,2),"%",sep = "")
}

Examinados<-read_csv2("c:/data/asistenciaexaminados.csv")

mtcars$mpg[4]

Examinados %>% 
  group_by(Facultad, Fecha) %>% 
  filter(Evaluacion=="QUINTA" && Modalidad=="PRESENCIAL" && Tipo=="ORDINARIA") %>% 
  mutate(porcentaje=perc_redondeado(mean(as.numeric(Examinados)/as.numeric(Inscritos))*100), GRUPOS_REPORTADOS=n()) %>% 
  distinct(Facultad, Fecha, porcentaje, GRUPOS_REPORTADOS) %>% 
  arrange(Fecha) 
  
Examinados %>% 
  group_by(Facultad, Fecha) %>% 
  filter(Evaluacion=="QUINTA" && Modalidad=="EN LINEA" && Tipo=="ORDINARIA") %>% 
  mutate(porcentaje=perc_redondeado(mean(as.numeric(Examinados)/as.numeric(Inscritos))*100), GRUPOS_REPORTADOS=n()) %>% 
  distinct(Facultad, Fecha, porcentaje, GRUPOS_REPORTADOS) %>% 
  arrange(Fecha) 

