library(tidyverse)
library(jsonlite)

Data<-fromJSON("https://tecnologica.utec.edu.sv/sistema/services/vistaexaminados.php")

#write_excel_csv(Data,"20220629.csv")

perc_redondeado<-function(x)
{
  paste(round(x,2),"%",sep = "")
}
# Data %>% 
#   group_by(facultad, fecha) %>% 
#   filter(evaluacion=="QUINTA" && modalidad=="EN LINEA" && tipo=="ORDINARIA") %>% 
#   mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
#   distinct(facultad, fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
#   arrange(fecha) %>%   
#   ggplot()+
#   geom_point(mapping = aes(x=fecha, y=Porcentaje,color=facultad, shape=facultad))+
#   xlab("Fecha de examen parcial")+
#   ylab("Porcentaje de examinados")+
#   ggtitle(paste("Reporte de examinados por facultad" , "UTEC" ))
# 
# Data %>% 
#   group_by(facultad, fecha) %>% 
#   filter(evaluacion=="QUINTA" && modalidad=="EN LINEA" && tipo=="Ordinario") %>% 
#   mutate(porcentaje=perc_redondeado(mean(as.numeric(asistencia)/as.numeric(inscritos_materia))*100)) %>% 
#   distinct(facultad, fecha, porcentaje) %>% 
#   arrange(fecha) %>% 
#   ggplot()+
#   geom_point(mapping = aes(x=fecha, y=porcentaje,color=facultad, shape=facultad, size=1))+
#   xlab("Fecha de examen parcial")+
#   ylab("Porcentaje de examinados")+
#   ggtitle(paste("Reporte de examinados por facultad" , "UTEC" ))

Data %>% 
  group_by(facultad, fecha) %>% 
  filter(evaluacion=="QUINTA", modalidad=="EN LINEA" && tipo=="ORDINARIA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(facultad, fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(mapping = aes(x=fecha, y=Porcentaje,fill=facultad, shape=facultad))+
  geom_bar(position = "dodge", stat = "identity")+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por facultad" , "UTEC" ))

X<-Data %>% 
  group_by(facultad, fecha) %>% 
  filter(evaluacion=="QUINTA", modalidad=="EN LINEA" && tipo=="ORDINARIA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(facultad, fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha)
###############################################################################
Data %>% 
  group_by(facultad, area, fecha) %>% 
  filter(evaluacion=="QUINTA", modalidad=="EN LINEA" && tipo=="ORDINARIA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(facultad, area, fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(mapping = aes(x=fecha, y=Porcentaje,fill=area, shape=area))+
  geom_bar(position = "dodge", stat = "identity")+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por Area" , "UTEC" ))

X<-Data %>% 
  group_by(facultad, area, fecha) %>% 
  filter(evaluacion=="QUINTA", modalidad=="EN LINEA" && tipo=="ORDINARIA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(facultad, area, fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) 

##########################################################################

X<-Data %>% 
  filter(evaluacion=="QUINTA", modalidad=="EN LINEA" && tipo=="ORDINARIA") %>% 
  group_by(modalidad, fecha) %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(modalidad, fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) 

##########################################################################
Data %>% 
  group_by(fecha, modalidad) %>% 
  filter(evaluacion=="QUINTA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(mapping = aes(x=fecha, y=Porcentaje,fill=modalidad, shape=modalidad))+
  geom_bar(position = "dodge", stat = "identity")+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evalauación #" ,"QUINTA"))

Data %>% 
  group_by(fecha,area) %>% 
  filter(evaluacion=="QUINTA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(area, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(aes(x=as.factor(area), fill=as.factor(area) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")+
  xlab("Escuela")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evalauación #" ,"QUINTA"))


library(RColorBrewer)
coul <- brewer.pal(5, "Set2") 
Data %>% 
  group_by(fecha,area) %>% 
  filter(evaluacion=="QUINTA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(area, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(aes(x=as.factor(area), fill=as.factor(area) )) + 
  geom_bar( ) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position="none")+
  xlab("Escuela")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evalauación #" ,"QUINTA"))

##########################################
Data %>% 
  group_by(fecha,area) %>% 
  filter(evaluacion=="QUINTA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(area, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot() +
  geom_col(aes(x=area, y=Porcentaje), fill = "#076fa2", width = 0.6) 

library(hrbrthemes)
Data %>% 
  group_by(fecha,area) %>% 
  filter(evaluacion=="QUINTA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(area, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(aes(ymin = 0)) + 
  geom_rect(aes(xmin=area, xmax=area,ymax=Porcentaje, colour = area, fill = area)) +
  xlab("Escuelas") + 
  ylab("Porcentaje de examinados") +
  theme_ipsum() +
  theme(legend.position="none") 

##############################################
Data %>% 
  group_by(fecha, modalidad) %>% 
  filter(evaluacion=="QUINTA" && tipo =="ORDINARIA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(fecha, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(fecha) %>%   
  ggplot(mapping = aes(x=fecha, y=Porcentaje,fill=modalidad, shape=modalidad))+
  geom_bar(position = "dodge", stat = "identity")+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evalauación #" ,"QUINTA"))+
  geom_text(aes(x=fecha, y= Porcentaje, 
                label=Porcentaje),
            vjust=0,angle=0,size=3.5)
  

####################################################
y<-Data %>%
  group_by(fecha, facultad) %>% 
  filter(evaluacion=="QUINTA") %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n()) %>% 
  distinct(facultad, fecha, Porcentaje) %>% 
  arrange(fecha) %>%   
  ggplot(aes(x=fecha, y=Porcentaje)) +
  geom_line(aes(group=facultad, color=facultad)) +
  geom_point(aes(group=facultad, color=facultad))

y<-Data %>%
  group_by(fecha, facultad) %>%
  filter(evaluacion=="QUINTA") %>%
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n()) %>%
  distinct(facultad, fecha, Porcentaje) %>%
  arrange(fecha) %>%  
  ggplot(aes(x=fecha, y=Porcentaje)) +
  geom_line(aes(group=facultad, color=facultad)) +
  geom_point(aes(group=facultad, color=facultad))

##########################################################

Data %>%
  group_by(fecha) %>%
  filter(evaluacion=="QUINTA") %>%
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n()) %>%
  distinct(facultad, fecha, Porcentaje) %>%
  arrange(fecha) %>%  
  ggplot(aes(x=fecha, y=Porcentaje)) +
  geom_point(aes(color="green"))+
  geom_line(aes(group=facultad, color="blue"))+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evaluaci?n #" ,"QUINTA"))+
  geom_text(aes(x=fecha, y= Porcentaje,
                label=Porcentaje),
            vjust=0,angle=0,size=3.5)

################################################################
z<-Data %>%
  group_by(fecha, horario) %>%
  filter(evaluacion=="QUINTA") %>%
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>%
  distinct(fecha, horario, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>%
  arrange(fecha)
#################################################################
z<-Data %>%
  group_by(fecha, area, horario) %>%
  filter(evaluacion=="QUINTA") %>%
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>%
  distinct(fecha, area, horario, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>%
  arrange(fecha, area)
################################################################

DataE %>%
  group_by(fecha) %>%
  filter(evaluacion=='PRIMERA' && tipo =="ORDINARIA") %>%
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100)) %>%
  distinct(fecha, Porcentaje) %>%
  arrange(fecha) %>%
  ggplot(aes(x=fecha, y=Porcentaje, color="Tendencia")) +
  geom_point()+
  geom_line()+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evaluación #" ,"primera"))+
  geom_text(aes(x=fecha, y= Porcentaje,
                label=Porcentaje),
            vjust=0,angle=0,size=3.5)

DataE %>%
  group_by(fecha) %>%
  filter(evaluacion=='PRIMERA') %>%
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100)) %>%
  distinct(fecha, Porcentaje) %>%
  arrange(fecha) %>%
  ggplot(aes(x=fecha, y=Porcentaje, color="Tendencia")) +
  geom_point()+
  geom_line()+
  xlab("Fecha de examen parcial")+
  ylab("Porcentaje de examinados")+
  ggtitle(paste("Reporte de examinados por dia de la evaluación #" ,"primera"))+
  geom_text(aes(x=fecha, y= Porcentaje,
                label=Porcentaje),
            vjust=0,angle=0,size=3.5)

DataE %>% 
  group_by(modalidad, evaluacion) %>% 
  mutate(Porcentaje=perc_redondeado(mean(as.numeric(examinados)/as.numeric(inscritos))*100), Grupos_reportados=n(), Inscritos=sum(as.numeric(inscritos)), Examinados=sum(as.numeric(examinados))) %>% 
  distinct(modalidad, Grupos_reportados, Porcentaje, Inscritos, Examinados) %>% 
  arrange(evaluacion)    
