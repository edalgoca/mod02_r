#datos demograficos de los paises a lo largo de a?os
library(gapminder)

#cargando data en entorno
data("gapminder")

#ver los datos
head(gapminder)

#se haran filtros por a?o, pais y hacer resumenes

library(tidyverse)
#filtrar sin %>%
filter(gapminder, country=='El Salvador')


#filtrar con %>%  (ctrl+shift+m)
gapminder %>% 
  filter(country=='El Salvador')

gapminder %>% 
  filter(country=='El Salvador')

gapminder %>% 
  filter(year>=1972 & lifeExp >=40)

gapminder %>% 
  filter(year>=1972, lifeExp>=40)

#resumenes
gapminder %>% 
  filter(continent=='Americas', year>=1972) %>% 
  summarise(conteo=n())

gapminder %>% 
  filter(year>=1972, lifeExp>=40) %>% 
  summarise(conteo=n())

#maxima esperanza de vida
gapminder %>% 
  summarise(maxima_experanza_vida=max(lifeExp))

#agrupando esperanza de vida promedio por a?o
gapminder %>% 
  group_by(year) %>% 
  summarise(promedio_vida_anul=mean(lifeExp))

data_x<-read_csv(paste(getwd(),"/mydata_cd.csv",sep = ""))

DataJSON %>% 
  select(Facultad, Coordinacion_Catedra,  Asignatura, Docente)

data_x %>% 
  group_by(GRUPOS) %>% 
  select(Carnet, Total) %>% 
  summarise(numero_estudiantes=n(), promedio=mean(Total), suma_notas=sum(Total))
