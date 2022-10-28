library(tidyverse)

apps <- read_csv("https://raw.githubusercontent.com/cienciadedatos/datos-de-miercoles/master/datos/2019/2019-07-24/apps_googleplaystore.csv")

apps <- read_csv(paste(getwd(),"/R/googleplaystore.csv", sep = ""))
#www.kaggle.com/

glimpse(apps)

apps %>% 
  count(Category, sort=TRUE) %>% 
  mutate(categoria=fct_reorder(Category,n)) %>%
  ggplot(aes(x=Category, y=n, label=n))+
    geom_col() +
    geom_text(hjust=1, colour="white")+
    coord_flip()+
    labs(title = "Cantidad de aplicaciones por categoria en Play Store", 
         x="categoria", y="frecuencia absoluta", 
         caption = "Fuente de datos: kaggle")



perc_redondeado<-function(x)
{
  paste(round(x,4)*100,"%",sep = "")
}

apps %>% 
  count(categoria, sort=TRUE) %>% 
  mutate(perc=n/sum(n),
        perc_acum=cumsum(perc),
        categoria=fct_reorder(categoria,-n)) %>%
  ggplot(aes(x=categoria, y=perc))+
  geom_col() +
  geom_point(aes(x=categoria, y= perc_acum, group=1))+
  geom_line(aes(x=categoria, y= perc_acum, group=1))+
  geom_text(aes(x=categoria, y= perc_acum, group=1, 
                  label=perc_redondeado(perc_acum)),
              vjust=0,angle=30,size=3.5)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(title = "Cantidad de aplicaciones por categoria en Play Store", 
       x="categoria", y="frecuencia absoluta", 
       caption = "Fuente de datos: kaggle")
