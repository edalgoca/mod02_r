library(ggplot2)
iris
?ggplot()
#data, estetica+geometria
ggplot(data = iris, aes(x=Sepal.Width, y=Sepal.Length))+
  geom_point()

ggplot(data = iris)+
  geom_point(aes(x=Sepal.Width, y=Sepal.Length))

#data, estetica y color +geome tria
ggplot(data = iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species))+
  geom_point()

ggplot(data = iris)+
  geom_point(aes(x=Sepal.Width, y=Sepal.Length, colour=Species))

#utilizando gramatica
p<-ggplot(data = iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species))

p<-p + geom_point()
p<-p+geom_line()
p

#agregando estilo a la geometria
p<-ggplot(data = iris, aes(x=Sepal.Width, y=Sepal.Length, colour=Species))
p<-p+geom_point(aes(shape=Species))
p

#agregando etiquetas
p<-p + xlab("Sepal length")+
  ylab("Sepal width")+
  ggtitle("Sepal length vrs width")

p
#Otra forma
ggplot(data = iris, 
       aes(x=Sepal.Width, y=Sepal.Length, 
           colour=Species))+
  geom_point(aes(shape=Species)) + 
  xlab("Sepal length")+
  ylab("Sepal width")+
  ggtitle("Sepal length vrs width")+
  geom_smooth(method = "lm")+
  facet_grid(Species ~ .)



# ajustar una linea para simular una regresion lineal (suaviz<a una linea)
?geom_smooth
p<-p+geom_smooth(method = "lm")
p
#data,estetica+geometria+faceta
p<- p+facet_grid(. ~ Species)
p
p<- p+facet_grid(Species ~ .)