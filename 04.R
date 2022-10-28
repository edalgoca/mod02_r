library(ggplot2)

mtcars

#Graficos que consideran una variable continua
##histograma de frecuencia

ggplot(data=mtcars, aes(x=mpg)) +
  geom_histogram(binwidth = 2.5)+
  geom_density()

ggplot(data = mtcars)+
  geom_point(aes(x=mpg, y=cyl))+
  geom_line(aes(x=mpg, y=hp))


ggplot(data = mtcars, mapping = aes(x=mpg, y=cyl))+
  geom_point()+
  geom_line()




##grafico de densidad
ggplot(data=mtcars, aes(x=mpg))+
  geom_density()

##grafico de puntos
ggplot(data=mtcars, aes(x=mpg))+
  geom_dotplot()+
  geom_density()

##poligono de frecuencias
ggplot(data=mtcars, aes(x=mpg))+
  geom_freqpoly()


#graficos que consideran una variable discreta
##grafico de barras
ggplot(data=mtcars, aes(x=factor(gear)))+
  geom_bar()

#graficos que consideran dos variables continuas
##grafico de dispersion o de puntos bivariadas
ggplot(data=mtcars,aes(x=mpg, y = hp))+
  geom_point()+
  geom_line()

ggplot(data=mtcars)+
  geom_point(aes(x=mpg, y = hp))

##grafico de dispersiion con variacion aleatoria (jitter), una peque?a dispersion entre el grafico anterior y este (util para dos variables continuas y no discretas)
ggplot(data=mtcars,aes(x=mpg, y = hp))+
  geom_jitter()

##grafico de alfombra, manta, tapete (rug), para utilizarlo en conjunto con otros graficos
ggplot(data=mtcars,aes(x=mpg, y = hp))+
  geom_rug()

##grafico que consideran dos variables (continuas y discretas)
ggplot(data=mtcars,aes(x=factor(gear), y=mpg))+
  geom_col()

##grafico de cajas o bigotes
par(mfrow=c(1,2))

ggplot(data=mtcars,aes(x=factor(gear), y=mpg))+
  geom_boxplot()

7##grafico de violin
ggplot(data=mtcars,aes(x=factor(gear), y=mpg, colour=c("blue","red","yellow")))+
  geom_violin()
