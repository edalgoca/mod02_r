library(tidyverse)
#compuesto por los paquetes a utilizar en casi todos los análisis de datos
install.packages("datos")
library(datos)

#1. ¿los automóviles con motores grandes consumen más combustible que los automóviles con motores pequeños?
?millas
#cilindrada: tamaño del motor del automóvil, en litros.
#autopista: eficiencia del uso de combustible de un automóvil en carretera, en millas por galón. Al recorrer la misma distancia, un automóvil de baja eficiencia consume más combustible que un automóvil de alta eficiencia.

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista))

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point()

#Diferenciando el tipo de vehiculos
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista, color=clase)) +
  geom_point()

#propiedades a clase puede ser->color, size, alpha (Hace referencia a la transparencia), shape (Hace referencia a la forma del punto)
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, alpha = clase))

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista, shape = clase))

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista), color = "blue")

#Para separar en facetas un gráfico según una sola variable, utiliza facet_wrap() (del inglés envolver una faceta). 
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_wrap(~ clase, nrow = 2)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(traccion ~ cilindros)

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  facet_grid(. ~ cilindros)

#tipos de graficas
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista))

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))
#para lineas
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, linetype = traccion))
###########multiples esteticas

ggplot(data = millas) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista, group = traccion))

ggplot(data = millas) +
  geom_smooth(
    mapping = aes(x = cilindrada, y = autopista, color = traccion),
    show.legend = FALSE
  )
#Para mostrar múltiples geoms en el mismo gráfico, agrega varias funciones geom a ggplot():
ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(mapping = aes(x = cilindrada, y = autopista))

ggplot(data = millas) +
  geom_point(mapping = aes(x = cilindrada, y = autopista)) +
  geom_smooth(
    mapping = aes(x = cilindrada, y = autopista, color = traccion),
    show.legend = FALSE
  )
#incluir el mapeo en la capa de datos y solo modificar el geom
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(color = clase)) +
  geom_smooth()

ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(color = clase, shape=traccion)) +
  geom_smooth()

#aplicando filtros a los datos
ggplot(data = millas, mapping = aes(x = cilindrada, y = autopista)) +
  geom_point(mapping = aes(color = clase)) +
  geom_smooth(data = filter(millas, clase == "subcompacto"), se = FALSE)

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, colour = corte))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = corte))

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad))

ggplot(data = diamantes, mapping = aes(x = corte, fill = claridad)) +
  geom_bar(alpha = 1/5, position = "identity")

ggplot(data = diamantes, mapping = aes(x = corte, colour = claridad)) +
  geom_bar(fill = NA, position = "identity")

ggplot(data = diamantes) +
  geom_bar(mapping = aes(x = corte, fill = claridad), position = "dodge")
