#Implementar algoritmos de arboles binarios
library(party)

vinos <-read.table(paste(getwd(),"/R/wine.data", sep=""),sep = ",")

names(vinos)<-c("cultivo","Alcohol","Ácido_málico","Ceniza","Alcalinidad",
                "Magnesio","Fenoles_totales","Flavonoides","Fenoles_flavonoides",
                "Proantocianinas","Intensidad_color","Tono","vinos_diluidos","Prolina")

modelo <- ctree(cultivo ~ ., data = vinos)
plot(modelo)

################################################

# Dataset
# install.packages('titanic')
# Libreria de clasificacion
# install.packages("rpart")
# install.packages("rattle")
# install.packages("rpart.plot")

# Llamar librerias
library(tidyverse)
library(titanic)
library(rpart)
library(rattle)
library(rpart.plot)

# Cargar dataset
data("titanic_train")


# Modelo arbol
arbol <- rpart(formula = Survived ~ Sex + Age, data = titanic_train, 
               method = 'class')

# Graficar
fancyRpartPlot(arbol)

# Prediccion
arbol_pred <- predict(arbol, type = 'class')

titanic_pre <- cbind(titanic_train, arbol_pred)

# Predecir pasajero x
predict(object = arbol, newdata = data.frame(Age = 25, Sex = 'male'), type = 'class')
