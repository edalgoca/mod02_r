library(forecast)
library(foreign)
library(tseries)
library(xts)
library(astsa)

# serie de tiempo de precipitación
ruta<-paste(getwd(),"/R/archivos/series de tiempo.csv",sep = "")
Precipitacion <- read.csv(ruta, header=TRUE)
Precipitacion
plot(Precipitacion,col="blue")
ats=ts(Precipitacion, start = c(1959,1), frequency=12)
ats
plot(ats)
sd(ats)
mean(ats)
seasonplot(ats, col = rainbow(12), year.labels = TRUE)
atsdesc = decompose(ats)# desconposición 1
descomp2=stl(ats,s.window="periodic")
str(descomp2)
plot(atsdesc, xlab='Año')
boxplot(ats ~ cycle(ats))
plot(ats,lty="dashed", ylab="Precipitacion",xlab="Años", main="Serie de Tiempo Quevedo")
plot.ts(ats)
ts.plot(ats)

#x11()
acf(ats) #autocorrelación simple
pacf(ats) #autocorrelación parcial
ajuste=sarima(ats, 2,0,0)

a=predict(ats)
plot(a)
sarima.for(ats, 10,4,2, n.ahead=10)