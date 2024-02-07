rm(list=ls())
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/resumen.R')

library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)

datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 2/pasajerosaerolinea.xlsx',sheet=1)
head(datos)
nrow(datos)#número de datos en la serie

pasajeros=ts(datos[1:130,1],start=c(2008,1),frequency=12)##87 datos para estimar el modelo
pasajeros
###1.1. Estabilización de varianza
plot(pasajeros,main='pasajeros de aerolinea')
BoxCox.lambda(pasajeros,method="loglik")
boxcox(pasajeros~1)
abline(v=0.5,col='red')

par(mfrow=c(2,1))
plot(pasajeros,main='pasajeros de aerolinea')
plot(log(pasajeros),main='pasajeros aerolinea')### serie con varianza estable

####1.2. Diferenciación
par(mfrow=c(2,1))
dy=diff(diff(log(pasajeros),12),1)### es equivalente a dy=diff(diff(log(pasajeros),1),12)
plot(log(pasajeros),main='log(pasajeros) en Nivel')
plot(dy,main='pasajeros Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF para diff(sqrt(pasajeros,1)
par(mfrow=c(2,1))
Acf(dy,lag.max=24)
Pacf(dy,lag.max=24)
eacf(dy)


#### Modelo arima(5,1,1) y MA(3)
###2. ESTIMACIÓN DEL MODELO

modelo1=Arima(log(pasajeros),order=c(p=5,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#ARIMA(5,1,1)
modelo2=Arima(log(pasajeros),order=c(p=0,d=1,q=3),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(3)
modelo1
modelo2

#################Seleccionamos el modelo 3 ya que tiene el menor AIC
modelo3=Arima(log(pasajeros),order=c(p=0,d=1,q=3),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo3
z=modelo3$coef/sqrt(diag(vcov(modelo3)))##valores z de los coeficientes del modelo estimado
abs(z)
resumen(modelo3)

####3. Chequeo del modelo
a=residuals(modelo3)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=60)
Pacf(a,lag.max=60)

###PRIMER REAJUSTE DEL MODELO
modelo4=Arima(log(pasajeros),order=c(p=49,d=1,q=3),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(5)
resumen(modelo4)
####3. Chequeo del modelo reajustado
b=residuals(modelo4)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=60)
Pacf(a,lag.max=60)

#####PRUEBA DE NORMALIDAD
JarqueBera.test(a)

#####4. PRONÓSTICO

p=forecast(modelo4,14)###comando para gráfica el pronóstico
plot(p,main='Pronosttico las Ventas 9 Meses Adelante')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
pasajeros##variable de análisis
p=predict(modelo4,14)##función de pronóstico
p
exp(p$pred)###pronósticos en unidades originales
###MAPE
datos2=datos[130:144,1]
mape=100*mean(abs(exp(p$pred)-datos2)/datos2)
mape

matplot(cbind(exp(p$pred),datos[88:96,1]),col=c('red','blue'),xlab='TIEMPO',ylab='Ventas',main='Real Rojo, Azul Pronosticado',type='l')

