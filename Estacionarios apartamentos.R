rm(list=ls())
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/resumen.R')

library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)

datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/1 parcial/parcialeconometria2.xlsx',sheet=2)
head(datos)


apts=ts(datos[1:75,2], start=c(2008,1),frequency=12)##162 datos para estimar el modelo
apts

plot(apts,main='índice mensual de precios de los apartamentos')

# no se estabiliza varianza

#1.2 DIFERENCIACIÓN DE LA SERIE

par(mfrow=c(2,1))
plot((apts),main='índice mensual de precios de los apartamentos')
plot(diff(apts,1),main='primera diferencia de índice mensual de precios de los apartamentos')
##La primera diferencia ya es estacionaria

plot(diff(diff(apts,2)),main='Segunda diferencia de índice mensual de precios de los apartamentos')

####PRUEBA ADF PARA LA PRIMERA DIFERENCIA
prueba1=ur.df(diff(diff(apts),2), type = c("none"), lags=10) 
summary(prueba1)
plot(prueba1)

####PRUEBA ADF para primera diferencia
prueba2=ur.df(diff(apts), type = c("trend"), lags=10)
summary(prueba2)
plot(prueba2)
##Concluímos que la serie es integrada de orden 1 ya que la serie en nivel tiene una raiz unitaria

####PRUEBA ADF A LA SERIE EN NIVEL
prueba3=ur.df(apts, type = c("trend"), lags=10)
summary(prueba3)
plot(prueba3)
##Concluímos que la serie es integrada de orden 2 ya que la serie en nivel tiene una raiz unitaria

##preguntar tendencia significativa


###1.3. Aplicamos ACF, PACF y EACF para diff(sqrt(apts),1)
par(mfrow=c(2,1))
Acf(diff(diff(apts),2),lag.max=25)
Pacf(diff(diff(apts),2),lag.max=25)
eacf(diff(diff(apts),1))
##Proceso AR(3), Ma(1)

###2. ESTIMACIÓN DEL MODELO
modelo=Arima(apts,order=c(p=3,d=2,q=0),method='ML')
modelo
z=modelo$coef/sqrt(diag(vcov(modelo)))
z
resumen(modelo)

modelo2=Arima(apts,order=c(p=0,d=2,q=1),method='ML')
modelo2
z1=modelo2$coef/sqrt(diag(vcov(modelo2)))
z1
resumen(modelo2)

#3. CHEQUEO DEL MODELO
a=residuals(modelo2)
par(mfrow=c(2,1))
Acf(a,lag.max=25)
Pacf(a,lag.max=25)

#####PRUEBA DE NORMALIDAD
JarqueBera.test(a)
##Preguntar

#####4. PRONÓSTICO

p=forecast(modelo2,30)###comando para gráfica el pronóstico
plot(p,main='Pronostico del la tasa de interes EEUU')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
apts##variable de análisis
p=predict(modelo2,12)##función de pronóstico
p
length(p)
###MAPE
datos2=datos[76:87,2]
datos2
mape=100*mean(abs(p-datos2)/datos2)
mape
matplot(cbind(p,datos2),col=c('red','blue'),main='Real Rojo, Azul Pronosticado',type='l')


