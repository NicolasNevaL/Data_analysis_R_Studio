rm(list=ls())
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/resumen.R')

library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)

datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 2/ventasmensuales.xlsx',sheet=1)
head(datos)
nrow(datos)#n�mero de datos en la serie

ventasm=ts(datos[1:86,1],start=c(2008,1),frequency=12)##86 datos para estimar el modelo
ventasm
###1.1. Estabilizaci�n de varianza
plot(ventasm,main='ventasm mensuales')
BoxCox.lambda(ventasm,method="loglik")
boxcox(ventasm~1)
abline(v=0.5,col='red')

par(mfrow=c(2,1))
plot(ventasm,main='ventasm de mensuales')
plot(log(ventasm),main='ventasm aerolinea')### serie con varianza estable

####1.2. Diferenciaci�n
par(mfrow=c(2,1))
dy=diff(diff(log(ventasm),12),1)### es equivalente a dy=diff(diff(log(ventasm),1),12)
plot(log(ventasm),main='log(ventasm) en Nivel')
plot(dy,main='ventasm Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF para diff(sqrt(ventasm,1)
par(mfrow=c(2,1))
Acf(dy,lag.max=24)
Pacf(dy,lag.max=24)
eacf(dy)


#### Modelo arima(1,1,1) y ma(1)
###2. ESTIMACI�N DEL MODELO

modelo1=Arima(log(ventasm),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#ARIMA(1,1,1)
modelo1

####3. Chequeo del modelo
a=residuals(modelo1)##obtenmos residuales
par(mfrow=c(2,1))#dos gr�ficos en una ventana
Acf(a,lag.max=40)
Pacf(a,lag.max=40)

###PRIMER REAJUSTE DEL MODELO
modelo2=Arima(log(ventasm),order=c(p=14,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(5)
resumen(modelo2)
####3. Chequeo del modelo reajustado
b=residuals(modelo2)##obtenmos residuales
par(mfrow=c(2,1))#dos gr�ficos en una ventana
Acf(b,lag.max=40)
Pacf(b,lag.max=40)

#####PRUEBA DE NORMALIDAD
JarqueBera.test(b)

#####4. PRON�STICO

p=forecast(modelo2,10)###comando para gr�fica el pron�stico
plot(p,main='Pronosttico las Ventas 10 Meses Adelante')

####PARA SACAR LA ESCALA DE PRON�STICO ORIGINAL
ventasm##variable de an�lisis
p=predict(modelo2,10)##funci�n de pron�stico
p
exp(p$pred)###pron�sticos en unidades originales
###MAPE
datos2=datos[87:96,1]
mape=100*mean(abs(exp(p$pred)-datos2)/datos2)
mape

matplot(cbind(exp(p$pred),datos[88:96,1]),col=c('red','blue'),xlab='TIEMPO',ylab='Ventas',main='Real Rojo, Azul Pronosticado',type='l')
############ preguntar por el error############################

####PRONOSTICO 12 PERIODOS ADELANTE
ventasm=ts(datos[1:96,1],start=c(2010,1),frequency=12)##96 datos para estimar el modelo
ventasm
modelo3=Arima(sqrt(ventasm),order=c(p=14,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(4)
modelo3
p=forecast(modelo3,12)###comando para gr�fica el pron�stico
plot(p,main='Pronosttico del precio 12 Trimestres Adelante')

####PARA SACAR LA ESCALA DE PRON�STICO ORIGINAL
ventasm##variable de an�lisis
p=predict(modelo3,12)##funci�n de pron�stico
p
exp(p$pred)
