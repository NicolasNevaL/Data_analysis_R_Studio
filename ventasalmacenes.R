rm(list=ls())
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/eacf.R')###PILAS CON LA RUTA
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/resumen.R')###PILAS CON LA RUTA
library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)
datos=read_excel('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/series estacionales/ventasalmacenes.xls',sheet=1)
datos=data.frame(datos)
head(datos)
nrow(datos)#n�mero de datos en la serie

ventas=ts(datos[1:87,1],start=c(2010,1),frequency=12)##87 datos para estimar el modelo
ventas
###1.1. Estabilizaci�n de varianza
plot(ventas,main='Ventas de Cadena de Almacenes')
BoxCox.lambda(ventas,method="loglik")
boxcox(ventas~1)
abline(v=0.5,col='red')

par(mfrow=c(2,1))
plot(ventas,main='Ventas de Cadena de Almacenes')
plot(log(ventas),main='Ventas de Cadena de Almacenes Estabilizada en Varianza')### serie con varianza estable

####1.2. Diferenciaci�n
par(mfrow=c(2,1))
dy=diff(diff(log(ventas),12),1)### es equivalente a dy=diff(diff(log(ventas),1),12)
plot(log(ventas),main='log(Ventas) en Nivel')
plot(dy,main='Ventas Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF para diff(sqrt(cafe),1)
par(mfrow=c(2,1))
Acf(dy,lag.max=24)
Pacf(dy,lag.max=24)
eacf(dy)

###2. ESTIMACI�N DEL MODELO
modelo1=Arima(log(ventas),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo2=Arima(log(ventas),order=c(p=6,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(6)
modelo1
modelo2

#################Seleccionamos el modelo 1 ya que tiene el menor AIC
modelo=Arima(log(ventas),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo
z=modelo$coef/sqrt(diag(vcov(modelo)))##valores z de los coeficientes del modelo estimado
abs(z)
resumen(modelo)

####3. Chequeo del modelo
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gr�ficos en una ventana
Acf(a,lag.max=25)
Pacf(a,lag.max=25)

###PRIMER REAJUSTE DEL MODELO
modelo=Arima(log(ventas),order=c(p=5,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(5)
resumen(modelo)
####3. Chequeo del modelo reajustado
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gr�ficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)

###SEGUNDO REAJUSTE DEL MODELO
modelo=Arima(log(ventas),order=c(p=5,d=1,q=12),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(13)
resumen(modelo)

####3. Chequeo del modelo
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gr�ficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)



#####PRUEBA DE NORMALIDAD
JarqueBera.test(a)

#####4. PRON�STICO

p=forecast(modelo,9)###comando para gr�fica el pron�stico
plot(p,main='Pronosttico las Ventas 9 Meses Adelante')

####PARA SACAR LA ESCALA DE PRON�STICO ORIGINAL
ventas##variable de an�lisis
p=predict(modelo,9)##funci�n de pron�stico
p
exp(p$pred)###pron�sticos en unidades originales
###MAPE
mape=100*mean(abs(exp(p$pred)-datos[88:96,1])/datos[88:96,1])
mape

matplot(cbind(exp(p$pred),datos[88:96,1]),col=c('red','blue'),xlab='TIEMPO',ylab='Ventas',main='Real Rojo, Azul Pronosticado',type='l')



