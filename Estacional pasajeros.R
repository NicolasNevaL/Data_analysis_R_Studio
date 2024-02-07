rm(list=ls())
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/R/resumen.R')

library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)

datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Unidad 1/1 parcial/parcialeconometria2.xlsx',sheet=1)
head(datos)


pasaj=ts(datos[1:144,2], start=c(2008,1),frequency=12)##162 datos para estimar el modelo
pasaj

###1.1. Estabilización de varianza
plot(pasaj,main='pasaj mensual DESDE EL 2005')
BoxCox.lambda(pasaj,method="loglik")
boxcox(pasaj~1)
abline(v=0.5,col='red')## NO NECESITAMOS EN ESTOS DATOS ESTABILIZAR VARIANZA
## Aplicar logaritnmo natural 

####1.2. Diferenciación
par(mfrow=c(2,1))
dy=diff(diff(log(pasaj),1),12)###log=raiz en R, también se puede dy=diff(diff(log(pasaj),4),1)
plot(log(pasaj),main='log(pasaj) en Nivel')
plot(dy,main='log(pasaj) Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=24)
Pacf(dy,lag.max=24)
eacf(dy)

###2. ESTIMACIÓN DEL MODELO MA(1)
modelo1=Arima(log(pasaj),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo1
resumen(modelo1)


####3. Chequeo del modelo
a=residuals(modelo1)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=35)
Pacf(a,lag.max=35)

##############Reajuste del modelo en el rezago 16
modelo2=Arima(log(pasaj),order=c(p=16,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(4)
resumen(modelo2)
### El coeficiente es significativo

##########chequeo del modelo
####3. Chequeo del modelo
b=residuals(modelo2)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(b,lag.max=35)
Pacf(b,lag.max=35)


#####PRUEBA DE NORMALIDAD
JarqueBera.test(b)

#####4. PRONÓSTICO
pasaj
p=forecast(modelo2,16)###comando para gráfica el pronóstico
plot(p,main='Pronosttico del precio 16 mese Adelante')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
pasaj##variable de análisis
p=predict(modelo2,16)##función de pronóstico
p
exp(p$pred)###pronósticos en unidades originales
###MAPE
datos2= datos[145:160,2]
mape=100*mean(abs(exp(p$pred)-datos2)/datos2)
mape

matplot(cbind(exp(p$pred),datos[145:160,2]),col=c('red','blue'),xlab='TIEMPO',ylab='pasaj',main='Real Rojo, Azul Pronosticado',type='l')

####PRONOSTICO 12 PERIODOS ADELANTE
pasaj=ts(datos[1:160,2],start=c(2008,1),frequency=12)##160 datos para estimar el modelo
pasaj
modelo3=Arima(log(pasaj),order=c(p=16,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(4)
modelo3
p=forecast(modelo3,12)###comando para gráfica el pronóstico
plot(p,main='Pronosttico del precio 12 mese Adelante')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
pasaj##variable de análisis
p1=predict(modelo3,12)##función de pronóstico
p1
exp(p1$pred)###pronósticos en unidades originales
