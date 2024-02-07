rm(list=ls())
library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/eacf.R')###PILAS CON LA RUTA
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/resumen.R')###PILAS CON LA RUTA
datos=read.table('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/series estacionales/pib.txt',header=TRUE)
datos=data.frame(datos)
head(datos)
nrow(datos)#número de datos en la serie

pib=ts(datos[1:50,1],start=c(2010,1),frequency=4)##87 datos para estimar el modelo
pib
###1.1. Estabilización de varianza
plot(pib,main='PIB TRIMESTRAL DESDE EL 2005')
BoxCox.lambda(pib,method="loglik")
boxcox(pib~1)
abline(v=0.5,col='red')## NO NECESITAMOS EN ESTOS DATOS ESTABILIZAR VARIANZA

####1.2. Diferenciación
par(mfrow=c(2,1))
dy=diff(diff(sqrt(pib),1),4)###sqrt=raiz en R, también se puede dy=diff(diff(sqrt(pib),4),1)
plot(sqrt(pib),main='sqrt(PIB) en Nivel')
plot(dy,main='sqrt(PIB) Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=24)
Pacf(dy,lag.max=24)
eacf(dy)

###2. ESTIMACIÓN DEL MODELO
modelo1=Arima(sqrt(pib),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=4),method='ML')#MA(1)
modelo1
resumen(modelo1)

modelo2=Arima(sqrt(pib),order=c(p=4,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(4)
modelo2
resumen(modelo2)

######################MODELO 1 SELECCIONADO, DE MÁS BAJO AIC
modelo=Arima(sqrt(pib),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=4),method='ML')#MA(1)
resumen(modelo)


####3. Chequeo del modelo
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)

##############Reajuste del modelo
modelo=Arima(sqrt(pib),order=c(p=0,d=1,q=17),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=4),method='ML')#AR(4)
resumen(modelo)

##########chequeo del modelo
####3. Chequeo del modelo
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)

##############Reajuste del modelo, despues de quitar términos no significativos
modelo=Arima(sqrt(pib),order=c(p=0,d=1,q=9),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=4),method='ML')#AR(4)
resumen(modelo)


##########chequeo del modelo
####3. Chequeo del modelo
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)


#####PRUEBA DE NORMALIDAD
JarqueBera.test(a)

#####4. PRONÓSTICO
pib
p=forecast(modelo,6)###comando para gráfica el pronóstico
plot(p,main='Pronosttico del precio 6 Trimestres Adelante')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
pib##variable de análisis
p=predict(modelo,6)##función de pronóstico
p
p$pred^2
###MAPE
mape=100*mean(abs(p$pred^2-datos[51:56,1])/datos[51:56,1])
mape

matplot(cbind(p$pred^2,datos[51:56,1]),col=c('red','blue'),xlab='TIEMPO',ylab='PIB',main='Real Rojo, Azul Pronosticado',type='l')

####PRONOSTICO 12 PERIODOS ADELANTE
pib=ts(datos[1:56,1],start=c(2010,1),frequency=4)##87 datos para estimar el modelo
pib
modelo=Arima(sqrt(pib),order=c(p=0,d=1,q=9),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=4),method='ML')#AR(4)
modelo
p=forecast(modelo,12)###comando para gráfica el pronóstico
plot(p,main='Pronosttico del precio 12 Trimestres Adelante')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
pib##variable de análisis
p=predict(modelo,12)##función de pronóstico
p
p$pred^2
