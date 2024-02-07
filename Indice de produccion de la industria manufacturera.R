rm(list=ls())
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Procesos estacionarios/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Procesos estacionarios/R/resumen.R')

library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)
datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Series estacionales/Outliers/INDICE DE PRODUCCION REAL DE LA INDUSTRIA MANUFACTURERA COLOMBIANA.xlsx',sheet=1)
datos=data.frame(datos)
head(datos)
nrow(datos)#número de datos en la serie

## Se reservaran 9 datos para evaluar el modelo 

indice=ts(datos[1:76,2],start=c(2014,1),frequency=12)## 76 datos para estimar el modelo
plot(indice,type='l',col='blue',main='INDICE DE PRODUCCION REAL DE LA INDUSTRIA MANUFACTURERA COLOMBIANA')

###1.1. Estabilización de varianza
BoxCox.lambda(indice,method="loglik")
boxcox(indice~1)
abline(v=0.5,col='red')
####1.2. Diferenciación
# SE PROCEDERA A NO APLICAR ESTABILIZACION DE VARIANZA

dy=diff(diff((indice),12),1)###serie diferenciada

par(mfrow=c(2,1))
plot((indice),main='INDICE DE PRODUCCION REAL DE LA INDUSTRIA MANUFACTURERA COLOMBIANA en Nivel')
plot(dy,main='Primera Diferencia INDICE DE PRODUCCION REAL DE LA INDUSTRIA MANUFACTURERA COLOMBIANA')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=48)
Pacf(dy,lag.max=48)
eacf(dy)

###2. ESTIMACIÓN DEL MODELO DE LOS MODELOS
modelo1=Arima(indice,order=c(p=3,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(3)
modelo2=Arima(indice,order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo1
modelo2


#####################MODELO ESCOGIDO EL MA(1)
modelo2=Arima(indice,order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo2
(modelo2)
##### Chequeo del modelo
par(mfrow=c(2,1))
a=residuals(modelo2)
Acf(a,lag.max=48)
Pacf(a,lag.max=48)

###############DETECCIÓN DE OUTLIERS
atipicos=tso(indice,types = c("IO","AO", "LS", "TC", "SLS"),
             discard.method="bottom-up", maxit.iloop=10,maxit.oloop=10,tsmethod = "arima", 
             args.tsmethod = list(order = c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1))))
atipicos
plot(atipicos)

####################SERIE CORREGIDA, SIN DATOS ATIPICOS
y=atipicos$yadj###SERIE CORREGIDA, AJUSTADA
matplot(cbind(indice,y),col=c('red','blue'),type='l',main='Real Versus Ajustada de Datos Atípicos')

dyc=diff(diff(y,12),1)###ya el logaritmo esta incluido desde el principio, por lo que no debemos usarlo más

par(mfrow=c(2,1))
plot(y)
plot(dyc)

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dyc,lag.max=48)
Pacf(dyc,lag.max=48)
eacf(dyc)

###################MODELO CON LA SERIE AJUSTADA POR DATOS ATIPICOS
modelo3=Arima(y,order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo4=Arima(y,order=c(p=5,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(5)
modelo3
modelo4
resumen(modelo3)
resumen(modelo4)

####3. CHEQUEO DEL MODELO
b=residuals(modelo3)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(b,lag.max=24)
Pacf(b,lag.max=24)

##ajuste del modelo
modelo5=Arima(y,order=c(p=8,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#ARMA(8,1)
modelo5

C=residuals(modelo5)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(C,lag.max=24)
Pacf(C,lag.max=24)

##Eliminacion coeficientes NO significativos
modelo6=Arima(y,order=c(p=7,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#ARMA(8,1)
modelo6

d=residuals(modelo6)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(d,lag.max=24)
Pacf(d,lag.max=24)

#####4. PRONÓSTICO

p=forecast(modelo6,9)###comando para gráfica el pronóstico
plot(p,main='Pronóstico INDICE DE PRODUCCION REAL DE LA INDUSTRIA MANUFACTURERA COLOMBIANA')

#Escala de pronostico
p=predict(modelo6,9)##función de pronóstico
p$pred
###MAPE
mape=100*mean(abs(p$pred-datos[77:85,2])/datos[77:85,2])
mape

resultados=cbind(datos[77:85,2],p$pred)
colnames(resultados)=c('Real','Pronósticado')

plot(resultados,plot.type="single",col=c('red','blue'),main='Real Versus Pronósticado ')
legend("bottomleft", colnames(resultados),col=c('red','blue'),lty=1,cex=.65)
















