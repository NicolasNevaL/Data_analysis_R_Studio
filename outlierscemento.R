rm(list=ls())
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/eacf.R')
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/resumen.R')
library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)
datos=read_excel('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/prodcemento.xlsx',sheet=1)
datos=data.frame(datos)
head(datos)
nrow(datos)#número de datos en la serie

####Como hay 155 datos vamos a tomar los primeros 140 

cemento=ts(datos[1:140,2],start=c(2005,11),frequency=12)##87 datos para estimar el modelo
cemento
plot(cemento,type='l',col='blue',main='Produccción de cemento')
###1.1. Estabilización de varianza
BoxCox.lambda(cemento,method="loglik")
boxcox(cemento~1)


###1.2. Diferenciación
dy=diff(diff(cemento,12),1)###serie diferenciada

par(mfrow=c(2,1))
plot(cemento,main='Cemento en Nivel')
plot(dy,main='Cemento Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=48)
Pacf(dy,lag.max=48)
eacf(dy)

###############DETECCIÓN DE OUTLIERS
atipicos=tso(cemento,types = c("IO","AO", "LS", "TC", "SLS"),maxit = 1,
discard.method="bottom-up", maxit.iloop=10,maxit.oloop=10,
tsmethod = "arima", args.tsmethod = list(order = c(p=1,d=1,q=1),
seasonal=list(order=c(P=0,D=1,Q=1),period=12)))
atipicos
plot(atipicos)

####################SERIE CORREGIDA, SIN DATOS ATIPICOS
y=atipicos$yadj###SERIE AJUSTADA DE OUTLIERS
par(mfrow=c(2,1))
plot(y,type='l',main='Serie Ajustada de Datos Atípicos',col='blue')
matplot(cbind(cemento,y),col=c('red','blue'),type='l',main='Real Versus Ajustada de Datos Atípicos')

BoxCox.lambda(y,method="loglik")###lambda cercano a uno, no es necesario estabilizar varianza
boxcox(y~1)

dy=diff(diff(y,12),1)###ya el logaritmo esta incluido desde el principio, por lo que no debemos usarlo más

par(mfrow=c(2,1))
plot(y)
plot(dy)

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=48)
Pacf(dy,lag.max=48)
eacf(dy)

###################MODELO CON LA SERIE AJUSTADA POR DATOS ATIPICOS
modelo1=Arima(y,order=c(p=1,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo1
modelo2=Arima(y,order=c(p=0,d=1,q=4),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo2


###################MODELO
modelo=Arima(y,order=c(p=0,d=1,q=4),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo
resumen(modelo)


####3. CHEQUEO DEL MODELO
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)

##########Reajuste del modelo
modelo=Arima(y,order=c(p=22,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='CSS')#MA(1)
modelo
resumen(modelo)

####3. CHEQUEO DEL MODELO REAJUSTADO
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)


#####4. PRONÓSTICO

p=forecast(modelo,15)###comando para gráfica el pronóstico
plot(p,main='Producción de Cemento')

p=predict(modelo,15)

res=cbind(p$pred,datos[141:155,2],p$pred-datos[141:155,2],abs(p$pred-datos[141:155,2])/datos[141:155,2])
colnames(res)=c('Predicción','Valor real','Error de Predicción','MAPE')
res
mape=100*mean(abs(p$pred-datos[141:155,2])/datos[141:155,2])
mape
matplot(cbind(p$pred,datos[141:155,2]),col=c('blue','red'),type='l')


