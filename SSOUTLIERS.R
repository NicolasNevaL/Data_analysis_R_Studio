rm(list=ls())
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/eacf.R')
source('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/resumen.R')
library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)
datos=read_excel('C:/Users/MAURICIO/Desktop/Clases 2020/SERIES DE TIEMPO/exportacionescafe.xlsx',sheet=1)
datos=data.frame(datos)
datos$toneladas=datos$toneladas/1000
head(datos)
nrow(datos)#número de datos en la serie

####Como hay 93 datos vamos a tomar los primeros 7 años (84 datos)

cafe=ts(datos[1:84,2],start=c(2012,1),frequency=12)##87 datos para estimar el modelo
plot(cafe,type='l',col='blue',main='Exportaciones de Café en Miles de Toneladas')
###1.1. Estabilización de varianza
BoxCox.lambda(cafe,method="loglik")
boxcox(cafe~1)
abline(v=0.5,col='red')
####1.2. Diferenciación

dy=diff(diff(log(cafe),12),1)###serie diferenciada

par(mfrow=c(2,1))
plot(log(cafe),main='log(Café) en Nivel')
plot(dy,main='log(Café) Diferenciada')
##la serie diferenciada ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=48)
Pacf(dy,lag.max=48)
eacf(dy)

###2. ESTIMACIÓN DEL MODELO DE LOS MODELOS
modelo1=Arima(log(cafe),order=c(p=1,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#AR(1)
modelo2=Arima(log(cafe),order=c(p=0,d=1,q=2),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(2)
modelo1
modelo2

#####################MODELO ESCOGIDO EL AR(1)
modelo=Arima(log(cafe),order=c(p=1,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(4)
modelo
resumen(modelo)

#####quitamos el componente AR(1) por no significativo
modelo=Arima(log(cafe),order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(4)
resumen(modelo)



###############DETECCIÓN DE OUTLIERS
atipicos=tso(log(cafe),types = c("IO","AO", "LS", "TC", "SLS"),
discard.method="bottom-up", maxit.iloop=10,maxit.oloop=10,tsmethod = "arima", 
args.tsmethod = list(order = c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1))))
atipicos
plot(atipicos)


####################SERIE CORREGIDA, SIN DATOS ATIPICOS
y=atipicos$yadj###SERIE CORREGIDA, AJUSTADA
matplot(cbind(log(cafe),y),col=c('red','blue'),type='l',main='Real Versus Ajustada de Datos Atípicos')


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
modelo=Arima(y,order=c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
resumen(modelo)



####3. CHEQUEO DEL MODELO
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)



#####4. PRONÓSTICO

p=forecast(modelo,9)###comando para gráfica el pronóstico
plot(p,main='Pronóstico de las Exportaciones de Café')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
p=predict(modelo,9)##función de pronóstico
exp(p$pred)
###MAPE
mape=100*mean(abs(exp(p$pred)-datos[85:93,2])/datos[85:93,2])
mape

resultados=cbind(datos[85:93,2],exp(p$pred))
colnames(resultados)=c('Real','Pronósticado')

plot(resultados,plot.type="single",col=c('red','blue'),main='Real Versus Pronósticado en Millones de Toneladas')
legend("bottomleft", colnames(resultados),col=c('red','blue'),lty=1,cex=.65)



