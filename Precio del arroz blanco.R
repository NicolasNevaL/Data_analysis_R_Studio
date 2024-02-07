source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Procesos estacionarios/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Procesos estacionarios/R/resumen.R')


library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)

datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Series estacionales/Outliers/Precio arroz blanco.xlsx',sheet=1)
datos=data.frame(datos)
head(datos)

precioarroz=ts(datos[1:110,2], start=c(2011,1),frequency=12)##200 datos para estimar el modelo
precioarroz

plot(precioarroz,type='l',col='blue',main='Precio Promedio Mensual Arroz blanco en Colombia, Pesos / Tonelada, 2010 - 2021 ')
## Proceso no estacional 
###1.1. Estabilización de varianza
## No es necesario estabilizar varianza
####1.2. Diferenciación

dy=diff((precioarroz),1)###serie diferenciada
dy2=diff(diff((precioarroz),1)) # segunda diferencia
par(mfrow=c(2,1))
plot((precioarroz),main='Precio Promedio Mensual Arroz blanco en Colombia')
plot(dy,main='Precio Promedio Mensual Arroz blanco en Colombia Diferenciada')
plot(dy2,main='Precio Promedio Mensual Arroz blanco en Colombia segunda Diferencia')

##la serie en segunda diferencia ya es estacionaria

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy2,lag.max=48)
Pacf(dy2,lag.max=48)
eacf(dy2)
# 

###2. ESTIMACIÓN DEL MODELO DE LOS MODELOS
modelo1=Arima(precioarroz,order=c(p=2,d=2,q=1),method='ML')#ARIMA(2,2,2)
modelo1
modelo2=Arima(precioarroz,order=c(p=0,d=2,q=2),method='ML')#ARIMA(0,2,2)
modelo2

## Eleccion del modelo 
modelo=Arima(precioarroz,order=c(p=0,d=2,q=2),method='ML')#ARIMA(0,2,2)
resumen(modelo)

###############DETECCIÓN DE OUTLIERS
atipicos=tso(precioarroz,types = c("IO","AO", "LS", "TC", "SLS"),
discard.method="bottom-up", maxit.iloop=10,maxit.oloop=10,tsmethod = "arima", 
args.tsmethod = list(order = c(p=0,d=2,q=2)))
atipicos
plot(atipicos)


####################SERIE CORREGIDA, SIN DATOS ATIPICOS
y=atipicos$yadj###SERIE CORREGIDA, AJUSTADA
matplot(cbind(precioarroz,y),col=c('red','blue'),type='l',main='Real Versus Ajustada de Datos Atípicos')


dya=diff(y)###primera diferencia
dy2a=diff(diff(y))

par(mfrow=c(2,1))
plot(y)
plot(dya)
plot(dy2a)

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy2a,lag.max=48)
Pacf(dy2a,lag.max=48)
eacf(dy2a)

###################MODELO CON LA SERIE AJUSTADA POR DATOS ATIPICOS
modelo3=Arima(y,order=c(p=2,d=2,q=1),method='ML')#ARIMA(2,2,1)
resumen(modelo3)
modelo3
modelo4=Arima(y,order=c(p=0,d=2,q=2),method='ML')#MA(2)
resumen(modelo4)
modelo4

#ajuste del modelo

####3. CHEQUEO DEL MODELO
a=residuals(modelo4)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=24)
Pacf(a,lag.max=24)
## reajuste del modelo
modelo5=Arima(y,order=c(p=0,d=2,q=23),method='ML')#MA(2)
resumen(modelo5)
b=residuals(modelo5)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(b,lag.max=24)
Pacf(b,lag.max=24)
### reajuste
modelo6=Arima(y,order=c(p=0,d=2,q=22),method='ML')#MA(2)
resumen(modelo6)
c=residuals(modelo5)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(c,lag.max=24)
Pacf(c,lag.max=24)

#####4. PRONÓSTICO

p=forecast(modelo6,12)###comando para gráfica el pronóstico
plot(p,main='Pronóstico Precio Promedio Mensual Arroz blanco en Colombia, Pesos / Tonelada, 2010 - 2021 ')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
p=predict(modelo6,12)##función de pronóstico
p
###MAPE
mape=100*mean(abs(p$pred-datos[111:122,2])/datos[111:122,2])
mape

resultados=cbind(datos[111:122,2],p$pred)
colnames(resultados)=c('Real','Pronósticado')

plot(resultados,plot.type="single",col=c('red','blue'),main='Real Versus Pronósticado en Pesos / Tonelada, 2010 - 2021')
legend("bottomleft", colnames(resultados),col=c('red','blue'),lty=1,cex=.65)
