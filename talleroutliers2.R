rm(list=ls())

source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Procesos estacionarios/R/eacf.R') 
source('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Procesos estacionarios/R/resumen.R')


library(forecast)
library(readxl)
library(MASS)
library(tsoutliers)

datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Series estacionales/datoscruceros.xlsx',sheet=1)
head(datos)
datos=data.frame(datos)
head(datos)
nrow(datos)#número de datos en la serie
datos1=datos[1:78,2]
cruceros=ts(datos1,start=c(2013,1),frequency=12)
cruceros
plot(cruceros,main="número de cruceros mensuales")
##no hay que estabilizar varianza
##la serie tiene componente estacional pero no es creciente ni decreciente


###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(cruceros,lag.max=24)
Pacf(cruceros,lag.max=24)
eacf(cruceros)

###2. ESTIMACIÓN DE LOS POSIBLES MODELOS

modelo1=Arima(cruceros,order=c(p=4,d=0,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#ARMA(2,3)
modelo1
z1=modelo1$coef/sqrt(diag(vcov(modelo1)))##valores z de los coeficientes del modelo estimado
abs(z1)
resumen(modelo1)


y.##QUITAMOS COMPONENTES NO SIGNIFICATIVOS
modelo2=Arima(cruceros,order=c(p=1,d=0,q=1),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#ARMA(2,3)
modelo2
z3=modelo2$coef/sqrt(diag(vcov(modelo2)))##valores z de los coeficientes del modelo estimado
abs(z3)
resumen(modelo2)


###############DETECCIÓN DE OUTLIERS
atipicos=tso(cruceros,types = c("IO","AO", "LS", "TC", "SLS"),
discard.method="bottom-up", maxit.iloop=10,maxit.oloop=10,tsmethod = "arima", 
args.tsmethod = list(order = c(p=0,d=1,q=1),seasonal=list(order=c(P=0,D=1,Q=1))))
atipicos
plot(atipicos)

####################SERIE CORREGIDA, SIN DATOS ATIPICOS
y=atipicos$yadj###SERIE CORREGIDA, AJUSTADA
matplot(cbind(log(cafe),y),col=c('red','blue'),type='l',main='Real Versus Ajustada de Datos Atípicos')


dy=diff(diff(y,12),1)

par(mfrow=c(2,1))
plot(y)
plot(dy)

###1.3. Aplicamos ACF, PACF y EACF 
par(mfrow=c(2,1))
Acf(dy,lag.max=48)
Pacf(dy,lag.max=48)
eacf(dy)

###################MODELO CON LA SERIE AJUSTADA POR DATOS ATIPICOS
modelo3=Arima(y,order=c(p=2,d=0,q=0),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo3
resumen(modelo3)


####3. CHEQUEO DEL MODELO
a=residuals(modelo3)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=25)
Pacf(a,lag.max=25)

##REAJUSTE DEL MODELO

modelo=Arima(y,order=c(p=2,d=0,q=4),seasonal=list(order=c(P=0,D=1,Q=1),seasonal=12),method='ML')#MA(1)
modelo
resumen(modelo)

####3. CHEQUEO DEL MODELO
a=residuals(modelo)##obtenmos residuales
par(mfrow=c(2,1))#dos gráficos en una ventana
Acf(a,lag.max=25)
Pacf(a,lag.max=25)

#####4. PRONÓSTICO

p=forecast(modelo,9)###comando para gráfica el pronóstico
plot(p,main='Pronóstico de las ventas')

####PARA SACAR LA ESCALA DE PRONÓSTICO ORIGINAL
p=predict(modelo,9)
p$pred
###MAPE
datos2=datos[79:87,2]
mape=100*mean(abs(p$pred-datos2)/datos2)
mape

resultados=cbind(datos2,p$pred)
colnames(resultados)=c('Real','Pronósticado')

plot(resultados,plot.type="single",col=c('red','blue'),main='Real Versus Pronósticado')
legend("bottomleft", colnames(resultados),col=c('red','blue'),lty=1,cex=.65)







