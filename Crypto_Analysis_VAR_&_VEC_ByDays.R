rm(list=ls())

library(readxl)
library(vars)
library(forecast)
library(urca)
library(TSA)
library(MASS)
library(tsoutliers)
datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Vectores Autorregresivos/Trabajo modelos VAR y VEC/Crypto Diaria/Base crypto diaria.xlsx',sheet=2)
datos=data.frame(datos)
datos=na.omit(datos)
head(datos)
nrow(datos)

####los datos de entrenamiento son de 1 a 700, 
dat=ts(datos[1:772,c(6,7,8)],start=c(2019,1),frequency=365)##variables endógenas
head(dat)
plot(ts(dat[1:772,-1]))
matplot(dat[1:772,-1],type='l',xlab='Tiempo',ylab='BITCOIN, ETHEREUM, CARDANO')

## pRUEBA DE RAICES UNITARIAS
# Prueba ADF 
btc=ts(datos[,6],start=c(2019,1),frequency=365)
eth=ts(datos[,7],start=c(2019,1),frequency=365)
ada=ts(datos[,8],start=c(2019,1),frequency=365)
ada
btc
####1.2. Diferenciación (IDENTIFICAR d)
par(mfrow=c(2,1))
plot(btc,main='Serie en nivel Bitcoin')
plot(diff(btc,1),main='Primera deiferencia del Bitcoin')
##la serie diferenciada ya es estacionaria

####Prueba en la primera diferencia Bitcoin
prueba1=ur.df(diff(btc,1), type = c("none"), lags=10)
summary(prueba1)
plot(prueba1)

##########PRUEBA A LA SERIE EN NIVEL Bitcoin 
prueba2=ur.df(btc, type = c("none"), lags=10)
summary(prueba2)
plot(prueba2)
#### La serie en nivel es ESTACIONARIA

## Dieferenciacion Ethereum 
par(mfrow=c(2,1))
plot(eth,main='Ethereum')
plot(diff(eth,1),main='Primera deiferencia del Ethereum')

##########PRUEBA A LA SERIE diferenciada Ethereum 
prueba3=ur.df(diff(eth,1), type = c("none"), lags=10)
summary(prueba3)
plot(prueba3)

##########PRUEBA A LA SERIE EN NIVEL Ethereum 
prueba4=ur.df(eth, type = c("none"), lags=10)
summary(prueba4)
plot(prueba4)
### La serie en nivel es ESTACIONARIA

## Dieferenciacion Cardano
par(mfrow=c(2,1))
plot(ada,main='Serie en nivel Cardano')
plot(diff(ada,1),main='Primera diferencia de Cardano')

####Prueba en la primera diferencia Cardano
prueba10=ur.df(diff(ada,1), type = c("none"), lags=10)
summary(prueba10)
plot(prueba10)

##########PRUEBA A LA SERIE EN NIVEL Cardano 
prueba11=ur.df(ada, type = c("none"), lags=10)
summary(prueba11)
plot(prueba11)
#### La serie es integrada de ORDEN 1

####SELCCIONAR LA LONGITUD DE REZAGO
poptimo=VARselect(datos[1:772,c(6,7,8)] ,lag.max=24,type='none')##p máximo considerado es 24
poptimo

modelo=VAR(datos[1:772,c(6,7,8)],p=6,type = "none",ic="AIC")##estima modelo var con p=14 , none 
modelo##muestra el modelo
summary(modelo)
x11()
plot(modelo)

####Reajuste del modelo con p=10, para tratar de corregir el modelo para Bitcoin 
modelo1=VAR(datos[1:772,c(6,7,8)],p=16,type = "none",ic="AIC")##
modelo1##muestra el modelo
summary(modelo1)
x11()
plot(modelo1)
### Los residuales se vuelven ruido blanco con K=16

#####Prueba estabilidad
estabilidad=stability(modelo1,type="OLS-CUSUM")
plot(estabilidad)
### Dentro de las bandas de confianza

#####PRONOSTICOS
p=predict(modelo1,n.ahead=20,ci=0.95)##10 días hacia adelante
p
plot(p)
####LOS MAPES DEL VAR(16) ORIGINAL
mbitcoin=data.frame(p$fcst[1])[,1]
mbitcoin
methereum=data.frame(p$fcst[2])[,1]
methereum
mcardano=data.frame(p$fcst[3])[,1]
mcardano
exp(mbitcoin)
exp(methereum)
exp(mcardano)
nrow(datos)
mapebitcoin=mean(abs(datos[701:761,10]-mbitcoin)/datos[701:761,10])*100
mapeethereum=mean(abs(datos[701:761,11]-methereum)/datos[701:761,8])*100
mapecardano=mean(abs(datos[701:761,14]-mcardano)/datos[701:761,10])*100
mapebitcoin
mapeethereum
mapecardano
######Pruebas de Auntocorrelación Multivariada
#?serial.test

port1=serial.test(modelo1,lags.pt=3,lags.bg=5,type="PT.asymptotic")
port1
## Preguntar

###PRUEBA DE HETEROSCEDASTICIDAD MULTIVARIADA (ARCH(q))
##?arch.test
arch=arch.test(modelo1,lags.single=5,lags.multi=3,multivariate.only=FALSE)
arch

#### Matriz A del modelo estructural 
head(datos[1:700,-1])
Amat=diag(3)###Comienza la matriz A
Amat
Amat[2,1]=NA
Amat[3,1]=Amat[3,2]=NA
Amat

####Como hay tres parametros a estimar, utilizamos tres valores iniciales
###Método directo
set.seed(124)#fija los valores iniciales
estructural=SVAR(modelo1,estmethod='scoring',start=rep(1,3),max.iter = 1000,Amat=Amat,Bmat=NULL,hessian=TRUE)
estructural

####IMPULSO RESPUESTA PARA EL MODELO ESTRUCTURAL
impulsos=irf(estructural,impulse='LNBTC',n.ahead=61,ortho=FALSE,cumulative=FALSE,boot=FALSE,seed=12345)
impulsos
x11()
plot(impulsos,main='Choques de lnBitcoin sobre las variables del sistema en el modelo estructural. 20 Periodos Adelante')

####DESCOMPOSICIÓN DE VARIANZA PARA EL MODELO ESTRUCTURAL
descomposicion=fevd(estructural,n.ahead=61)
descomposicion
x11()
plot(descomposicion,col=c('yellow','blue','pink','red','purple'))

################COINTEGRACIÓN##################################


####los datos de entrenamiento son de 1 a 94
dat1=ts(datos[1:700,c(14,11,10)],start=c(2019,1),frequency=365)##variables endógenas
head(dat1)
plot(dat1)##traza las series de tiempo, quitamos la fecha
matplot(dat1,type='l',xlab='Tiempo',ylab='Logaritmo natural precio de criptomonedas')

##################PRUEBA DE COINTEGRACIÓN
prueba=ca.jo(dat1, type = c("trace"), ecdet ="none", K =16,spec="transitory", season = NULL)
summary(prueba)
### Hay una relación Cointegrante

####IDENTIFICACIÓN TRIANGULAR DE VECTORES, ESTIMACIÓN DE VECTORES
betas=cajorls(prueba,r = 1)
class(prueba)###clases en prueba
slotNames(prueba)##objetos de prueba
betas
round(betas$beta,6)

####REGRESAR A LA FORMA VAR(16), EN NIVELES
library(vars)
var16=vec2var(prueba,r=1)
var16

res=residuals(var16)
head(res)
x11()
par(mfrow=c(2,1))
acf(res[,1],lag.max=10)
pacf(res[,1],lag.max=10)

ar(mfrow=c(2,1))
acf(res[,2],lag.max=10)
pacf(res[,2],lag.max=10)

par(mfrow=c(2,1))
acf(res[,3],lag.max=10)
pacf(res[,3],lag.max=10)

################PRONÓSTICO

p=predict(var16,n.ahead=61)
p
plot(p)

ada1=data.frame(p$fcst[1])[,1]
eth1=data.frame(p$fcst[2])[,1]
btc1=data.frame(p$fcst[3])[,1]
ada1

mapebitcoin1=mean(abs(datos[701:761,10]-btc1)/datos[701:761,10])*100
mapeethereum1=mean(abs(datos[701:761,11]-eth1)/datos[701:761,11])*100
mapecardano1=mean(abs(datos[701:761,14]-ada1)/datos[701:761,14])*100
mapebitcoin1
mapeethereum1
mapecardano1






