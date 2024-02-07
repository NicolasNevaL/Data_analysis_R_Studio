rm(list=ls())
library(readxl)
library(vars)
datos=read_excel('C:/Users/ECONOMETRÍA II/tasasdeinteres.xlsx',sheet='colombia')
datos=data.frame(datos)
head(datos)
nrow(datos)
####los datos de entrenamiento son de 1 a 955
datos=ts(datos,start=c(1,2,2015),frequency=252)##los datos quedan en series de tiempo
plot(ts(datos[1:904,-1]))##traza las series de tiempo, quitamos la fecha

matplot(datos[,2:4],type='l')

####SELCCIONAR LA LONGITUD DE REZAGO
poptimo=VARselect(datos[1:904,c(2,4)] ,lag.max=24,type='both')##p máximo considerado es 24
poptimo

intusa=data.frame(datos[1:904,5])
colnames(intusa)=c('intusa')

###?VAR ayuda para estimar modelo VAR

modelo=VAR(datos[1:904,c(2:4)],p=2,type = "both",exogen=intusa,ic="AIC")##estima modelo var con p=2
modelo##muestra el modelo
summary(modelo)
plot(modelo)


####Reajuste del modelo con p=4, para tratar de corregir el modelo para interes 12
modelo=VAR(datos[1:904,c(2:4)],p=4,type = "both",exogen=intusa,ic="AIC")##estima modelo var con p=2
modelo##muestra el modelo
summary(modelo)
plot(modelo)

#####PRONOSTICOS
intusa=data.frame(datos[905:924,5])
colnames(intusa)=c('intusa')
p=predict(modelo,n.ahead=20,ci=0.95,dumvar=intusa)##24 días hacia adelante
p
plot(p)

####LOS MAPES DEL VAR(4)ORIGINAL
i12=data.frame(p$fcst[1])[,1]
i60=data.frame(p$fcst[2])[,1]
i120=data.frame(p$fcst[3])[,1]

mape12=mean(abs(datos[905:924,2]-i12)/datos[905:924,2])*100
mape60=mean(abs(datos[905:924,3]-i60)/datos[905:924,3])*100
mape120=mean(abs(datos[905:924,4]-i120)/datos[905:924,4])*100
mape12
mape60
mape120


######Pruebas de Auntocorrelación Multivariada
#?serial.test

port1=serial.test(modelo,lags.pt=5,lags.bg=5,type="PT.asymptotic")
port1
plot(port1)

###PRUEBA DE HETEROSCEDASTICIDAD MULTIVARIADA (ARCH(q))
##?arch.test
arch=arch.test(modelo,lags.single=5,lags.multi=5,multivariate.only=FALSE)
arch

#####Prueba estabilidad
estabilidad=stability(modelo,type="OLS-CUSUM")
plot(estabilidad)

####FUNCIÓN DE IMPULSO RESPUESTA C6N LA DESCOMPOSICIÓN DE CHOLESKI
impulsos=irf(modelo,impulse='interes12',n.ahead=20,ortho=FALSE,
cumulative=FALSE,boot=FALSE,seed=12345)
impulsos
plot(impulsos,main='Choques de interes12 sobre las variables del sistema. 20 Periodos Adelante')

###DESCOMPOSICIÓN DE VARIANZA
descomposicion=fevd(modelo,n.ahead=20)
descomposicion

#####Modelo Estructural
head(datos[1:924,-c(1,5)])
Amat=diag(3)###Comienza la matriz A
Amat[2,1]=NA
Amat[3,1]=NA
Amat[3,2]=NA
Amat

####Como hay tres parametros a estimar, utilizamos tres valores iniciales
###Método directo
set.seed(124)#fija los valores iniciales
estructural=SVAR(modelo,estmethod='direct',start=c(0.7,0.1,0.8),max.iter = 1000,Amat=Amat,Bmat=NULL,hessian=TRUE)
estructural

####IMPULSO RESPUESTA PARA EL MODELO ESTRUCTURAL
impulsos=irf(estructural,impulse='interes12',n.ahead=20,ortho=FALSE,cumulative=FALSE,boot=FALSE,seed=12345)
impulsos
plot(impulsos,main='Choques de interes12 sobre las variables del sistema en el modelo estructural. 20 Periodos Adelante')

####DESCOMPOSICIÓN DE VARIANZA PARA EL MODELO ESTRUCTURAL
descomposicion=fevd(estructural,n.ahead=20)
descomposicion
plot(descomposicion,col=c('yellow','blue','pink'))


################COINTEGRACIÓN##################################


####los datos de entrenamiento son de 1 a 900
dat=ts(datos[1:900,-c(1,5)],start=c(1,2,2015),frequency=240)##variables endógenas
head(dat)
plot(dat)##traza las series de tiempo, quitamos la fecha

intusa=data.frame(datos[1:900,5])
colnames(intusa)=c('intusa')

##################PRUEBA DE COINTEGRACIÓN
prueba=ca.jo(dat[,c(3,2,1)], type = c("trace"), ecdet ="none", K =9,spec="transitory", season = NULL, dumvar = intusa)
summary(prueba)

####IDENTIFICACIÓN TRIANGULAR DE VECTORES, ESTIMACIÓN DE VECTORES
betas=cajorls(prueba,r = 2)
class(prueba)###clases en prueba
slotNames(prueba)##objetos de prueba

betas
round(betas$beta,6)


####REGRESAR A LA FORMA VAR(4), EN NIVELES
library(vars)
var9=vec2var(prueba,r=2)
var9
### PRUEBAS AL MODELO
arch.test(var9)
normality.test(var9)
autocorrelacion=serial.test(var9)###prueba de autocorrelación
plot(autocorrelacion)


####ANALISIS DE IMPULSO RESPUESTA Y DESCOMPOSICIÓN DE VARIANZA
impulso=irf(var9,boot=FALSE)
plot(impulso)
varianza=fevd(var9)
plot(varianza)
class(var9)
methods(class='vec2var')

####PRONOSTICO
intusa=matrix(datos[901:924,5],24,1)
colnames(intusa)=c('intusa')

p=predict(var9,n.ahead=24,dumvar=intusa)#PRONOSTICOS DEL VAR(4) DEL VECM(3)

plot(p)
i120=data.frame(p$fcst[1])[,1]
i60=data.frame(p$fcst[2])[,1]
i12=data.frame(p$fcst[3])[,1]

####MAPE DEL VAR(4) OBTENIDO DEL VECM(3)
mape12=mean(abs(datos[901:924,4]-i120)/datos[901:924,4])*100
mape60=mean(abs(datos[901:924,3]-i60)/datos[901:924,3])*100
mape120=mean(abs(datos[901:924,2]-i12)/datos[901:924,2])*100
mape12
mape60
mape120















