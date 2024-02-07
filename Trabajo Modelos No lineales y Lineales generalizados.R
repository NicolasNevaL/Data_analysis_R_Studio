####### Mdeolos no lineales
###Ejemplo 5
n=200##200 valores
x=seq(1,n,1)##valores de x de 1 a 200
set.seed(467382)##fija la simulación
e=rnorm(n,0,1)##200 valores de una normal(0,1)
y=log(2*x)+e ##genera los valores y
plot(x,y)
b=seq(1,40,1)###crea la secuencia de b, para las 40 iteraciones de Gauss-Newton
b[1]=0.1##primer valor inicial
for(i in 2:n){b[i]=b[i-1]+sum((y-log(b[i-1]*x))*(x/(b[i-1]*x)))/sum((x/(b[i-1]*x))^2)
}
b
plot(b)
datos=data.frame(cbind(x,y))
head(datos)
modelo=nls(y~log(b*x),start=c(b=1),algorithm='default',control=nls.control(maxiter=50),data=datos)
summary(modelo)
