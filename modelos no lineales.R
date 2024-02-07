###Ejemplo 5
n=200##200 valores
x=seq(1,n,1)##valores de x de 1 a 200
set.seed(467382)##fija la simulación
e=rnorm(n,0,1)##200 valores de una normal(0,1)
y=x^1.5+e##genera los valores y
plot(x,y)
b=seq(1,40,1)###crea la secuencia de b, para las 40 iteraciones de Gauss-Newton
b[1]=2##primer valor inicial
for(i in 2:n){b[i]=b[i-1]+sum((y-x^b[i-1])*x^b[i-1]*log(x))/sum((x^b[i-1]*log(x))^2)
}
b
plot(b)
datos=data.frame(cbind(x,y))
head(datos)
modelo=nls(y~x^b,start=c(b=2),algorithm='default',control=nls.control(maxiter=50),data=datos)
summary(modelo)


###Ejemplo 6
n=100##200 valores
x1=seq(0.01,1,0.01)##valores de x de 1 a 200
x2=log(x1)
set.seed(467382)##fija la simulación
e=rnorm(n,0,1)##200 valores de una normal(0,1)
y=exp(2.5*x1+3*x2)+e##genera los valores y
b=array(0,dim=c(2,1,50))
b[,,1]=matrix(c(10,20),nrow=2,ncol=1)##primer valor inicial
for(i in 2:50){b[,,i]=b[,,i-1]+
solve(matrix(c(sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1^2),sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1*x2),
sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1*x2),sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x2^2)),nrow=2,ncol=2))%*%+##el +  indica que sigue la linea misma linea abajo
matrix(c(sum((exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))*x1*(y-exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))),
sum((exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))*x2*(y-exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2)))),2,1)}
b[1,,]
b[2,,]
par(mfrow=c(2,1))
plot(b[1,,],type='l',main='Trayectoria de B_1',ylab='B_1',xlab='Iteración')
plot(b[2,,],type='l',main='Trayectoria de B_2',ylab='B_2',xlab='Iteración')



###Ejemplo 6, cambio de valores iniciales
n=100##200 valores
x1=seq(0.01,1,0.01)##valores de x de 1 a 200
x2=log(x1)
set.seed(467382)##fija la simulación
e=rnorm(n,0,1)##200 valores de una normal(0,1)
y=exp(2.5*x1+3*x2)+e##genera los valores y
b=array(0,dim=c(2,1,50))
b[,,1]=matrix(c(5,5),nrow=2,ncol=1)##primer valor inicial
for(i in 2:50){b[,,i]=b[,,i-1]+
solve(matrix(c(sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1^2),sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1*x2),
sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x1*x2),sum(exp(2*b[1,1,i-1]*x1+2*b[2,1,i-1]*x2)*x2^2)),nrow=2,ncol=2))%*%+##el +  indica que sigue la linea misma linea abajo
matrix(c(sum((exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))*x1*(y-exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))),
sum((exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2))*x2*(y-exp(b[1,1,i-1]*x1+b[2,1,i-1]*x2)))),2,1)}
b[1,,]
b[2,,]
par(mfrow=c(2,1))
plot(b[1,,],type='l',main='Trayectoria de B_1',ylab='B_1',xlab='Iteración')
plot(b[2,,],type='l',main='Trayectoria de B_2',ylab='B_2',xlab='Iteración')


###ESTIMACIÓN EN R UTILIZANDO LA FUNCIÓN nls()
datos=data.frame(x1,x2,y)
head(datos)
modelo=nls(y~exp(b1*x1+b2*x2),data=datos,                             ##default=Gauss-Newton
start=c(b1=5,b2=5),control=nls.control(maxiter=50),algorithm = "default")
summary(modelo)
b[1,,]
b[2,,]

                 



