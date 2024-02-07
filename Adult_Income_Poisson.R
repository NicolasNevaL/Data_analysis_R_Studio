##### Regresion Poisson
rm(list=ls())
library(readxl)
datos=read_excel('C:/Users/Nicodreamer/Documents/Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Modelos lineales Generalizados/Trabajo MLG/Adult_income.xlsx',sheet=2)
datos=data.frame(datos[,-6])
head(datos)

table(datos$Clm_Count)
r=seq(1,nrow(datos),1)
s=sample(r,size=0.1*nrow(datos))
prueba=datos[s,]
entrenamiento=datos[-s,]
head(entrenamiento)

#############MODELO ESTIMADO
modelo=glm(salary~workclass+fnlwgt+education+education.num+occupation+relationship+race+sex+hours.per.week+native.country,data=entrenamiento,family=poisson(link='log'))
summary(modelo)
round(coef(summary(modelo)),2)

efectos=exp(modelo$coef)
efectos=data.frame(as.matrix(efectos,ncol=1))
efectos=round(efectos,2)
colnames(efectos)=c('efectos')
efectos

p1=predict(modelo,newdata=prueba,type='response')
round(p1,0)
table(round(p1,0))
