rm(list=ls())
library(readxl)
datos <- read_excel("Resumen de carpetas/Academia/Economia/Noveno semestre/Econometria II/Modelos lineales Generalizados/Trabajo MLG/healthstroke.xlsx", 
                           col_types = c("numeric", "text", "numeric", 
                                         "numeric", "numeric", "text", "text", 
                                         "text", "numeric", "numeric", "text", 
                                         "numeric"))
View(datos)
head(datos)
table(datos$stroke)

exitos=subset(datos,stroke==1)##separo éxitos
head(exitos)
fallas=subset(datos,stroke==0)##separo fallas
head(fallas)

###MUESTRA ALEATORIA
r1=seq(1,nrow(exitos),1)
r2=seq(1,nrow(fallas),1)

set.seed(10)##semilla
s1=sample(r1,size=0.1*nrow(exitos))##muestra del 10% de los positivos
set.seed(10)
s2=sample(r2,size=0.1*nrow(fallas))###muestra del 10% de los negativos

entrenamiento=data.frame(rbind(exitos[-s1,],fallas[-s2,]))####datos para estimar el modelo
prueba=data.frame(rbind(exitos[s1,],fallas[s2,]))###datos para evaluar el modelo

####Regresión Logística hay dos formas de estimar el modelo

###primer forma, con todas las variables
modelo=glm(stroke~.,data=entrenamiento,family=binomial(link='logit'))###regresión logística
summary(modelo)
 
round(coef(summary(modelo)),2)
head(prueba)
table(prueba$stroke)

####Segunda forma con las variables que yo desee
modelo=glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type +
             Residence_type + avg_glucose_level+bmi+smoking_status,data=entrenamiento,family=binomial(link='logit'))

summary(modelo)
round(coef(summary(modelo)),2)

#####EVALUACIÓN DEL MODELO
head(prueba)
table(prueba$stroke)
prueba[1,]
p1=predict(modelo,newdata=prueba[1,],type='response')##primer individuo de prueba
p1
p1=predict(modelo,newdata=prueba,type='response')####todos los individuos de la muestra de prueba
p1
p=round(p1,0)###redondeadas a cero
p
table(prueba[,12])
table(prueba[,12],p)##matriz de confusión
p=ifelse(p1>=0.3,1,0)
table(prueba[,12],p)

####PRUEBA DE BONDAD DE AJUSTE DE HOSMER Y LEMESHOW g>número de covariables+1
library(generalhoslem)
logitgof(entrenamiento$stroke, fitted(modelo),g=4)###en los datos de entrenamiento
logitgof(prueba$stroke, p1,g=4)###en los datos de prueba
####Error in quantile.default(yhat, probs = seq(0, 1, 1/g)) : 
####missing values and NaN's not allowed if 'na.rm' is FALSE


library(ROCR)
####Modelo Probit
modelo1=glm(stroke~.,entrenamiento,family=binomial(link='probit'))

###otra forma de estimar el modelo probit
modelo1=glm(stroke ~ gender + age + hypertension + heart_disease + ever_married + work_type +
              Residence_type + avg_glucose_level+bmi+smoking_status,data=entrenamiento,family=binomial(link='probit'))

summary(modelo1)

p1=predict(modelo1,newdata=prueba,type='response')
p1
p=round(p1,0)
p
table(prueba[,12])
table(prueba[,12],p)##matriz de confusión

p=ifelse(p1>=0.4,1,0)##cambio del punto de corte
table(prueba[,12],p)##matriz de confusión

#REGRESIÓN PROBIT
curva1=prediction(data.frame(p1),prueba$stroke)
rl1=performance(curva1,measure="auc",x.measure ="cutoff")
rl2=performance(curva1, "tpr","fpr")
plot(rl2,colorize=T,main=paste("AUC:",(rl1@y.values)))

