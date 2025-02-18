library(survival)
library(dplyr)
library(ggplot2)

datos<-read.csv("BaseDatos20.csv")
head(datos)
attach(datos)

datos.surv<-Surv(y, failed)

summary(datos)

##### An�lisis exploratorio ####

#Tenemos las covariables
#Edad (factor de 4 valores) + 1 = 5
#Sexo (factor de 2 valores)
#Fumar (factor de 2 valores)
#Producto (factor 3 valores) + 1 = 4

###NOTA: El tiempo esta en meses desde la emisi�n de la p�liza hasta la muerte

######Edad#####

edad.km<-survfit(datos.surv~Edad66.70+Edad71.75+Edad76.80+EdadMas.de.81, 
                 type="kaplan-meier", data=datos, conf.type="plain")
edad.km
plot(edad.km, conf.int=F, xlab="Tiempo en meses", col=1:5,lab=c(10,10,7), 
     cex=2, lty=1:5)
legend(locator(1), legend=c("Otros", "Mas de 81", "76-80", "71-75", "66-70"), 
       lty=1:5, col=1:5)


######Sexo#####

gender.km<-survfit(datos.surv~SexoMujer, type="kaplan-meier", data=datos, 
                   conf.type="plain")
gender.km #Podemos ver que gr�fica pertenece a que categor�a y sacar algunas conclusiones
plot(gender.km, conf.int=F, xlab="Tiempo en meses", col=1:2,lab=c(10,10,7), 
     cex=2, lty=1:2)
legend(locator(1), legend=c("Hombre", "Mujer"), lty=1:2, col=1:2)

######Fumar#####

fumar.km<-survfit(datos.surv~FumarSi, type="kaplan-meier", data=datos, 
                   conf.type="plain")
fumar.km
plot(fumar.km, conf.int=F, xlab="Tiempo en meses", col=1:2,lab=c(10,10,7), 
     cex=2, lty=1:2)
legend(locator(1), legend=c("No Fumador", "Fumador"), lty=1:2, col=1:2)

#Como era de esperarse la mediana del tiempo de supervivencia para la poblaci�n 
#no fumadora, es superior a la de la poblaci�n fumadora
#Podemos mencionar alguna conclusi�n del siguiente:
#Si S1 es para la poblaci�n no fumadora y S2 para la fumadora
#S1(t)>S2(t) para toda t
#Dado un tiempo t, la probabilidad de supervivencia para la poblaci�n no fumadora es 
#mayor que la probabilidad de supervivencia para la fumadora

######Producto#####

#traducci�n
#term es el temporal
#universal life y whole life son un tipo de ordinarios

producto.km<-survfit(datos.surv~ProductoTERM+ProductoU.L.+ProductoWHOLE.LIFE, type="kaplan-meier", data=datos, 
                  conf.type="plain")
producto.km
plot(producto.km, conf.int=F, xlab="Tiempo en meses", col=1:4,lab=c(10,10,7), 
     cex=2, lty=1:4)
legend(locator(1), legend=c("Otros", "Whole Life", "Universal Life", "Term"), lty=1:4, col=1:4)

##### Modelo de Cox #####

###### Paso 1 #####
#Estableceremos modelos simples con cada covariable para encontrar cuales son significativas
#Para aquellas con m�s de 2 factores intentaremos encontrar las combinaciones que tengan mas componentes
#significativos

#Edad
#A�adiremos la variable dummy faltante para poder ir modificando el riesgo base
datos$EdadOtros<-as.numeric((datos$Edad66.70==0)&(datos$Edad71.75==0)&(datos$Edad76.80==0)&(datos$EdadMas.de.81==0))

edad.ph<-coxph(Surv(y, failed)~Edad66.70+Edad71.75+Edad76.80+EdadMas.de.81, 
                 datos, method="breslow", na.action=na.exclude)
edad.ph #Edad76.80 y edad 71.75 son significativos (2)

edad.ph<-coxph(Surv(y, failed)~Edad66.70+Edad71.75+Edad76.80+EdadOtros, 
               datos, method="breslow", na.action=na.exclude)
edad.ph #Edad76.80 y edad 71.75 son significativos (2)

edad.ph<-coxph(Surv(y, failed)~Edad66.70+Edad71.75+EdadOtros+EdadMas.de.81, 
               datos, method="breslow", na.action=na.exclude)
edad.ph #Edad71.75, EdadOtros y EdadMas.de.81 son significativos (3)

edad.ph<-coxph(Surv(y, failed)~Edad66.70+EdadOtros+Edad76.80+EdadMas.de.81, 
               datos, method="breslow", na.action=na.exclude)
edad.ph #Todos son significativos (4)*

edad.ph<-coxph(Surv(y, failed)~EdadOtros+Edad71.75+Edad76.80+EdadMas.de.81, 
               datos, method="breslow", na.action=na.exclude)
edad.ph #S�lo Edad71.75 es significativa (1)

#Por lo tanto la combinaci�n de variables que escogeremos ser�
#Edad66.70+EdadOtros+Edad76.80+EdadMas.de.81
#Riesgo base ser� Edad71.75

#Sexo
sexo.ph<-coxph(Surv(y, failed)~SexoMujer, 
               datos, method="breslow", na.action=na.exclude)
sexo.ph
#S� es significativa

#Fumar
fumar.ph<-coxph(Surv(y, failed)~FumarSi, 
               datos, method="breslow", na.action=na.exclude)
fumar.ph
#S� es significativa


#Producto

#Nuevamente primero a�adimos la dummy faltante para poder modificar el riesgo base

datos$ProductoOtros<-as.numeric((datos$ProductoTERM==0)&(datos$ProductoU.L.==0)&(datos$ProductoWHOLE.LIFE==0))

producto.ph<-coxph(Surv(y, failed)~ProductoTERM+ProductoU.L.+ProductoWHOLE.LIFE, 
               datos, method="breslow", na.action=na.exclude)
producto.ph #Todas son significativas, nos quedaremos con esta combinaci�n


###### Paso 2 #####
#Corremos el modelo con las que fueron significativas, en este caso fueron todas

modelo1.ph<-coxph(Surv(y, failed)~Edad66.70+EdadOtros+Edad76.80+EdadMas.de.81
                  +ProductoTERM+ProductoU.L.+ProductoWHOLE.LIFE
                  +SexoMujer
                  +FumarSi, 
               datos, method="breslow", na.action=na.exclude)

modelo1.ph
#Todas las variables fueron significativas a�n en presencia de otras
#as� que no se elimina ninguna

###### Paso 3 #####

#Como no eliminamos covariables en el paso 2 entonces omitimos este paso
#Omitiremos el uso de interacciones con el fin de mantener simple el modelo

###### Paso 4 #####
#Ya hab�amos verificado que todas sean significativas

##### Modelo final #####

#Por lo anterior nuestro modelo final es el siguiente

modelofinal.ph<-coxph(Surv(y, failed)~Edad66.70+EdadOtros+Edad76.80+EdadMas.de.81
                  +ProductoTERM+ProductoU.L.+ProductoWHOLE.LIFE
                  +SexoMujer
                  +FumarSi, 
                  datos, method="breslow", na.action=na.exclude)

modelofinal.ph

##### Validaci�n ####

###### Riesgos proporcionales ####
#Nos arroja las p de las pruebas individuales y la global
cox.modelo.ph<-cox.zph(modelofinal.ph)
cox.modelo.ph

#Todos los p valores individuales son mayores a 0.05 
#Incluso el p-valor global es 0.97, cercano a 1
#Es decir, nuestro modelo presenta riesgos proporcionales

par(mfrow=c(3,3))
plot(cox.modelo.ph)

#Para observar mejor las gr�ficas las observaremos de 4 en 4
par(mfrow=c(2,2))
plot(cox.modelo.ph[1:4])
plot(cox.modelo.ph[5:8])
par(mfrow=c(1,1))
plot(cox.modelo.ph[9])


###### Linealidad #####
#Se omite, no tenemos covariables continuas (variables)


###### Datos influyentes #####
dfbeta.modelo<-residuals(modelofinal.ph, type='dfbeta')

par(mfrow=c(3,3))
for (j in 1:9){
        plot(dfbeta.modelo[,j], ylab=names(coef(modelofinal.ph))[j])
        abline(h=0, lty=2)
}

#Para observarlos mejor
par(mfrow=c(2,2))
for (j in 1:4){
        plot(dfbeta.modelo[,j], ylab=names(coef(modelofinal.ph))[j])
        abline(h=0, lty=2)
}
for (j in 5:8){
        plot(dfbeta.modelo[,j], ylab=names(coef(modelofinal.ph))[j])
        abline(h=0, lty=2)
}
par(mfrow=c(1,1))
plot(dfbeta.modelo[,9], ylab=names(coef(modelofinal.ph))[9])
abline(h=0, lty=2)


#Los datos parecen comportarse de manera estable y no parece haber datos
#que puedan arruinar el modelo significativamente

#No podemos concluir de outliers, pero podemos sospechar de ellos
#Ya para mayor detalles habr�a que hablar con el experto



