# practica-16
install.packages("fpp")
require(fpp)

desocupación

tdes<- ts(desocupación, frequency = 4, start=2005)
tdes
#des<- as.numeric(ts(data.frame(desocupación), frequency = 4, start=2005))

mod1<-ses(tdes, alpha=.1, initial="simple", h=8)
mod2<-ses(tdes, alpha=.3, initial="simple", h=8)
mod3<-ses(tdes, alpha=.9, initial="simple", h=8)
mod4<-ses(tdes, alpha=.95, initial="simple", h=8)
plot(mod1, ylab = "Año", main = "Desocupación", type="o")
lines(fitted(mod1), col="brown", type="o")
lines(fitted(mod2), col="red", type="o")
lines(fitted(mod3), col="green", type="o")
lines(fitted(mod4), col="blue", type="o")

###mae y rmse para el modelo 1
aa<- des- mod1$fitted ##valores ajustados
aa   ##residuos

#analizamos los residules de manera manual
mean(abs(aa))##MAE
sqrt(mean(aa^2))#RMSE
#analizamos con accuracy
evalm1<-accuracy(mod1)
evalm1

###mae y rmse para el modelo 2
aa2<- des- mod2$fitted ##
aa2
mean(abs(aa2))##MAE
sqrt(mean(aa2^2))#RMSE

evalm2<-accuracy(mod2)
evalm2

###mae y rmse para el modelo 3
aa3<- des- mod3$fitted ##
aa3
mean(abs(aa3))##MAE
sqrt(mean(aa3^2))#RMSE

evalm3<-accuracy(mod3)
evalm3

###mae y rmse para el modelo 4
aa4<- des- mod4$fitted ##
aa4
mean(abs(aa4))##MAE
sqrt(mean(aa4^2))#RMSE

evalm4<-accuracy(mod4)
evalm4

##eligiriamos el modelo 3 por q graficamentes es el mas cercano 
##el mae es la medida de residuos, y es el mas cercano a 0
