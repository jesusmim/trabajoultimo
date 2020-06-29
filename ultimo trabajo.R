rm(list=ls())
library(dplyr)
library(ggplot2)
library(datasets)
class(iris)
View(iris)
help("iris")
colnames(iris)
Irissetosasl<-filter(select(iris,"Sepal.Length","Species"),Species=="setosa")
summary(Irissetosasl)
Irissetosasw<-filter(select(iris,"Sepal.Width","Species"),Species=="setosa")
datasetalsetosa<-data.frame(Irissetosasl$Sepal.Length,Irissetosasw$Sepal.Width)

plot(datasetalsetosa)
boxplot(datasetalsetosa)
cor(datasetalsetosa)

Irissetosapl<-filter(select(iris,"Petal.Length","Species"),Species=="setosa")
Irissetosapw<-filter(select(iris,"Petal.Width","Species"),Species=="setosa")
datasetalsetosapp<-data.frame(Irissetosapl$Petal.Length,Irissetosapw$Petal.Width)
plot(datasetalsetosapp)
boxplot(datasetalsetosapp)
cor(datasetalsetosapp)

Irisversicolorsl<-filter(select(iris,"Sepal.Length","Species"),Species=="versicolor")
Irisversicolorsw<-filter(select(iris,"Sepal.Width","Species"),Species=="versicolor")
datasetalversicolor<-data.frame(Irisversicolorsl$Sepal.Length,Irisversicolorsw$Sepal.Width)
plot(datasetalversicolor)
boxplot(datasetalversicolor)
cor(datasetalversicolor)

Irisversicolorpl<-filter(select(iris,"Petal.Length","Species"),Species=="versicolor")
Irisversicolorpw<-filter(select(iris,"Petal.Width","Species"),Species=="versicolor")
dataversicolorpetal<-data.frame(Irisversicolorpl$Petal.Length,Irisversicolorpw$Petal.Width)
plot(dataversicolorpetal)
boxplot(dataversicolorpetal)
cor(dataversicolorpetal)

datapetall<-select(iris,"Petal.Length","Petal.Width","Species")
ggplot(datapetall,aes(x=Petal.Length,y=Petal.Width,colour=Species))+geom_point()+ggtitle("Petal Length vs Petal Width")
boxplot(select(datapetall,"Petal.Length","Petal.Width"))

datasepall<-select(iris,"Sepal.Length","Sepal.Width","Species")
ggplot(datasepall,aes(x=Sepal.Length,y=Sepal.Width,colour=Species))+geom_point()+ggtitle("Sepal Length vs Sepal Width")
boxplot(select(datasepall,"Sepal.Length","Sepal.Width"))

library(MPV)
library(help = MPV)

data(iris)
datos <-iris
datos <- na.omit(datos)

str(datos)
cor(datos[,-5])
library(corrplot)
corrplot(cor(datos[,-5]))
pairs(datos[,-5])


library(rpart)
library(help = rpart)


mod1 <- rpart(Species ~ ., data = datos)
class(mod1)
typeof(mod1)
mod1
mod1$splits

library(rpart.plot)
prp(mod1)
NuevaEspecie <- data.frame(
  Sepal.Length = 6.5, Sepal.Width = 3.0,
  Petal.Length = 5.2, Petal.Width = 2.0
)


predict(object=mod1,newdata=NuevaEspecie)



library(mlbench)
data(PimaIndiansDiabetes2)
datos2<-PimaIndiansDiabetes2[,-7]
datos2.1<-PimaIndiansDiabetes2[,-2]
mod11 <- rpart(diabetes ~ ., data=datos2, method='class')

nuevadata1<-data.frame(pregnant=7,glucose=90,pressure=70,triceps=23,insulin=92,age=30,mass=25)
predict(mod11,newdata = nuevadata1)

mod111 <- rpart(diabetes ~ ., data=datos2.1, method='class')
nuevadata2<-data.frame(pregnant=7,pedigree=0.6,pressure=70,triceps=23,insulin=92,age=30,mass=25)
predict(mod111,newdata = nuevadata2)


library(MASS)
library(primes)
data("Boston")

Prueba1 <-c(1 ,3, 9,12, 14,15,  32,  36,  45 , 59 , 66  ,94 , 95 ,130 ,146, 149 ,171, 188 ,193, 194, 209,
            210 ,218 ,227, 237 ,241, 255, 277,304 ,308 ,316 ,320, 334 ,349,366 ,367 ,371 ,378, 393 ,401 ,
            406 ,422, 423 ,453 ,455 ,485, 496, 505)
Prueba2 <- sample(primes::generate_primes(min=200,max = 506),size = 25)
Escenario1 <- Boston[Prueba1,-14]
Escenario2 <- Boston[Prueba2,-14]

datos22 <-Boston
datos22 <- na.omit(datos22)

str(datos22)
cor(datos22)
library(corrplot)
corrplot(cor(datos22))
pairs(datos22)



mod1111<- rpart(age ~ ., data = datos22[,-14])
class(mod1111)
typeof(mod1111)

prediccion1<-predict(mod1111,newdata = Escenario1)

prediccion2<-predict(mod1111,newdata = Escenario2)

hist(prediccion1)
hist(prediccion2)

test_mse<???mean((prediccion1???1)^2)
     