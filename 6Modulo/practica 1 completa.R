dat1 <- read.csv("/home/fou/Desktop/diplomado/Multivariado2/practica1/eleccciodeoptativas2016grupo2.xlsx - Base completa de eleccion.csv" )
dat2 <- read.csv("/home/fou/Desktop/diplomado/Multivariado2/practica1/eleccciodeoptativas2016grupo2.xlsx - resumen.csv", header = F)[, 1:2]

library(reshape2)

head(dat1)
head(dat2)
colnames(dat2) <- c("materia", "clave")
str(dat1)


dat2 <- na.omit(dat2)
trainData.pre <- dat1[, c(1, 3:4)]

head(trainData.pre)
names(airquality) <- tolower(names(airquality))
melt(airquality, id=c("month", "day"))
names(ChickWeight) <- tolower(names(ChickWeight))
melt(ChickWeight, id=2:4)


head(x<-melt(trainData.pre, id.vars = c("Individuo","Clave")))
head(encoding <- acast(x,Individuo  ~ Clave))
index <- is.na(encoding)
encoding[index]<-0


set.seed(0)
a<-c()
for (i in 2:10)
{
    a[i] <- tryCatch(sum(kmeans(encoding, centers = i)$withinss),warnings="puto", error="aqui")
}
plot(a)
lines(a)


