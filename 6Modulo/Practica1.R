
dat1 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - Base completa de eleccion.csv", 
				 dec = ",")
dat2 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - resumen.csv", header = F)[, 1:2]

library(reshape2)

head(dat1)
head(dat2)
colnames(dat2) <- c("materia", "clave")

dat1 <- na.omit(dat1)
dat2 <- na.omit(dat2)
trainData.pre <- dat1[, c(1, 3:4)]

head(x<-melt(trainData.pre, id.vars = c("Individuo","Clave")))
head(encoding <- acast(x,Individuo  ~ Clave))
index <- is.na(encoding)
encoding[index] <- 0

head(encoding)


set.seed(0)
varIn <- c()
for (i in 1:30)
{
    varIn[i] <- tryCatch(sum(kmeans(encoding, centers = i)$withinss),
                         warnings="warning", error="aqui")  #Por si algun modelo falla
}

##
##





