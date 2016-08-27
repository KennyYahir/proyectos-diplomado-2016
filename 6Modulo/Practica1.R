
dat1 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - Base completa de eleccion.csv", 
				 dec = ",")
#dat2 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - resumen.csv", header = F)[, 1:2]



library(reshape2)

head(dat1)
dat1

trainData.pre <- dat1[, c(1, 3:4)]

trainData.melt <- melt(trainData.pre, id.vars = c("Individuo","Clave"))
trainData <- acast(trainData.melt, Individuo ~ Clave)

head(trainData)

trainData[is.na(trainData)] <- 0
