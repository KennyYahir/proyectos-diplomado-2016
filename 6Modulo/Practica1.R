
dat1 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - Base completa de eleccion.csv", 
				 dec = ",")

library(reshape2)


dat1 <- na.omit(dat1)
head(dat1)

trainData.pre <- dat1[, c(1, 3:4)]

trainData.melt <- melt(trainData.pre, id.vars = c("Individuo","Clave"))
trainData <- acast(trainData.melt, Individuo ~ Clave)

trainData[is.na(trainData)] <- 0
head(trainData)


trainData[is.na(trainData)] <- 0


