install.packages("arules")
install.packages("ggplot2")
library(arules)
library(ggplot2)

#setwd("C:/Users/Atycus/Documents/ExamenDM")

temaBarras1 <- theme(panel.background = element_blank(), # Tema para la grafica
					 panel.grid.major = element_line(colour = "gray", linetype = "solid"),
					 panel.grid.major.x = element_blank(), # Es necesario para desactivar las mallas verticales
					 axis.ticks = element_blank(),
					 axis.text = element_text(size = 9),
					 #axis.text.x = element_text(angle = 32),
					 axis.title = element_text(size = 10, face = "bold"),
					 axis.line.x = element_line(color = "black"),
					 legend.text = element_text(size = 9),
					 legend.title = element_text(size = 10),
					 legend.key = element_blank(),
					 title = element_text(face = "bold", size = 10)
)

data <-read.csv("Datos.csv", sep=",", header=T)
data
str(data)

#Histogramas de Temperatura y Humedad
#qplot(data$Temperature, geom="histogram", binwidth = 0.5, main = "Histograma para Temperatura", xlab = "Temperatura", ylab = "Ocurrencias", fill=I("blue"), col=I("red"), alpha=I(.2))
pt1 <- ggplot(data = data) + geom_histogram(aes(x = Temperature), binwidth = 0.5, fill = "orange")
pt2 <- pt1 + xlab("Temperatura") + ylab("Frecuencia") + ggtitle("Histograma") + temaBarras1
pt2

#qplot(data$Humidity, geom="histogram", binwidth = 0.5, main = "Histograma para Humedad", xlab = "Humedad", ylab = "Ocurrencias", fill=I("blue"), col=I("red"), alpha=I(.2))
pt1 <- ggplot(data = data) + geom_histogram(aes(x = Humidity), binwidth = 0.5, fill = "orange")
pt2 <- pt1 + xlab("Temperatura") + ylab("Frecuencia") + gtitle("Histograma") + temaBarras1
pt2

#Categorizar Temperature
x<-cut(data$Temperature, 4, include.lowest=TRUE)
x
cut(data$Temperature, 4, include.lowest=TRUE, labels=c("Cold", "Cool", "Warm", "Hot"))

data_bin<-data

data_bin$Temperature<-cut(data$Temperature, 4, include.lowest=TRUE, labels=c("Cold", "Cool", "Warm", "Hot"))
data_bin

#qplot(data_bin$Temperature)

pt1 <- ggplot(data = data_bin) + geom_bar(aes(x = Temperature), fill = "lightseagreen")
pt2 <- pt1 + xlab("Temperatura") + ylab("Frecuencia") + temaBarras1
pt2

str(data_bin)

#Categorizar Humidity
y<-cut(data$Humidity, 3, include.lowest=TRUE)
y
cut(data$Humidity, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))

data_bin$Humidity<-cut(data_bin$Humidity, 3, include.lowest=TRUE, labels=c("Low", "Med", "High"))
data_bin
str(data_bin)
#qplot(data_bin$Humidity)

pt1 <- ggplot(data = data_bin) + geom_bar(aes(x = Humidity), fill = "purple")
pt2 <- pt1 + xlab("Humidity") + ylab("Frecuencia") + temaBarras1
pt2

data_bin$Wind<-as.factor(data_bin$Wind)

data_bin
rules <- apriori(data_bin, parameter = list(supp=0.25, conf=0.9), 
                 appearance = list(rhs=c("Play=no", "Play=yes"), default="lhs"))
inspect(rules)
summary(rules)
