install.packages("neuralnet")
library(neuralnet)

setwd("C:/Users/Atycus/Documents/ExamenDM")
data <-read.csv("Datos2.csv", sep=",", header=T)
data
data<-data[1:8,]
str(data)
summary(data)

data$a2<-(data$a2 - min(data$a2)) / (max(data$a2) - min(data$a2))
data
set.seed(53)
w<-matrix(runif(13,-.8,.8))

net.data <- neuralnet(clase ~ a1_pos + a1_neg + a2 + a3, data[2,], hidden = c(2), algorithm = "backprop", learningrate = .9, startweights = w)

net.data$call
net.data$response
net.data$covariate
net.data$err.fct
net.data$act.fct
net.data$data
net.data$net.result
net.data$weights

plot.nn(net.data, col.entry="blue", col.hidden.synapse = "red", col.intercept = "green")

#prediction(net.data)

w

net.data$weights
