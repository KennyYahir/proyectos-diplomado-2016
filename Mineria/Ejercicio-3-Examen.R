install.packages("neuralnet")
library(neuralnet)

sustVal <- function(x, ...){ # Recibimos x como un vector y '...' como pares
	
	lista <- list(...)
	
	for(elem in lista){
		
		oldVal <- elem[1]
		newVal <- elem[2]
		
		pos <- x == oldVal
		
		x[pos] <- newVal
	}
	
	x
} # Devuelve un vector con valores sustituidos

setwd("C:/Users/Atycus/Documents/ExamenDM")

data <-read.csv("Datos2.csv", sep=",", header=T)

data<-data[1:8,]

data$a1 <- as.numeric(data$a1)
data$a1 <- normaliza(data$a1)

data$a2 <- normaliza(data$a2)
data$a3 <- normaliza(data$a3)



data
set.seed(123)
w <- matrix(runif(11,-.8,.8))

net.data <- neuralnet(clase ~ a1 + a2 + a3, data[2,], 
					  hidden = c(3), algorithm = "backprop", 
					  learningrate = 0.7, startweights = w)

net.data$call
net.data$response
net.data$covariate
net.data$err.fct
net.data$act.fct
net.data$data
net.data$net.result
net.data$weights

plot.nn(net.data, col.entry = "red", col.hidden.synapse = "orange", 
		col.intercept = "black")

#prediction(net.data)

w

net.data$weights
