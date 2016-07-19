install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

setwd("C:/Users/Atycus/Documents/ExamenDM")
data_bin

summary(data_bin)
ftable(data_bin, row.vars=NULL, col.vars="Play")

modelo<-rpart(Play ~ ., data_bin, control = rpart.control(minsplit = 1), 
			  parms = list(split = "gini"))
#modelo

rpart.plot(modelo)

data_pred <-read.csv("tupla_pred.csv", sep=",", header=T)
data_pred

data_pred$Wind <- factor(data_pred$Wind, levels = unique(data_bin$Wind))

#predict(modelo, data_pred)
p <- predict(modelo, data_pred, type="class")
p
