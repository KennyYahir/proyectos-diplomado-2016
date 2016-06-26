install.packages("rpart.plot")
library(rpart)
library(rpart.plot)

setwd("C:/Users/Atycus/Documents/ExamenDM")
data_bin

summary(data_bin)
ftable(data_bin, row.vars=NULL, col.vars="Play")

data_tree<-rpart(Play ~ ., data_bin, control=rpart.control(minsplit=1), parms = list(split = "gini"))
data_tree

rpart.plot(data_tree)

data_pred <-read.csv("Datos_pred.csv", sep=",", header=T)
str(data_pred)
data_pred$Wind<-as.factor(data_pred$Wind)

predict(data_tree, data_pred)
p <- predict(data_tree, data_pred, type="class")
p
