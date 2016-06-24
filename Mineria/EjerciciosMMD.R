setwd("Documentos/FESAcatlan/Diplomado/Mineria/")

library(e1071)

datos <- read.csv("Datos.csv")

testData <- data.frame(Outlook = factor(c("rain", "sunny"), levels = sort(unique((datos$Outlook)))), 
					   Temperature = c(73, 72),
					   Humidity = c(82, 65), 
					   wind = c(TRUE, TRUE))

modelo <- naiveBayes(Play ~ . , data = datos)

result <- predict(modelo, newdata = testData)
result


#library(class)

discre <- function(x, y){
	if(is.factor(x))
		return (x == y)
	else  
		(x - y)^2
} # Devuelve la diferencia entre 2 elementos in R^1

my_dist <- function(X, Y){ # X y Y son vectores
	
	diffs <- mapply(discre , X, Y)
	
	sum(diffs)
	
} # Devuelve un escalar (distancia)

normaliza <- function(x){(x - min(x)) / (max(x) - min(x))} # normaliza un vector

###########################################################

## Ejercicio KNN

datos <- read.csv("Datos2.csv")

datos

datos$a2 <- normaliza(datos$a2)
datos$a3 <- normaliza(datos$a3)

trainData <- datos[1:8, 2:3]
testData <- datos[9, 2:3]


arr_dist <- data.frame(val = -1, clase = datos$clase[1:8])

for(i in 1:nrow(trainData))
{
	arr_dist[i, "val"] <- my_dist(testData[1, ], trainData[i, ])
}

K <- 3

primerosK <- (arr_dist[order(arr_dist$val, decreasing = F), ])[1:K, ]

sum(primerosK$clase == 1) / nrow(primerosK)

# De acuerdo al agrupamiento general, la tupla nueva pertenece a la clase 1



### Agrupamiento por clase

grupo1 <- datos[datos$clase == 1 & !is.na(datos$clase), 2:3]
grupo2 <- datos[datos$clase == 0 & !is.na(datos$clase), 2:3]

K <- 3

arr_dist_gp1 <- data.frame(val = 0, clase = rep(1, nrow(grupo1))) 

for(i in 1:nrow(grupo1)){
	arr_dist_gp1[i, "val"] <- my_dist(testData[1, ], grupo1[i, ]) 
}

(arr_dist_gp1[order(arr_dist_gp1$val), ])[K, ]


arr_dist_gp2 <- data.frame(val = 0, clase = rep(0, nrow(grupo2)))


for(i in 1:nrow(grupo2)){
	arr_dist_gp2[i, "val"] <- my_dist(testData[1, ], grupo2[i, ]) 
}

(arr_dist_gp2[order(arr_dist_gp2$val), ])[K, ]



ggplot(datos, aes(x = a2, y = a3, color = factor(clase))) + geom_point(size = 3)

# Usando K = 3, obtenemos una distancia más pequeña hacia la clase "0", 
# de manera que la nueva observación es clasificada en esta clase.


## 6. Ejercicio de conglomerados

datos

datosSinEtiquetas <- datos[, 2:3]

agrupamiento <- hclust(dist(datosSinEtiquetas))
plot(agrupamiento)

nDatos <- datos

nDatos$pred_jerar <- cutree(agrupamiento, 2)
nDatos


nDatos$pred_kmeans <- kmeans(datosSinEtiquetas, centers = 2)$cluster
nDatos


library(ggplot2)

plt1 <- ggplot(data = nDatos, aes(x = a2, y = a3, color = factor(pred_kmeans))) + geom_point()
plt1

res_kmeans <- nDatos$pred_kmeans - 1
res_jerar <- nDatos$pred_jerar - 1


library(caret)

confusionMatrix(nDatos$clase, res_jerar)
confusionMatrix(nDatos$clase, res_kmeans)
