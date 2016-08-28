############################################################
# Practica 1
#
#
#
#
############################################################


############################################################
## Preparacion de datos
dat1 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - Base completa de eleccion.csv", 
				 dec = ",")

dat2 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - resumen.csv", header = F)[, 1:2]

library(reshape2)


dat1 <- na.omit(dat1)
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


############################################################
## Analisis

set.seed(0)
varIn <- c()
for (i in 1:30)
{
    varIn[i] <- tryCatch(sum(kmeans(encoding, centers = i)$withinss),
                         warnings = "warning", error = "aqui")  # Por si algun modelo falla
}




############################################################
## Visualizacion


# pistache, morado, rosa, verde pasto, azul, negro, purpura, naranja
my_colors <- c("#79c15a", "#7e5ea6", "#ee525f", "#047a52", "#3d85c6", "black", "#a7114e", "orange")


Jerarquico <- hclust(dist(encoding)) # Corremos un clustering jerarquico

K <- 8 # Numero de grupos
plot(Jerarquico)
rect.hclust(Jerarquico, K - 3) # Cortamos a K grupos

ets <- cutree(Jerarquico, k = K - 3)
ets[38]


# Examinamos en dendograma por omisión y obtenemos manualmente el orden en el que corta los grupos
hc.cols <- my_colors[c(5, 7, 4, 3, 6, 8, 2, 1)] # Definimos el orden de los colores

source("dendo.R") # Cargamos el código para la visualización

op = par(bg = "#FFFFFF")

A2Rplot(Jerarquico, k = K, boxes = F, col.up = "gray50", show.labels = F, hc.cols[c(1:5,5,5,5)],
		main = NULL) # Grafica para el reporte


# Colores y grupos:
# Grupo1: Negro, Grupo2: Rosa, Grupo3: Verde pasto, Grupo4: Purpura, Grupo5: Azul


sapply(1:(K-3), function(i) sum(ets == i))
