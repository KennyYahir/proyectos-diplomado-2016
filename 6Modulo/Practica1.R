############################################################
# Practica 1
#
#
#
#
############################################################


############################################################
## Preparacion de datos
#setwd("/home/fou/Desktop/proyectos-diplomado-2016/6Modulo/")
dat1 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - Base completa de eleccion.csv", 
				 dec = ",")

dat2 <- read.csv("eleccciodeoptativas2016grupo2.xlsx - resumen.csv", header = F)[, 1:2]

library(reshape2)

head(dat2)
colnames(dat2) <- c("materia", "clave")

dat1 <- na.omit(dat1)
dat2 <- na.omit(dat2)

trainData.pre <- dat1[, c(1, 3:4)]

head(x <- melt(trainData.pre, id.vars = c("Individuo","Clave")))
head(encoding <- acast(x, Individuo ~ Clave))

index <- is.na(encoding) # Buscamos los NA's
encoding[index] <- 0 # Los remplazamos


############################################################
## Analisis

set.seed(0)
varIn <- c() # Vector que guardara las varianzas totales

for (i in 1:7)
{
    varIn[i] <- tryCatch(sum(kmeans(encoding, centers = i)$withinss),
                         warnings = "warning", error = "aqui")  # Por si algun modelo falla
}

plot(varIn, col = "orange", xlab = "No. clusters", pch = 19)
lines(varIn, col = "orange") # Graficamos la varianza acumulada

Jerarquia <- hclust(dist(encoding)) # Cluster Jerarquico desdepues de hacer una elección del K
GruposJerarquia <- cutree(Jerarquia, 5) # con K-means

FiltroMateria <- function(grupo, etiquetas, datos, dat2)
{
    Filtro <- function(datos, etiquetas)
        {
        datos[ datos==0] <- NA
        kpi <- apply(datos[etiquetas==grupo,], 2, mean, na.rm = TRUE)
        top <- data.frame( Top  = names(kpi[order(kpi, decreasing = TRUE)][1:3]), 
        				   kpi = kpi[order(kpi, decreasing = TRUE)][1:3])
        Top <- merge(top, dat2, by.x = 'Top', by.y = 'clave')
        return(Top[order(Top['kpi']),])
        }
    
    
}

Mahil <- FiltroMateria(1, GruposJerarquia, encoding, dat2 )
Top1 <- Mahil(encoding, GruposJerarquia)
Top1[order(Top1['kpi'], decreasing = TRUE),]

Mahil <- FiltroMateria(2, GruposJerarquia, encoding, dat2 )
Top2 <- Mahil(encoding, GruposJerarquia)
Top2[order(Top2['kpi'], decreasing = TRUE),]

Mahil <- FiltroMateria(3, GruposJerarquia, encoding, dat2 )
Top3 <- Mahil(encoding, GruposJerarquia)
Top3[order(Top3['kpi'], decreasing = TRUE),]


Mahil <- FiltroMateria(4, GruposJerarquia, encoding, dat2 )
Top4 <- Mahil(encoding, GruposJerarquia)
Top4[order(Top4['kpi'], decreasing = TRUE),]

Mahil <- FiltroMateria(5, GruposJerarquia, encoding, dat2 )
Top5 <- Mahil(encoding, GruposJerarquia)
Top5[order(Top5['kpi'], decreasing = TRUE),]

#library(sqldf)
#for (i in 1:5)
#{            
#    inter <- sqldf(paste0( paste0('select Top from Top4 intersect select Top from Top',i)))
#    print (inter)    
#}


e <- data.frame(Top = rownames(t(encoding)))
a <- merge(e, dat2, by.x = 'Top', by.y = 'clave', all.x = TRUE) # Cruze de las tablas
ax <- as.character(a$materia)
ax.nas <- is.na(ax) # Posiciones de los NA's
ax[ax.nas] <- paste0('na', 1:sum(ax.nas))
encoding2 <- t(encoding)
rownames(encoding2) <- ax

### Ahora tratamos de agrupar las materias

lala <- hclust(dist(encoding2), method = "centroid")
Gruposlala <- cutree(lala, 5)

plot(lala)
rect.hclust(lala, 5) # No obtenemos un buen agrupamiento :(
sqldf("select * from dat2 where clave=2033")

library(data.table)
library(ggplot2)

stats <- encoding2

stats[ stats == 0] <- NA # Regresamos los valores a NA
stats.media <- apply(stats, 1, mean, na.rm = T)

stats[!is.na(stats)] <- 1 # Solo contaremos las apariciones de cada materia(no importa la preferencia)
stats.suma = apply(stats, 1, sum, na.rm = T)

frecc <- data.table(media = round(stats.media, 2),
					suma = stats.suma,
					nom = rownames(stats)) # Para contar por incidencia y no por preferencia)

var.sel <- "suma"
frecc <- frecc[order(frecc[ , var.sel, with = F])] # Orden de las barras
frecc$nom <- factor(frecc$nom, levels = unique(frecc$nom)) # Para poder mantener un orden distinto


pt1 <- ggplot(data = frecc, aes_string(x = "nom", y = var.sel, fill = var.sel, label = var.sel))
pt2 <- pt1 + geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low = "blue", 
																			  high = "red")
pt3 <- pt2 + geom_text(hjust = -0.08) + scale_y_continuous(limits = c(0, 1.1*max(frecc[, var.sel, 
																					   with = F])))
pt4 <- pt3 + xlab(NULL) + ylab("Frecuencia")
pt4 + theme_classic() + theme(legend.position = "none")


var.sel <- "media"
frecc <- frecc[order(frecc[ , var.sel, with = F])] # Orden de las barras
frecc$nom <- factor(frecc$nom, levels = unique(frecc$nom)) # Para poder mantener un orden distinto


pt1 <- ggplot(data = frecc, aes_string(x = "nom", y = var.sel, fill = var.sel, 
									   label = var.sel, 2))
pt2 <- pt1 + geom_bar(stat = "identity") + coord_flip() + scale_fill_gradient(low = "blue", 
																			  high = "red")
pt3 <- pt2 + geom_text(hjust = -0.08) + scale_y_continuous(limits = c(0, 1.1*max(frecc[, var.sel, 
																					   with = F])))
pt4 <- pt3 + xlab(NULL) + ylab("Frecuencia")
pt4 + theme_classic() + theme(legend.position = "none")
############################################################
## Visualizacion


# pistache, morado, rosa, verde pasto, azul, negro, purpura, naranja
my_colors <- c("#79c15a", "#7e5ea6", "#ee525f", "#047a52", "#3d85c6", "black", "#a7114e", "orange")


Jerarquico <- hclust(dist(encoding)) # Corremos un clustering jerarquico

K <- 8 # Numero de grupos
plot(Jerarquico)
rect.hclust(Jerarquico, K - 3) # Cortamos a K grupos

ets <- cutree(Jerarquico, k = K - 3)


# Examinamos en dendograma por omisión y obtenemos manualmente el orden en el que corta los grupos
hc.cols <- my_colors[c(5, 7, 4, 3, 6, 8, 2, 1)] # Definimos el orden de los colores

source("dendo.R") # Cargamos el código para la visualización del dendograma

op = par(bg = "#FFFFFF")

A2Rplot(Jerarquico, k = K, boxes = F, col.up = "gray50", show.labels = F, 
		hc.cols[c(1:5, 5, 5, 5)],
		main = NULL) # Grafica para el reporte


# Colores y grupos:
# Grupo1: Negro, Grupo2: Rosa, Grupo3: Verde pasto, Grupo4: Purpura, Grupo5: Azul

sapply(1:(K-3), function(i) sum(ets == i))
