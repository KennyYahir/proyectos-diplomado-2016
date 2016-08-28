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


############################################################
## Analisis

set.seed(0)
varIn <- c()
for (i in 1:6)
{
    varIn[i] <- tryCatch(sum(kmeans(encoding, centers = i)$withinss),
                         warnings = "warning", error = "aqui")  # Por si algun modelo falla
}

Jerarquia <- hclust(dist(encoding))
GruposJerarquia <- cutree(Jerarquia, 5)
GruposJerarquia

FiltroMateria <- function(grupo, etiquetas, datos, dat2)
{
    Filtro <- function(datos, etiquetas)
        {
        datos[ datos==0] <- NA
        kpi <- apply(datos[etiquetas==grupo,], 2, mean, na.rm = TRUE)
        top <- data.frame( Top  = names(kpi[order(kpi, decreasing = TRUE)][1:3]), kpi = kpi[order(kpi, decreasing = TRUE)][1:3])
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

library(sqldf)
for (i in 1:5)
{            
    inter<-sqldf(paste0( paste0('select Top from Top4 intersect select Top from Top',i)))
    print (inter)    
}





e <- data.frame(Top = rownames(t(encoding)))
a <- merge(e,dat2, by.x='Top', by.y = 'clave', all.x=TRUE)
ax <- as.character(a$materia) 
ax[is.na(ax)] <- 'Kenny'
encoding2 <- t(encoding)
rownames(encoding2) <- ax

lala<-hclust(dist(encoding2), method = "centroid")
Gruposlala <- cutree(lala, 5)
#lala$labels <- e['materia']
plot(lala)
rect.hclust(lala,5)
sqldf("select * from dat2 where clave=2033")
stats <- encoding2
#stats[ stats >0] <- 1
frecc <- data.frame(frec = apply(stats,1,sum), nom = rownames(stats))

pt1 <- ggplot(data = frecc, aes(x = nom, y = frec)) + geom_bar(stat = "identity") + coord_flip()
pt1 + theme_bw() + theme(axis.text.x = element_text(angle = 90))

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
