############################################################
# Practica 1
#
#
#
#
############################################################


############################################################
## Preparacion de datos
setwd("/home/fou/Desktop/proyectos-diplomado-2016/6Modulo/")
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

stats <- (encoding2[encoding2>0] <-1)
apply(stats,1,sum)


############################################################
## Visualizacion
