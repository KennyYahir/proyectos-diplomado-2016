
datos.brut <- read.csv("test_trabajo.csv", dec = ",")
head(datos.brut)

#install.packages("GPArotation") # GPArotation is required
library(psych)
library(knitr)

datos.lim <- datos.brut#[-ncol(datos.brut)]

model.0 <- pca(datos.brut, 7)
model.0

model.1 <- fa(r = datos.lim, nfactors = 4, fm = "pa", rotate = "oblimin")
class(model.1$loadings)
model.1

as.data.frame(model.1$loadings)
model.1$loadings
