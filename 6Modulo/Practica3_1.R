
library(psych)
library(knitr)

datos.brut <- read.csv("test_trabajo.csv", dec = ",")
head(datos.brut)

#install.packages("GPArotation") # GPArotation is required

datos.lim <- datos.brut[-c(1:3, 9)]
head(datos.lim)
#model.0 <- pca(datos.brut, 7)
#model.0

model.1 <- fa(r = datos.lim, nfactors = 3, fm = "pa", rotate = "oblimin")
model.1$loadings
model.1

model.1$communality[model.1$communality >= 0.7]
model.1$communality[model.1$communality < 0.7]

model.1$rotation
