

library(gexGA)
library(data.table)

ThemeBlank <- function(...){
	theme_bw() + theme(panel.grid = element_blank(),
					   axis.text = element_blank(),
					   axis.ticks = element_blank()
					   ,...)
}

HistoPlot <- function(data, x.name, title = NULL,
					  fill = 'gray'){
	
	pt <- ggplot(data) + geom_histogram(aes_string(x = x.name), fill = fill)
	pt <- pt + ylab(NULL) + theme_bw()
	
	pt <- pt + ggtitle(title)
	return(pt)
}

BarPlot <- function(data, x.name, y.name, type = 'identity', 
					title = NULL, fill = 'gray'){
	
	pt <- ggplot(data, aes_string(x = x.name, y = y.name))
	pt <- pt + geom_bar(stat = type, fill = fill, width = 0.3)
	
	pt <- pt + ylab(NULL) + theme_bw()
	return(pt)
}

DonutPlot <- function(data, y.name, title = NULL){
	
	pt <- ggplot(data, aes_string(fill = y.name, ymax = 'ymax', ymin = 'ymin', 
								  xmax = 4, xmin = 3))
	pt <- pt + geom_rect(colour = "grey30") + coord_polar(theta = "y")
	pt <- pt + xlim(c(0, 4))
	pt <- pt + ggtitle(NULL) + ylab(y.name) + scale_fill_discrete(name = NULL)
	pt <- pt + ThemeBlank(legend.key.size = unit(0.04, 'npc'))
	
	return(pt)
}

## Análisis descriptivo

datos <- read.csv('diabetes_data.csv')
head(datos)


g <- function(var.name, data){
	HistoPlot(data = data, x.name = var.name)
}

GetPlots <- function(data){
	
	plots <- list()
	vars <- colnames(data)
	
	for(i in 1:(length(vars))){
		plots[[i]] <- HistoPlot(data, vars[i], fill = 'springgreen3')
	}
	
	dat.aux <- as.data.table(data)
	dt1 <- dat.aux[, .N, by = class][, .(class, frec = N)]
	dt1 <- as.data.frame(dt1)
	dt1[, 'class'] <- as.factor(dt1[, 'class'])
	
	p2 <- BarPlot(dt1, x.name = 'class', y.name = 'frec', fill = 'springgreen3')
	plots[[9]] <- p2
	
	return(plots)
}

ScatterPlot <- function(data, var.x, var.y, color = 'orange', size = 1){
	pt <- ggplot(data, aes_string(x = var.x, y = var.y))
	pt <- pt + geom_point(size = size, color = color)
	pt <- pt + theme_bw()
	
	pt
}
	
Multiplot(plotlist = plots, cols = 3)




dat <- as.data.frame(cat.dat)
dat$class <- as.factor(dat$class)
dat$ymax = cumsum(dat$prop)
dat$ymin = c(0, head(dat$ymax, n=-1))
#p2 <- DonutPlot(dat, y.name = 'class')


## Revisamos los cuantiles

pecdf <- function(x, val, data){
	ecdf(data[, x])(val)
}


SetNAS <- function(x){
	x[x == 0] <- NA
	
	x
}

scales::percent(pecdf('diastolic.blood', 0, datos))
scales::percent(pecdf('triceps.skin.tickness', 0, datos))
scales::percent(pecdf('bmi', 0, datos))
scales::percent(pecdf('plasma.glucose', 0, datos))
scales::percent(pecdf('serum.insulin', 0, datos))

datos.clean <- datos

ReplaceVars <- function(x.name){
	datos.clean[, x.name] <- SetNAS(datos.clean[, x.name])
	return(datos.clean)
}

datos.clean <- ReplaceVars('diastolic.blood')
datos.clean <- ReplaceVars('bmi')
datos.clean <- ReplaceVars('serum.insulin')
datos.clean <- ReplaceVars('plasma.glucose')
datos.clean <- ReplaceVars('triceps.skin.tickness')


plots <- GetPlots(datos.clean)
Multiplot(plotlist = plots, cols = 3)


## Remplazando NAs

library(gexGA)

feats1 <- c('n.times.pregnant', 'age', 'pedigree.function')
feats2 <- c('age', 'pedigree.function')
feats3 <- c('pedigree.function')
feats4 <- c('age')
feats5 <- c('age', 'pedigree.function')

f1 <- BuildFormula('serum.insulin', feats)
f2 <- BuildFormula('triceps.skin.tickness', feats2)
f3 <- BuildFormula('bmi', feats3)
f4 <- BuildFormula('diastolic.blood', feats4)
f5 <- BuildFormula('plasma.glucose', feats5)

m1 <- lm(f1, data = datos.clean)
m2 <- lm(f2, data = datos.clean)
m3 <- lm(f3, data = datos.clean)
m4 <- lm(f4, data = datos.clean)
m5 <- lm(f5, data = datos.clean)

summary(m2)

resps <- c('serum.insulin', 'triceps.skin.tickness',
		   'bmi', 'diastolic.blood', 'plasma.glucose')


var.response <- resps[5]
pred <- predict(m5, newdata = datos.clean)
feats <- feats5

#datos.new <- datos.clean
datos.new[, var.response] <- pred

GetScatters <- function(data, resp.name, feats){
	resp.name <- as.character(resp.name)
	feats <- as.character(feats)
	pts <- list()
	
	for(i in 1:length(feats)){
		pts[[i]] <- ScatterPlot(data = data, var.y = resp.name[i], 
								var.x = feats[i])
	}
	
	pts
}


out1 <- data.frame('x' = resps[1], 'y' = feats1)
out2 <- data.frame('x' = resps[2], 'y' = feats2)
out3 <- data.frame('x' = resps[3], 'y' = feats3)
out4 <- data.frame('x' = resps[4], 'y' = feats4)
out5 <- data.frame('x' = resps[5], 'y' = feats5)

pairs <- rbind(out1, out2, out3, out4, out5)

pts <- GetScatters(datos.new, resp.name = pairs$x, feats = pairs$y)
Multiplot(cols = 3, plotlist = pts)

head(datos.new)
write.table(datos.new, file = 'diabetes_data_no_nas.csv', sep = ",",
			row.names = F)

hists <- GetPlots(datos.new)
Multiplot(plotlist = hists, cols = 3)

###############################################################################
## Pruebas con LDA


datos <- read.csv("diabetes_data_no_nas.csv")
head(datos)


pre.data <- as.data.frame(cbind(scale(as.matrix(datos[, -9])), 
								  'class' = datos[, 'class']))
head(pre.data)

hists <- GetPlots(pre.data)
Multiplot(plotlist = hists, cols = 3)


library(MASS)

var.exp <- names(datos)[!(names(datos) %in% c('bmi', 'diastolic.blood'))]
all.data <- pre.data

set.seed(123)
idxs <- sample(1:nrow(all.data), 0.7*nrow(all.data))
train.data <- all.data[idxs, ]
test.data <- all.data[-idxs, ]

train.data$class <- as.factor(train.data$class)
head(train.data)

var.exp <- c('n.times.pregnant', 'plasma.glucose', 'diastolic.blood')
lda.model <- lda(formula = BuildFormula('class', var.exp), data = train.data)

pred <- predict(lda.model, newdata = test.data)

head(pred$class)


spt <- table(pred$class, test.data$class)
caret::confusionMatrix(spt)


dat.pt <- data.frame('y' = 1, 'x' = pred$x[, 1], 'class' = test.data$class)
head(dat.pt)

pt <- ggplot(dat.pt, aes_string(x = 'x', y = 'y', color = 'class'))
pt <- pt + geom_point(size = 3)
pt <- pt + theme_bw()

pt
