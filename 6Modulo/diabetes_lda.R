

library(gexGA)

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

plots <- list()
vars <- colnames(datos)

for(i in 1:(length(vars))){
	plots[[i]] <- HistoPlot(datos, vars[i], fill = 'springgreen3')
}

library(data.table)

dat.aux <- as.data.table(datos)
dt1 <- dat.aux[, .N, by = class][, .(class, frec = N)]
cat.dat <- dt1[, .(class, prop = frec / sum(N))]

dt1 <- as.data.frame(dt1)
dt1[, 'class'] <- as.factor(dt1[, 'class'])
p2 <- BarPlot(dt1, x.name = 'class', y.name = 'frec', fill = 'springgreen3')
p2

dat <- as.data.frame(cat.dat)

dat$class <- as.factor(dat$class)
dat$ymax = cumsum(dat$prop)
dat$ymin = c(0, head(dat$ymax, n=-1))


#p2 <- DonutPlot(dat, y.name = 'class')
plots[[9]] <- p2

Multiplot(plotlist = plots, cols = 3)
