
path1 <- "https://archive.ics.uci.edu/ml/machine-learning-databases/"
path2 <- "pima-indians-diabetes/pima-indians-diabetes.data"


dat.brut <- read.table(paste0(path1, path2), header = F, sep = ",")
colnames(dat.brut) <- c("n.times.pregnat", 
						"plasma.glucose",
						"diastolic.blood",
						"triceps.skin.tickness",
						"serum.insulin",
						"bmi",
						"pedigree.function",
						"age",
						"class")

head(dat.brut)

write.table(dat.brut, file = 'diabetes_data.csv', sep = ',', row.names = F)
