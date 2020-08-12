#variables
pltc <- read.table("train/X_train.txt")
drme <- read.table("test/X_test.txt")
X <- rbind(pltc, drme)
pltc  <- read.table("train/subject_train.txt")
drme <- read.table("test/subject_test.txt")
S <- rbind(pltc, drme)
pltc  <- read.table("train/y_train.txt")
drme <- read.table("test/y_test.txt")
Y <- rbind(pltc, drme)

#  extrayendo valores medidas de la media estandard 
carateristicas <- read.table("features.txt")
indices_de_caracteristicas <- grep("-mean\\(\\)|-std\\(\\)", features[, 2])
X <- X[, indices_de_caracteristicas]
rnombre(X) <- caracteristicas[indices_de_caracteristicas, 2]
rnombre(X) <- gsub("\\(|\\)", "", rnombre(X))
rnombre(X) <- tolower(rnombre(X))

#  se le ha asignado nombres para describir las actividades en el conjunto de datos
actividades <- read.table("activity_labels.txt")
actividades[, 2] = gsub("_", "", tolower(as.character(actividades[, 2])))
Y[,1] = actividades[Y[,1], 2]
rnombre(Y) <- "activity"

#  descripcion de actividad de data set
rnombre(S) <- "sujeto"
cleaned <- cbind(S, Y, X)
write.table(cleaned, "merged_clean_data.txt")

uniqueSubjects = unique(S)[,1]
numSubjects = length(unique(S)[,1])
numActivities = length(activities[,1])
numCols = dim(cleaned)[2]
result = cleaned[1:(numSubjects*numActivities), ]
row = 1
for (s in 1:numSubjects) {
for (a in 1:numActivities) {
result[row, 1] = uniqueSubjects[s]
result[row, 2] = activities[a, 2]
tmp <- cleaned[cleaned$subject==s & cleaned$activity==activities[a, 2], ]
result[row, 3:numCols] <- colMeans(tmp[, 3:numCols])
row = row+1
}
}
write.table(result, "data_set_with_the_averages.txt")
