# Adresse du dossier où vous travaillez
setwd("/home/simo/I3/AnalyseDeDonnees/Code")
# Packages utilisés dans la suite
library(class)
library(caret)
library(ROCR)

# Supprimer toutes les variables
rm(list=ls(all=TRUE))
# Supprimer tous les graphiques déjà présents
graphics.off()

# Lecture des données d’apprentissage
load("../Data/Data/Projets/spam_data_train.rda");
names(data_train) <- c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13","x14","x15","x16","x17","x18","x19","x20","x21","x22","x23","x24","x25","x26","x27","x28","x29","x30","x31","x32","x33","x34","x35","x36","x37","x38","x39","x40","x41","x42","x43","x44","x45","x46","x47","x48","x49","x50","x51","x52","x53","x54","x55","x56","x57","y")
print(data_train)

# Séparation des données et de la sortie
data = data_train
set.seed(42)
rows <- sample(nrow(data_train))
rows_train = rows[c(1:2070)]
rows_test = rows[c(2071:2588)]
data_train = data[rows_train,] 
data_test = data[rows_test,]
data_train_x = data_train[,-c(58)]
data_test_x = data_test[,-c(58)]

#5. Appliquer les k-plus proches voisins sur les données d’apprentissage :
# nombre de voisins (par ex proche de la racine carré du nombre d’obs)
for (i in 1:30)
{
num_of_neigh <- i
data_predict <- knn(train=data_train_x,test=data_test_x,cl=data_train$y,k=num_of_neigh,prob=TRUE)

# Calcul du taux d’erreur
error_rate <- mean(data_predict != data_test$y)
cat("Pour k:",i,"error_rate using train data = ",error_rate)
cat("\n")
}

data_predict <- knn(train=data_train_x,test=data_test_x,cl=data_train$y,k=1)
# Matrice de confusion
confmat = table(data_predict,data_test$y)
print("Confusion Matrix")
print(confmat)
# vrais positifs + vrais negatifs + faux positifs + faux négatifs
TP = confmat[1,1]; TN = confmat[2,2]; FP = confmat[1,2]; FN = confmat[2,1];

# Sensibilité (sensitivity ; TPR = true positive rate)
TPR = TP/(TP+FN)
cat("TPR",TPR,"\n")
# Spécificité (specificity ; TNR = true negative rate)
TNR = TN/(TN+FP)
cat("TNR",TNR,"\n")
# Précision (precision ; positive predictive value)
PPV = TP/(TP+FP)
cat("PPV",PPV,"\n")
# se compare à la prévalence (prevalence)
cat("Prev =",length(data_test$y[data_test$y==1])/length(data_test$y),"\n")

cat("F-score = ",2 * TPR * PPV / (TPR+PPV),"\n")

