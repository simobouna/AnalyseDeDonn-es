# Adresse du dossier où vous travaillez
setwd("/home/simo/I3/AnalyseDeDonnees/Code")
# Packages utilisés dans la suite
library(randomForest)
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
rows <- sample(nrow(data_train))
rows_train = rows[c(1:2070)]
rows_test = rows[c(2071:2588)]
data_train = data[rows_train,] 
data_test = data[rows_test,]
data_train_x = data_train[,-c(58)]
data_test_x = data_test[,-c(58)]



# Création de la sortie (à mettre sous le format facteur sinon
# un modèle de régression est créé)
data_train_y <- as.factor(data_train$y)

# Forêts aléatoires
rf <- randomForest(x = data_train_x, y = data_train_y)

# Affichage des résultats
print(rf)

# Importance des variables
rf$importance
varImpPlot(rf)


# Prediction sur les données test
rf_predit_data_test <- predict(rf, newdata=data_test_x)

# Comparaison des valeurs prédites et des valeurs observées
table(rf_predit_data_test, data_test$y)

# Calcul du taux d’erreur
error_rate <- mean(rf_predit_data_test != data_test$y)
cat("error_rate using test data = ",error_rate)

# Validation du modèle par validation croisée
# MSE <- 0
# for (i in 1:647)
# {
#   datatopredict <- data_train$y[c(i,i+1,i+2,i+3)]
#   datatemp <- data_train[-c(i,i+1,i+2,i+3),]
#   data_train_y <- as.factor(datatemp$y)
#   rf <- randomForest(x = datatemp[,-c(58)], y = data_train_y)
#   
#   predictedvalue <- predict(rf, newdata= data_train[,-c(58)][c(i,i+1,i+2,i+3),])
#   for (j in 1:4)
#   {
#     MSE <- MSE+(as.numeric(as.character(datatopredict[j]))-as.numeric(as.character(predictedvalue[j])))^2
#   }
# cat("MSE :",MSE, "i :",i,"\n")
# }
# MSE <- MSE/647
# cat("Valeur du résidu avec la validation croisée", MSE)

#Validation du modèle par validation croisée
MSE <- 0
for (i in 0:3)
{
  rows_test = rows[c((i*647):(646+i*647))]
  x = ((3-i)*647)%%4
  y = ((3-i-2)*647)%%4
  rows_train = rows[c((647*x):(647*y + 647))]
  data_train = data[rows_train,] 
  data_test = data[rows_test,]
  data_train_x = data_train[,-c(58)]
  data_test_x = data_test[,-c(58)]
  data_train_y <- as.factor(data_train$y)
  rf <- randomForest(x = data_train_x, y = data_train_y)
  rf_predit_data_test <- predict(rf, newdata=data_test_x)
  error_rate <- mean(rf_predit_data_test != data_test$y)
  cat("error_rate using test data = ",error_rate,"\n")
  for (j in 1:646)
  {
      MSE <- MSE+(as.numeric(as.character(rf_predit_data_test[j]))-as.numeric(as.character(data_test$y[j])))^2
  }
}
MSE <- MSE/4
cat("Valeur du résidu avec la validation croisée", MSE)

