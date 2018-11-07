# Elev:           Marcus Stenroos
# Lärare:         Martin Gräslund
# Inlämningsdag:  2018-11-06
# Uppgift 2 med   C50 Decision Tree Algoritm

#install.packages("readxl")
#install.packages("C50")
library(readxl)
library(C50)



# läser in xls-filen till modell1
concrete <- read_excel("C:/Users/macka/Downloads/Concrete_Data.xls")

#Läser in xls-filen till modell2
concrete_2 <- read_excel("C:/Users/macka/Downloads/Concrete_Data.xls")




#av någon anledning behövdes Java uppdataras
Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jdk-11.0.1')




#Tar bort betong äldre än 100 dagar
concrete <- subset(concrete, concrete$`Age (day)` <= 100)

#Tar bort betong äldre än 100 dagar från modell2
concrete_2 <- subset(concrete_2, concrete_2$`Age (day)` <= 100)




#Döper om kolumnerna då dess namn är så långa
names(concrete) <- c("Cement", "Blast_Furnace","Fly_Ash", "Water", "Superplastic", "Coarse_Aggregate", "Fine_Aggregate",
                     "Age", "Compressive_Concrete")

#Döper om kolumnerna då dess namn är så långa för modell2
names(concrete_2) <- c("Cement", "Blast_Furnace","Fly_Ash", "Water", "Superplastic", "Coarse_Aggregate", "Fine_Aggregate",
                     "Age", "Compressive_Concrete")




#Splittar till test och trainingsset 1
split <- sample(2,nrow(concrete), replace = TRUE, prob = c(0.7,0.3))
concrete_train <- concrete[split==1,]
concrete_test <- concrete[split==2,]

#Splittar till test och trainingsset 2
split <- sample(2,nrow(concrete_2), replace = TRUE, prob = c(0.75,0.25))
concrete_2_train <- concrete_2[split==1,]
concrete_2_test <- concrete_2[split==2,]




#Gör Age.kolumnen till factor
concrete_train$Age <-as.factor(concrete_train$Age)

#Gör Age.kolumnen_2 till factor
concrete_2_train$Age <-as.factor(concrete_2_train$Age)



#Fixar modell för decision tree
concrete$Age <- factor(concrete$Age)
Decision_Tree_M <- C5.0(concrete_train[-8], concrete_train$Age )
Decision_Tree_M

#Fixar modell för decision tree2
concrete_2$Age <- factor(concrete_2$Age)
Decision_2_Tree_M <- C5.0(concrete_2_train[-8], concrete_2_train$Age )
Decision_2_Tree_M





#Gör en prediktion för modell1
concrete_prediction <- predict(Decision_Tree_M, concrete_test)
concrete_prediction

#Gör en prediktion för modell2
concrete_2_prediction <- predict(Decision_2_Tree_M, concrete_2_test)
concrete_2_prediction



#Skapar 2 subset för båda modellerna för att kunna göra beräkningar på
#prediktioner som gick rätt och de prediktioner som gick snett för att därgenom avgöra om 
#modellen fungerade bra eller inte.
wrongprediction <- (subset(concrete_test, concrete_test$Age != concrete_prediction))
Accurate_pred <- (subset(concrete_test, concrete_test$Age == concrete_prediction))

wrongprediction2 <- (subset(concrete_2_test, concrete_2_test$Age != concrete_2_prediction))
Accurate_pred_2 <- (subset(concrete_2_test, concrete_2_test$Age == concrete_2_prediction))


outcome <- nrow(wrongprediction2)/nrow(concrete_test) * 100


outcome2 <- nrow(wrongprediction2)/nrow(concrete_2_test) * 100

#Skriver ut resultaten
outcome
outcome2
