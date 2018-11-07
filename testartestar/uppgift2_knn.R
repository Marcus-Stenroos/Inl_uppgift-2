# Elev:           Marcus Stenroos
# Lärare:         Martin Gräslund
# Inlämningsdag:  2018-11-06
# Uppgift 2       med k-NN




#install.packages("class")
#install.packages("caTools")
#install.packages("gmodels")
library(class)
library(caTools)
library(datasets)
library(gmodels)


# läser in xls-filen
concrete_knn <- read_excel("C:/Users/macka/Downloads/Concrete_Data.xls")

concrete$Age

#Tar bort rader med dagar större än 100
concrete_knn <- subset(concrete_knn, concrete_knn$`Age (day)`<= 100)



#Döper om kolumnerna för att få namnen mer tydliga och framförallt kortare
names(concrete_knn) <- c("Cement", "Blast_Furnace","Fly_Ash", "Water", "Superplastic", "Coarse_Aggregate", "Fine_Aggregate",
                     "Age", "Compressive_Concrete")



#Gör factor av age
concrete_knn$Age <- factor(concrete_knn$Age )




#Splittar upp till tränings och testdata (hårdkodad)
concrete_knn_train <- concrete_knn[1:776, ]
concrete_knn_test <- concrete_knn[777:968, ]

concrete_knn_train_label <- concrete_knn[1:776, ]
concrete_knn_test_label <- concrete_knn[777:968, ]

#Kollar vilket K värde som vi kan använda. Om jämnt avrundar jag uppåt till närmaste ojämnatal

#sqrt(968)
#31


#Passar in kNN
knn_test_pred <- knn(train = concrete_knn_train,
              test = concrete_knn_test,
              cl = concrete_knn_train_label$Age,
              k = 31
              )

CrossTable(x =  concrete_knn_test_label$Age , y = knn_test_pred,
           prop.chisq = FALSE)





str(knn_test_pred)
str(concrete_knn_train$Age)

#Försöker förbättra modellens träffsäkerhet tar bort target dvs Age då den inte ska påverkas av z-score
concrete_z_score <- as.data.frame(scale(concrete_knn[-8]))



#Kontrollerar att transformationen lyckades dvs medelvärdet ska ligga på noll i en z-score standalisering
#summary(concrete_z_score_train)



#Splittar upp till tränings och testdata (hårdkodad)
concrete_knn_train <- concrete_z_score[1:776, ]
concrete_knn_test <- concrete_z_score[777:968, ]



#Skapar labels som ska användas i knn modellen. Target Age ska sättas tillbaka
concrete_z_score_train_labels <-concrete_knn[1:776, ]
concrete_z_score_test_labels <- concrete_knn[777:968, ]



#Sätter in Knn
knn_test_pred2 <- knn(train = concrete_knn_train,
                     test = concrete_knn_test,
                     cl = concrete_z_score_train_labels$Age,
                     k = 5
)

#Ställer upp crostables för båda för att se hur bra prediktningarna motsvarar förväntningarna
CrossTable(x =  concrete_knn_test_label$Age , y = knn_test_pred,
           prop.chisq = FALSE)

CrossTable(x = concrete_z_score_test_labels$Age , y = knn_test_pred2,
           prop.chisq = FALSE)
