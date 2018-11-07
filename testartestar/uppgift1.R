# Elev:           Marcus Stenroos
# Lärare:         Martin Gräslund
# Inlämningsdag:  2018-11-06
# Uppgift 2 med   C50 Decision Tree Algoritm

#install.packages("RWeka")
#install.packages("e1071")
#install.packages("caret")
#install.packages("rminer")
library(caret)
library(e1071)
library(RWeka) 

Sys.setenv(JAVA_HOME = 'C:/Program Files/Java/jdk-11.0.1')


#Hämtar ned datasettet
googleplaystore <- read.csv("C:/Users/macka/Downloads/googleplaystore.csv")


#Tar bort kolumner som jag inte använder
googleplaystore$App <- NULL
googleplaystore$Last.Updated <- NULL
googleplaystore$Current.Ver <- NULL
googleplaystore$Android.Ver <- NULL
googleplaystore$Installs <- NULL


#Tar bort en skräprad
googleplaystore <- googleplaystore[-c(10473),]



#tar bort rader med NaN innehåll
googleplaystore <- googleplaystore[complete.cases(googleplaystore),]



#Splittar till test och trainingsset
split <- sample(2,nrow(googleplaystore), replace = TRUE, prob = c(0.5,0.5))
genretrain <- googleplaystore[split==1,]
genretest <- googleplaystore[split==2,]



#bygger upp en klassifierare
genreclassifier <- naiveBayes(Genres ~., data = genretrain,laplace = 1)



#Gör prediktionen
genreprediction <- predict(genreclassifier, genretest)
genreprediction


#Skapar 2 subset för att kunna göra beräkningar på
#prediktioner som gick rätt och de prediktioner som gick snett för att därgenom avgöra om 
#modellen fungerade bra eller inte.
wrongprediction <- (subset(genretest, genretest$Genres != genreprediction))
Accurate_pred <- (subset(genretest, genretest$Genres == genreprediction))


#Räknar ut antalet procent på träffsäkerheten på prediktionen
outcome <- nrow(wrongprediction)/nrow(genretest) *100 
outcome

