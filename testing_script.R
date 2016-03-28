setwd("/Users/psoberanis/PracticalMachineLearning/")
pml.train <- read.csv("pml-training.csv")
pml.test <- read.csv("pml-testing.csv")
set.seed(137)

#partition data
inTrain<- createDataPartition(y=pml.train$classe,p=0.6,list=FALSE)
locTrain<-pml.train[inTrain,]
locTest<-pml.train[-inTrain,]

#Near zero values removed
locTrainNZV <- nearZeroVar(locTrain, saveMetrics=TRUE)
locNZVvar<-!locTrainNZV$nzv
locTrain<-locTrain[locNZVvar]

#Remove large number of NA's
locTrain<-locTrain[c(-1)]
myTrain <- locTrain #creating another subset to iterate in loop
for(i in 1:length(locTrain)) { #for every column in the training dataset
        if( sum( is.na( locTrain[, i] ) ) /nrow(locTrain) >= .7 ) { #if n?? NAs > 70% of total observations
                for(j in 1:length(myTrain)) {
                        if( length( grep(names(locTrain[i]), names(myTrain)[j]) ) ==1)  { #if the columns are the same:
                                myTrain <- myTrain[ , -j] #Remove that column
                        }   
                } 
        }
}

cleanDataNames1<-colnames(myTrain)
cleanDataNames2<-colnames(myTrain[,-58])
myTest<-locTest[cleanDataNames1]
pml.test<-pml.test[cleanDataNames2]


###Coerce data
for (i in 1:length(pml.test) ) {
        for(j in 1:length(myTrain)) {
                if( length( grep(names(myTrain[i]), names(pml.test)[j]) ) ==1)  {
                        class(pml.test[j]) <- class(myTrain[i])
                }      
        }      
}
pml.test <- rbind(myTrain[2, -58] , pml.test) 
pml.test <- pml.test[-1,]

pml_rf<-train(classe~., data = myTrain, method='rf')
test1<-predict(pml_rf,myTest)

###

confusionMatrix(test1,myTest$classe)

pml_pred<-predict(pml_rf,pml.test)

