
###########################################
library(doParallel)
# Find how many cores are on your machine
detectCores() # Result = Typically 4 to 6

# Create Cluster with desired number of cores. Don't use them all! Your computer is running other processes. 
cl <- makeCluster(2)
registerDoParallel(cl)
on.exit(stopCluster(cl))


# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# Stop Cluster. After performing your tasks, stop your cluster. 
#stopCluster(cl)
###########################################
library(plotly)
library(corrplot)
library(dplyr)
library(caret)
library(kknn)
library(e1071 )
library(arules)


############################################################Laod Data##################################################

iphoneData<-read.delim("~/Desktop/Ubiqium/Data Analytics/Big Data/ismallmatrix_labeled_8d.csv",",",header=TRUE)
SamsungData<-read.delim("~/Desktop/Ubiqium/Data Analytics/Big Data/ismallmatrix_labeled_8d.csv",",",header = TRUE)
sentiment<-read.delim("~/Desktop/Ubiqium/Data Analytics/Big Data/sentiment.csv",",",header = TRUE)


iphoneData$iphonesentiment<-NULL
SamsungData$iphonesentiment<-NULL


iphoneData$iphoneSentiment<-sentiment$iphoneSentiment
SamsungData$galaxySentiment<-sentiment$galaxySentiment  
   
all.equal(iphoneData,SamsungData)
table(SamsungData$galaxySentiment)
table(iphoneData$iphoneSentiment)
########NA'S and type of vars 
anyNA(iphoneData)
anyNA(SamsungData)


labels(iphoneData$iphoneSentiment)
str(iphoneData)
summary(iphoneData)
dim(iphoneData)
table(iphoneData$iphonesentiment)
summary(iphoneData$iphonesentiment)


labels(SamsungData)
str(SamsungData)
summary(SamsungData)
dim(SamsungData)
table(SamsungData$galaxysentiment)
summary(SamsungData$galaxySentiment)



#leave only sentiment 


labels(SamsungDataOnlySentiment)
labels(iphoneDataOnlySentiment)




iphoneDataOnlySentiment$Sums <- rowSums(iphoneDataOnlySentiment[,1:44])
table(iphoneDataOnlySentiment$Sums)

iphoneDataOnlySentiment<-filter(iphoneDataOnlySentiment, Sums !=0)

iphoneDataOnlySentiment$Sums<-NULL


SamsungDataOnlySentiment$Sums <- rowSums(SamsungDataOnlySentiment[,1:44])
table(SamsungDataOnlySentiment$Sums)

SamsungDataOnlySentiment<-filter(SamsungDataOnlySentiment, Sums !=0)

SamsungDataOnlySentiment$Sums<-NULL









#####Corleation  

res1 <- cor.mtest(iphoneData,conf.level = .95)
res2 <- cor.mtest(iphoneDataOnlySentiment, conf.level = .95)
res3 <- cor.mtest(SamsungData, conf.level = .95)
res4 <- cor.mtest(SamsungDataOnlySentiment, conf.level = .95)


#Iphone

IphoneCorr<-cor(iphoneData)

corrplot(IphoneCorr, order="hclust", 
         p.mat = res1$p, sig.level = 0.01,insig = "blank",tl.cex=0.5,method = "number",number.cex = 0.25)


summary(IphoneCorr[upper.tri(IphoneCorr)])

highlyCorIphone <- findCorrelation(IphoneCorr, cutoff = .80) 



#iphoneDataOnlySentiment
iphoneDataOnlySentimentCorr<-cor(iphoneDataOnlySentiment)

corrplot(iphoneDataOnlySentimentCorr, order="hclust", 
         p.mat = res2$p, sig.level = 0.01,insig = "blank",tl.cex=0.5,method = "number",number.cex = 0.25)

summary(iphoneDataOnlySentimentCorr[upper.tri(iphoneDataOnlySentimentCorr)])

highlyCorIphoneOnlySentiment <- findCorrelation(iphoneDataOnlySentimentCorr, cutoff = .90) 


#Only independent 
iphoneDataOnlySentimentCorrIndependent<-cor(iphoneDataOnlySentiment[,1:44])

corrplot(iphoneDataOnlySentimentCorrIndependent, order="hclust", 
         p.mat = res2$p, sig.level = 0.01,insig = "blank",tl.cex=0.5,method = "number",number.cex = 0.25)

summary(iphoneDataOnlySentimentCorrIndependent[upper.tri(iphoneDataOnlySentimentCorrIndependent)])

highlyCorIphoneOnlySentimentIndependet <- findCorrelation(iphoneDataOnlySentimentCorrIndependent, cutoff = .80) #same as with dependent



#new file with without highly coralted atrubites 

iPhoneCorrelation <- iphoneDataOnlySentiment[, -highlyCorIphoneOnlySentiment]



#Samsung

SamsungCorr<-cor(SamsungData)

corrplot(IphoneCorr, order="hclust", 
         p.mat = res3$p, sig.level = 0.01,insig = "blank",tl.cex=0.5,method = "number",number.cex = 0.25)


#SamsungDataOnlySentiment
SamsungDataOnlySentimentCorr<-cor(SamsungDataOnlySentiment)

corrplot(iphoneDataOnlySentimentCorr, order="hclust", 
         p.mat = res4$p, sig.level = 0.01,insig = "blank",tl.cex=0.5,method = "number",number.cex = 0.25)

highlyCorSamsung <- findCorrelation(SamsungDataOnlySentimentCorr, cutoff = .90)

SamsungCorrelation <- SamsungDataOnlySentiment[, -highlyCorSamsung]
SamsungCorrelation$galaxySentiment<-SamsungDataOnlySentiment$galaxySentiment




#################################################
###########################DISCRPTIVE####################################


-iphoneDataOnlySentiment$iphoneSentiment


str(sentiment)



iphoneDataOnlySentiment$iphoneSentiment<-disfixed3
SamsungDataOnlySentiment$galaxySentiment<-disfixed3S

levels(iphoneDataOnlySentiment$iphoneSentiment)<- c("negative","neutral","positive")
levels(SamsungDataOnlySentiment$galaxySentiment)<- c("negative","neutral","positive")
levels(sentiment$iphoneSentiment)<- c("very negative","negative","somewhat negative","somewhat positive","neutral","positive","very positive")
levels(sentiment$galaxySentiment)<- c("very negative","negative","somewhat negative","somewhat positive","neutral","positive","very positive")




plot_ly(sentiment, x= ~sentiment$iphoneSentiment, type='histogram')
plot_ly(sentiment, x= ~sentiment$galaxySentiment, type='histogram')

summary(SamsungData$galaxySentiment)

 ggplot()+
  geom_density(data = sentiment, aes(x=iphoneDataOnlySentiment$iphoneSentiment),fill="black",alpha=0.8) +
  # Change the fill colour to differentiate it
  geom_density(data=sentiment,aes(SamsungDataOnlySentiment$galaxySentiment), fill="pink",alpha=0.3) +
  labs(color = "fill")+
  labs(title = "Distribution Of Sentiment")+
  labs(y="Density")+
  labs(x="IPhone vs Samsung Galaxy")
 #################################################
 


#Iphone 
nzvMetrics <- nearZeroVar(iphoneDataOnlySentiment, saveMetrics = TRUE)
nzvMetrics
nzv <- nearZeroVar(iphoneDataOnlySentiment, saveMetrics = FALSE) 
nzv

#Samsung 
nzvMetricsSamungSen <- nearZeroVar(SamsungDataOnlySentiment, saveMetrics = TRUE)
nzvMetricsSamungSen

nzvSamsungSen <- nearZeroVar(SamsungDataOnlySentiment, saveMetrics = FALSE) 
nzvSamsungSen




# create a new data set and remove near zero variance features

iPhoneNZV<-iphoneDataOnlySentiment[,-nzv]
iPhoneNZV$iphoneSentiment<-iphoneDataOnlySentiment$iphoneSentiment
str(iPhoneNZV)

SamsungNZV <- SamsungDataOnlySentiment[,-nzvSamsungSen]

str(SamsungNZV)

#####RFE 




# Let's sample the data before using RFE
# Register Cluster
registerDoParallel(cl)

# Confirm how many cores are now "assigned" to R and RStudio
getDoParWorkers() # Result 2 

# Stop Cluster. After performing your tasks, stop your cluster. 
stopCluster(cl)



set.seed(222)

iphoneDataOnlySentimentSample <-iphoneDataOnlySentiment[sample(1:nrow(iphoneDataOnlySentiment), 1000, replace=FALSE),]

SamsungSampleSentiment <- SamsungDataOnlySentiment[sample(1:nrow(SamsungDataOnlySentiment), 1000, replace=F),]
table(SamsungDataOnlySentiment$galaxySentiment)
table(iphoneDataOnlySentiment$iphoneSentiment)


# Set up rfeControl with randomforest, repeated cross validation and no updates
ctrl <- rfeControl(functions = rfFuncs, 
                   method = "repeatedcv",
                   repeats = 5,
                   verbose = FALSE)

# Use rfe and omit the response variable (attribute 59 iphonesentiment) 
rfeResultsIphoneSent <- rfe(iphoneDataOnlySentimentSample[,1:44], 
                            iphoneDataOnlySentimentSample$iphoneSentiment, 
                            sizes=(1:44), 
                            rfeControl=ctrl)


rfeResultsSamsung <- rfe(SamsungSampleSentiment[,1:44], 
                         SamsungSampleSentiment$galaxySentiment, 
                            sizes=(1:44), 
                            rfeControl=ctrl)




# Get results
rfeResultsSamsung
rfeResultsIphoneSent

# Plot results
plot(rfeResultsSamsung, type=c("g", "o"))
plot(rfeResultsIphoneSent, type=c("g", "o"))



# create new data set with rfe recommended features

iphoneRFE <- iphoneDataOnlySentiment[,predictors(rfeResultsIphoneSent)]

SamsungRFE <- SamsungDataOnlySentiment[,predictors(rfeResultsSamsung)]



# add the dependent variable to iphoneRFE
iphoneRFE$iphoneSentiment<- iphoneDataOnlySentiment$iphoneSentiment
SamsungRFE$galaxySentiment<-SamsungDataOnlySentiment$galaxySentiment

anyNA(iphoneRFE)

table(iphoneRFE$iphoneSentiment)

# review outcome
str(iphoneRFE)
str(iphoneRFESen)







disfixed3 <- discretize(iphoneDataOnlySentiment$iphoneSentiment, "fixed", categories= c(-Inf,-1, 1,Inf))
disfixed7 <- discretize(iphoneDataOnlySentiment$iphoneSentiment, "fixed", categories= c(-Inf, -50, -10, -1, 1, 10, 50, Inf))

disfixed7S <- discretize(SamsungDataOnlySentiment$galaxySentiment, "fixed", categories= c(-Inf, -50, -10, -1, 1, 10, 50, Inf))

iphoneDataOnlySentiment$iphoneSentiment<-disfixed3
SamsungDataOnlySentiment$galaxySentiment<-disfixed7S
  
levels(iphoneDataOnlySentiment$iphoneSentiment)<- c("very negative","negative","somewhat negative","somewhat positive","neutral","positive","very positive")
levels(SamsungDataOnlySentiment$galaxySentiment)<- c("very negative","negative","somewhat negative","somewhat positive","neutral","positive","very positive")
table(iphoneDataOnlySentiment$iphoneSentiment)
table(SamsungDataOnlySentiment$galaxySentiment)

iPhoneCorrelation$iphoneSentiment<-iphoneDataOnlySentiment$iphoneSentiment
IphoneNZV$iphoneSentiment<-iphoneDataOnlySentiment$iphoneSentiment




#######################################################MODELS


#All of the Data 



set.seed(666)

inTraining <- createDataPartition(iphoneDataOnlySentiment$iphoneSentiment, p = .75, list = FALSE)
training <- iphoneDataOnlySentiment[ inTraining,]
testing  <- iphoneDataOnlySentiment[-inTraining,]



Control <- trainControl(method = "repeatedcv",repeats = 5,class,Probs=T)





C5Iphone<- train(iphoneSentiment~., data=training, method= "C5.0", trControl = Control,preProc = c("center", "scale"))

plot(C5Iphone) 

C5IphoneOnlySen<-predict(C5Iphone, testing)

confusionMatrix(testing$iphoneSentiment, C5IphoneOnlySen)
# Accuracy : 0.8745          
# 95% CI : (0.8414, 0.9028)
# No Information Rate : 0.8054          
# P-Value [Acc > NIR] : 4.037e-05       
# 
# Kappa : 0.6604          
# Mcnemar's Test P-Value : NA      

#KNN


KNN_All <- train.kknn(iphoneSentiment~., data=training,trControl = Control,preProcess = c("center","scale"),kmax = 10, )
?make.names 


plot(KNN_All)

KNNIphoneOnlySen<-predict(KNN_All, testing)

confusionMatrix(testing$iphoneSentiment,KNNIphoneOnlySen)

# Accuracy : 0.8556          
# 95% CI : (0.8209, 0.8859)
# No Information Rate : 0.7908          
# P-Value [Acc > NIR] : 0.0001793       
# 
# Kappa : 0.6193          
# Mcnemar's Test P-Value : NA     

RandomeForest_All_Iphone <- train(iphoneSentiment~., data=training,
                 method = "rf", 
                 trControl = Control,
                 tuneLength = 3)

plot(RandomeForest_All_Iphone )

RandomeForest_All_model<-predict(RandomeForest_All_Iphone, testing)

confusionMatrix(testing$iphoneSentiment,RandomeForest_All_Iphone)

# Accuracy : 0.8724         
# 95% CI : (0.8391, 0.901)
# No Information Rate : 0.795          
# P-Value [Acc > NIR] : 6.608e-06      
# 
# Kappa : 0.6612         
# Mcnemar's Test P-Value : NA   

svmFit <- train(iphoneSentiment~., data=training, 
                method = "svmLinear", 
                trControl = Control, 
                preProc = c("center", "scale"),
                tuneLength = 8)




SVM_All_Predcit<-predict(svmFit, testing)

confusionMatrix(testing$iphoneSentiment,SVM_All_Predcit)

# Accuracy : 0.8598          
# 95% CI : (0.8254, 0.8897)
# No Information Rate : 0.8661          
# P-Value [Acc > NIR] : 0.6854          
# 
# Kappa : 0.5713          
# Mcnemar's Test P-Value : NA    

NaiveBayes_All <- train(iphoneSentiment~., data=training, 
              method = "nb",trControl=Control)








########Models for NVZ

set.seed(666)

inTrainingNZV <- createDataPartition(iPhoneNZV$iphoneSentiment, p = .75, list = FALSE)
trainingNZV <- iPhoneNZV[ inTrainingNZV,]
testingNZV  <- iPhoneNZV[-inTrainingNZV,]




C5iPhoneNZV<- train(iphoneSentiment~., data=trainingNZV, method= "C5.0", trControl = Control,preProc = c("center", "scale"))

plot(C5iPhoneNZV) 

C5IC5iPhoneNZV_predict<-predict(C5iPhoneNZV,testingNZV)

confusionMatrix(testingNZV$iphoneSentiment, C5IC5iPhoneNZV_predict)
# Accuracy : 0.8766          
# 95% CI : (0.8437, 0.9047)
# No Information Rate : 0.8054          
# P-Value [Acc > NIR] : 2.335e-05       
# 
# Kappa : 0.6657          
# Mcnemar's Test P-Value : NA             

#KNN


KNN_NZV <- train(iphoneSentiment~., data=trainingNZV,method= "kknn",trControl = Control,preProcess = c("center","scale"))

plot(KNN_NZV)

KNNiPhone_NZV<-predict(KNN_NZV , testingNZV)

confusionMatrix(testingNZV$iphoneSentiment,KNNiPhone_NZV)

# Accuracy : 0.8556          
# 95% CI : (0.8209, 0.8859)
# No Information Rate : 0.7908          
# P-Value [Acc > NIR] : 0.0001793       
# 
# Kappa : 0.6193          
# Mcnemar's Test P-Value : NA     

RandomeForest_NZV<- train(iphoneSentiment~., data=trainingNZV,
                           method = "rf", 
                           trControl = Control,
                           tuneLength = 3)

plot(RandomeForest_NZV)

RandomeForest_NZV_model<-predict(RandomeForest_NZV, testingNZV)

confusionMatrix(testingNZV$iphoneSentiment,RandomeForest_NZV_model)

# Accuracy : 0.8766          
# 95% CI : (0.8437, 0.9047)
# No Information Rate : 0.8013          
# P-Value [Acc > NIR] : 8.947e-06       
# 
# Kappa : 0.6683          
# Mcnemar's Test P-Value : NA      

svmFitNZV <- train(iphoneSentiment~., data=trainingNZV, 
                method = "svmLinear", 
                trControl = Control, 
                preProc = c("center", "scale"),
                tuneLength = 8)




SVM_NZV_Predcit<-predict(svmFitNZV, testingNZV)

confusionMatrix(testingNZV$iphoneSentiment,SVM_NZV_Predcit)

# Accuracy : 0.8473          
# 95% CI : (0.8118, 0.8783)
# No Information Rate : 0.9121          
# P-Value [Acc > NIR] : 1               
# 
# Kappa : 0.4827          
# Mcnemar's Test P-Value : NA    


########Models for Corrlation 

set.seed(666)

inTrainingCor <- createDataPartition(iPhoneCorrelation$iphoneSentiment, p = .75, list = FALSE)
trainingCor <- iPhoneCorrelation[ inTrainingCor,]
testingCor <- iPhoneCorrelation[-inTrainingCor,]




C5iPhoneCor<- train(iphoneSentiment~., data=trainingCor, method= "C5.0", trControl = Control,preProc = c("center", "scale"))

plot(C5iPhoneCor) 

C5IC5iPhoneCor_predict<-predict(C5iPhoneCor,testingCor)

confusionMatrix(testingCor$iphoneSentiment, C5IC5iPhoneCor_predict)
# Accuracy : 0.8787         
# 95% CI : (0.846, 0.9066)
# No Information Rate : 0.8285         
# P-Value [Acc > NIR] : 0.00153        
# 
# Kappa : 0.6575         
# Mcnemar's Test P-Value : NA             


#KNN


KNN_Cor <- train(iphoneSentiment~., data=trainingCor,method= "kknn",trControl = Control,preProcess = c("center","scale"))


plot(KNN_Cor)

KNNiPhone_Corr<-predict(KNN_Cor , testingCor)

confusionMatrix(testingCor$iphoneSentiment,KNNiPhone_Corr)

# Accuracy : 0.8577          
# 95% CI : (0.8232, 0.8878)
# No Information Rate : 0.8013          
# P-Value [Acc > NIR] : 0.0008226       
# 
# Kappa : 0.6176          
# Mcnemar's Test P-Value : NA         

RandomeForest_Cor<- train(iphoneSentiment~., data=trainingCor,
                          method = "rf", 
                          trControl = Control,
                          tuneLength = 3)

plot(RandomeForest_Cor)

RandomeForest_Cor_model<-predict(RandomeForest_Cor, testingCor)

confusionMatrix(testingCor$iphoneSentiment,RandomeForest_Cor_model)

# Accuracy : 0.8808          
# 95% CI : (0.8483, 0.9084)
# No Information Rate : 0.8138          
# P-Value [Acc > NIR] : 4.992e-05       
# 
# Kappa : 0.6722          
# Mcnemar's Test P-Value : NA              


svmFitCor<- train(iphoneSentiment~., data=trainingCor, 
                   method = "svmLinear", 
                   trControl = Control, 
                   preProc = c("center", "scale"),
                   tuneLength = 8)




SVM_Cor_Predcit<-predict(svmFitCor, testingCor)

confusionMatrix(testingCor$iphoneSentiment,SVM_Cor_Predcit)

# Accuracy : 0.8724         
# 95% CI : (0.8391, 0.901)
# No Information Rate : 0.8787         
# P-Value [Acc > NIR] : 0.6927         
# 
# Kappa : 0.5986         
# Mcnemar's Test P-Value : NA    



################Models for RFE



set.seed(666)

inTrainingRFE <- createDataPartition(iphoneRFE$iphoneSentiment, p = .75, list = FALSE)
trainingRFE <- iphoneRFE[ inTraining,]
testingRFE  <- iphoneRFE[-inTraining,]



Control <- trainControl(method = "repeatedcv",repeats = 5)





C5IphoneRFE<- train(iphoneSentiment~., data=trainingRFE, method= "C5.0", trControl = Control,preProc = c("center", "scale"))

plot(C5IphoneRFE) 

C5IphoneOnlyRFE<-predict(C5IphoneRFE, testingRFE)

confusionMatrix(testingRFE$iphoneSentiment,C5IphoneOnlyRFE)
# Accuracy : 0.8808          
# 95% CI : (0.8483, 0.9084)
# No Information Rate : 0.8159          
# P-Value [Acc > NIR] : 7.868e-05       
# 
# Kappa : 0.6702          
# Mcnemar's Test P-Value : NA              


#KNN


KNN_RFE <- train(iphoneSentiment~., data=trainingRFE,method= "kknn",trControl = Control,preProcess = c("center","scale"))

plot(KNN_RFE)

KNNIphoneRFE<-predict(KNN_RFE, testingRFE)

confusionMatrix(testingRFE$iphoneSentiment,KNNIphoneRFE)

# Accuracy : 0.8536         
# 95% CI : (0.8186, 0.884)
# No Information Rate : 0.7762         
# P-Value [Acc > NIR] : 1.395e-05      
# 
# Kappa : 0.6238         
# Mcnemar's Test P-Value : NA             
  

RandomeForest_RFE <- train(iphoneSentiment~., data=trainingRFE,
                           method = "rf", 
                           trControl = Control,
                           tuneLength = 3)

plot(RandomeForest_RFE)

RandomeForest_RFE_model<-predict(RandomeForest_RFE, testingRFE)

confusionMatrix(testingRFE$iphoneSentiment,RandomeForest_RFE_model)

# Accuracy : 0.8787         
# 95% CI : (0.846, 0.9066)
# No Information Rate : 0.7971         
# P-Value [Acc > NIR] : 1.787e-06      
# 
# Kappa : 0.6765         
# Mcnemar's Test P-Value : NA     

svmFitRFE <- train(iphoneSentiment~., data=trainingRFE, 
                method = "svmLinear", 
                trControl = Control, 
                preProc = c("center", "scale"),
                tuneLength = 8)




SVM_RFE_predict<-predict(svmFitRFE, testingRFE)

confusionMatrix(testingRFE$iphoneSentiment,SVM_RFE_predict)

# Accuracy : 0.8577          
# 95% CI : (0.8232, 0.8878)
# No Information Rate : 0.8828          
# P-Value [Acc > NIR] : 0.9593          
# 
# Kappa : 0.5494          
# Mcnemar's Test P-Value : NA          


######Iphone models 

resampsAll <- resamples(list(
  RFALL=RandomeForest_All_Iphone,
  RFNCOR = RandomeForest_Cor,
  RFNZV = RandomeForest_NZV,
  RFRFE=RandomeForest_RFE,
  SVMAll=svmFit,
  SVMCor=svmFitCor,
  SVMNZV=svmFitNZV,
  SVMRFE=svmFitRFE,
  KNNALL=KKNN3,
  KNNCOR=KNN_Cor,
  KNNNZV=KNN_NZV,
  KNNRFE=KNN_RFE,
  C5All=C5Iphone3,
  C50COR=C5iPhoneCor,
  C50NZV=C5iPhoneNZV,
  C50RFE=C5IphoneRFE))

summary(resampsAll)

theme1 <- trellis.par.get()
theme1$plot.symbol$col = rgb(.2, .2, .2, .4)
theme1$plot.symbol$pch = 16
theme1$plot.line$col = rgb(1, 0, 0, .7)
theme1$plot.line$lwd <- 2
trellis.par.set(theme1)
bwplot(resampsAll, layout = c(3, 1))


trellis.par.set(theme1)
xyplot(resampsAll$metrics, what = "BlandAltman")

dotplot(resampsAll, metric = "Kappa")
dotplot(resampsAll, metric = "Accuracy")































