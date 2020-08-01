#Let's look at data to be used for training model
summary(Dataset)
combined<-Dataset

#Removing irrelevant Variables
combined<-combined[-c(1,2,3,7,10,11,14,15,17,19,20)]

#Converting variables into factor
combined$gender<-as.factor(combined$gender)
combined$job_industry_category<-as.factor(combined$job_industry_category)
combined$wealth_segment<-as.factor(combined$wealth_segment)
combined$state<-as.factor(combined$state)
combined$owns_car<-as.factor(combined$owns_car)
summary(combined)
combined<-na.omit(combined)

#Target
profit <- data.frame(combined$Profit)
combined<- combined[,-10]

#Extracting numeric variables
num<-unlist(lapply(combined, is.numeric))

#Function to normalize numeric variables
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
norm <- as.data.frame(lapply(combined[,num], normalize))

library(psych)

#Coverting Factors to dummycode
DataFac<-as.data.frame(sapply(combined[ ,!num], dummy.code))

#Combine all variables
Data.Final <- data.frame(DataFac,norm)           
Data.Final['gender.U']=0  
set.seed(1234)

#Splitting data into 70% train and 30% test
ind = sample(2, nrow(Data.Final), replace=TRUE, prob=c(0.7, 0.3))
training = Data.Final[ind==1,]
test = Data.Final[ind==2,]
trainLabels = profit[ind==1, ]
testLabels = profit[ind==2, ]

#Numeric Target K-NN model
library(FNN)

rmse = function(actual, predicted) 
  {sqrt(mean((actual - predicted) ^ 2))}

make_knn_pred = function(k = 1, training, test,trainLabels)
  {
  model <- knn.reg(train = training , test = test, y = trainLabels, k=k ) 
  # here y is response variable in training data
  pred <- model$pred
  rmse(predicted = pred, actual = testLabels)
  }

k = c(1, 5, 10, 15, 25, 50, 250)
knn_trn_rmse = sapply(k, make_knn_pred, training = training, test = test, trainLabels = trainLabels)
knn_trn_rmse
#Optimum K-value is 25 with less Root Mean Square error(1808.903) and less complexity.

###################################################################################

#Let's look at the new customers list
summary(newcust)

#Removing irrelevant Variables
newcust<-newcust[-c(1,2,6,9,12,13,15,17,18,19,20,21,22,23)]

#Converting variables into factor
newcust$gender<-as.factor(newcust$gender)
newcust$job_industry_category<-as.factor(newcust$job_industry_category)
newcust$wealth_segment<-as.factor(newcust$wealth_segment)
newcust$state<-as.factor(newcust$state)
newcust$owns_car<-as.factor(newcust$owns_car)
newcust$past_3_years_bike_related_purchases<-as.numeric(newcust$past_3_years_bike_related_purchases)
newcust$property_valuation<-as.numeric(newcust$property_valuation)
summary(newcust)

#Extracting numeric variables
num<-unlist(lapply(newcust, is.numeric))

#Function to normalize numeric variables
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}
norm <- as.data.frame(lapply(newcust[,num], normalize))

library(psych)

#Converting Factors to dummy code
DataFac<-as.data.frame(sapply(newcust[ ,!num], dummy.code))

#Combine all variables
New.Final <- data.frame(DataFac,norm)           
New.Final['state.Victoria']=0
New.Final['state.New.South.Wales']=0

#K-NN Model Development
model <- knn.reg(train = Data.Final , test = New.Final, y =profit, k=25) 

#Predictions
Predictions<-model$pred
New.Final<-cbind(New.Final,Predictions)

#Linear Regression Model Development
Data_reg<-data.frame(cbind(Data.Final, profit))
lmModel <- lm(Data_reg$combined.Profit ~ ., data = Data_reg)
summary(lmModel)

#Predictions
pred <- predict(lmModel, newdata = New.Final)

#write.csv(Predictions,"K-NN.csv")
#write.csv(pred,"Regression.csv")









