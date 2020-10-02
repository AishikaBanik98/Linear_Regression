power<- read.csv("..../Combined Cycle Power Plant Dataset.csv")    #importing the dataset
head(power)                       #this command shows the first six rows of the dataset
summary(power)       #this command gives the summary of the dataset
str(power)           #this command gives the structure of the dataset
any(is.na(power))    #to check whether there are any null values in the dataset

#The following libraries are used for Exploratory Data Analysis
library(ggplot2)
library(ggthemes)
library(dplyr)



#Grabbing only the numeric columns as we can't see correlation between the categorical variables
numeric.cols<- sapply(power,is.numeric)

#Filtering to numeric columns for correlation
correlation.data<-cor(power[ , numeric.cols])
correlation.data

#For proper data visualization we use the 'corrgram' package and the 'corrplot' package
library(corrgram)
library(corrplot)

#Now we perform the correlation plot and see what we can infer from that
corrplot(correlation.data, method='color') 

#Now we perform the correlogram and see what we can infer from that
corrgram(power,order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

#Histogram of the net hourly electrical energy output (PE)
ggplot(power,aes(x=PE))+ geom_histogram(bins=20, alpha=0.5, fill='green') + theme_minimal()

#We need to split our data into a training set and a testing set in order to test our accuracy, so we can do this using the caTools library
library(caTools)

#Spliting up the sample for training and testing and assigns boolean values to a new column
sample <- sample.split(power$PE, SplitRatio = 0.70)

#Training data
train_data = subset(power, sample == TRUE)

#Testing data
test_data= subset(power, sample == FALSE)
#Training the model
model <- lm(PE ~ (AT+AP+RH+V), train_data)  

summary(model)

#Visualizing the model

res<- residuals(model)      #Grabbing the residuals
res<- as.data.frame(res)    #Converting the residuals to dataframe for ggplot
head(res)

#Histogram of residuals
ggplot(res, aes(res)) + geom_histogram(fill='green', alpha=0.5)

#Predictions
PE.predictions <- predict(model,test_data)

results<- cbind(PE.predictions,test_data$PE)
colnames(results) <- c('predicted','real')
results<- as.data.frame(results)
head(results)

#To remove negative predictions and replace it with 0
to_zero <- function(x){
  if(x <0) {
    return(0)
  }else{
    return(x)
  }
}

results$predicted <- sapply(results$predicted,to_zero)
head(results)

#Evaluating the prediction values by the method of MSE(Mean Squared Error)
mse <- mean((results$real - results$predicted)^2)
print(mse)

#Evaluating the prediction values by the method of RMSE(Root Mean Squared Error)
mse^0.5 

#Or we can just use the R-Squared Value for the model which gives the accuracy of the model
SSE <- sum((results$predicted - results$real)^2)
TSS <- sum( (mean(power$PE) - results$real)^2)
R2 <- 1-SSE/TSS
R2             


#Training the model
model <- lm(PE ~ AP, train_data)  #here PE is the dependent variable and AP is the independent variable

summary(model)

#Visualizing the model

res<- residuals(model)      #Grabbing the residuals
res<- as.data.frame(res)    #Converting the residuals to dataframe for ggplot
head(res)

#Histogram of residuals
ggplot(res, aes(res)) + geom_histogram(fill='green', alpha=0.5)

#Predictions
PE.predictions <- predict(model,test_data)
head(PE.predictions)

results<- cbind(PE.predictions,test_data$PE)
colnames(results) <- c('predicted','real')
results<- as.data.frame(results)
head(results)

#To remove negative predictions and replace it with 0
to_zero <- function(x){
  if(x <0) {
    return(0)
  }else{
    return(x)
  }
}

results$predicted <- sapply(results$predicted,to_zero)
head(results)

#Evaluating the prediction values by the method of MSE(Mean Squared Error)
mse <- mean((results$real - results$predicted)^2)
print(mse)

#Evaluating the prediction values by the method of RMSE(Root Mean Squared Error)
mse^0.5 

#Or we can just use the R-Squared Value for the model which gives the accuracy of the model
SSE <- sum((results$predicted - results$real)^2)
TSS <- sum( (mean(power$PE) - results$real)^2)
R2 <- 1-SSE/TSS
R2       