install.packages("mlbench")
library(mlbench)

data(Glass)
head(Glass)         #To see the first six rows of the dataset
summary(Glass)      #To see the summary of the dataset
str(Glass)          #To see the structure of the dataset
any(is.na(Glass))   #To check whether there are any null values in the dataset

#The following libraries are used for Exploratory Data Analysis
library(ggplot2)
library(ggthemes)
library(dplyr)

#Grabbing only the numeric columns as we can't see correlation between the categorical variables
num.cols<- sapply(Glass,is.numeric)
#Filtering to numeric columns for correlation
cor.data<-cor(Glass[ , num.cols])
round(cor.data,2)

#For proper data visualization we use the 'corrgram' package and the 'corrplot' package
library(corrgram)
library(corrplot)
#Now we perform the correlation plot and see what we can infer from that
corrplot(cor.data, method='color') 

#Now we perform the correlogram and see what we can infer from that
corrgram(Glass ,order=TRUE, lower.panel=panel.shade, upper.panel=panel.pie, text.panel=panel.txt)

#Histogram of the amount of Ca
ggplot(Glass,aes(x=Ca))+ geom_histogram(bins=20, alpha=0.5, fill='pink') + theme_minimal()

#We need to split our data into a training set and a testing set in order to test our accuracy, so we can do this using the caTools library
library(caTools)

#Spliting up the sample for training and testing and assigns boolean values to a new column
sample <- sample.split(Glass$Ca, SplitRatio = 0.70)

#Training data
train_data = subset(Glass, sample == TRUE)

#Testing data
test_data= subset(Glass, sample == FALSE)

#Training the model
model <- lm(Ca ~ RI, train_data)

summary(model)

#Visualizing the model

res<- residuals(model)      #Grabbing the residuals
res<- as.data.frame(res)    #Converting the residuals to dataframe for ggplot
head(res)

#Histogram of residuals
ggplot(res, aes(res)) + geom_histogram(fill='pink', alpha=0.5)

#Predictions
Ca.predictions <- predict(model,test_data)

results<- cbind(Ca.predictions,test_data$Ca)
colnames(results) <- c('pred','real')
results<- as.data.frame(results)
results

#To remove negative predictions and replace it with 0
to_zero <- function(x){
  if(x <0) {
    return(0)
  }else{
    return(x)
  }
}

results$pred <- sapply(results$pred,to_zero)
results

#Evaluating the prediction values by the method of MSE(Mean Squared Error)
mse <- mean((results$real - results$pred)^2)
print(mse)

#Evaluating the prediction values by the method of RMSE(Root Mean Squared Error)
mse^0.5 

#Or we can just use the R-Squared Value for the model which gives the accuracy of the model
SSE <- sum((results$pred - results$real)^2)      #Sum Square of Errors
TSS <- sum( (mean(Glass$Ca) - results$real)^2)   #Total Sum of Squares
R2 <- 1-SSE/TSS                                  #R-Squared
R2 