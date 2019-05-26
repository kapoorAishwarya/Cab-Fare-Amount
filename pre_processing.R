#setting the working directory
setwd("C:/Users/Lenovo/Desktop/Edwisor/Project 2")
  
#Loading the library
library(DMwR)
library(rpart)
install.packages("ggplot")
library(ggplot2)
library(corrgram)
install.packages("ggpubr")
library(ggpubr)
install.packages("devtools")
install.packages("dpylr")
install.packages("C50")
library(C50)
library(MASS)

#reading the data before the clearing the R enviorment
rm(list = ls(all=T))
read.csv("train_cab.csv", header= T, na.strings = c("", " ", "NA"))-> cab
read.csv("test.csv", header = T, na.strings = c(" ", "", "NA")) -> cab_pre

#converting into Numeric
cab[] <- lapply(cab[], as.numeric)
cab_pre[] <- lapply(cab_pre[], as.numeric)

#creating some basic graphs 
qqplot(cab)
hist(cab$fare_amount, col = "green")
hist(cab$pickup_longitude, col = "blue")
hist(cab$pickup_latitude, col = "red")
hist(cab$dropoff_longitude, col = "orange")
hist(cab$dropoff_latitude, col = "yellow")
hist(cab$passenger_count, col = "grey")

#Checking all the columns name
colnames(cab)
View(cab)

#Summary of the data
summary(cab)

#checking the count of all the na's in data
#creating dataframe only for missing values
sum(is.na(cab))
names(cab)
miss_values = data.frame(apply(cab, 2, function(x) {sum(is.na(x))}))
data.frame(miss_values)
class(miss_values)
miss_values$columns = row.names(miss_values)
row.names(miss_values) = NULL
names(miss_values)[1] = "percentage"
names(miss_values)[2] = "Variable"


miss_values$percentage = (miss_values$percentage/ nrow(cab))*100
miss_values = miss_values[order(-miss_values$percentage),]
View(miss_values)
miss_values[1:2,] -> miss
miss

#Plotting the bar graph for the Missing variables
ggplot(miss, aes(x = Variable, y = percentage)) + geom_col() 
  + xlab = ("variable") + ylab = ("Percentage") +  main = ("Missing_data") + col = ("blue")


#dropping the Date variable as well
cab <- cab[,-2, drop = FALSE]

head(cab)

#treating the NA
#lets create the missing value for one point 
cab[6,1]
cab[6,1] = NA

cab[6,6]
cab[6,6] = NA
#Actual Value = 26 / 1
#Mean method value = 269.4907 / 2.625
#Median method value = 341 /1
#knn IMpoutation value = 393.625 / 1.0000

# 1st will use the central tendancy in which we will use "MEAN" because it have only numeric data
cab$fare_amount[is.na(cab$fare_amount)] = mean(cab$fare_amount, na.rm = T)
cab$passenger_count[is.na(cab$passenger_count)] = mean(cab$passenger_count, na.rm = T)

#Median method
cab$fare_amount[is.na(cab$fare_amount)] = median(cab$fare_amount, na.rm = T)
cab$passenger_count[is.na(cab$passenger_count)] = median(cab$passenger_count, na.rm = T)

#Knn - imputaation 
cab$fare_amount =knnImputation(cab, k= 5)
cab$passenger_count =knnImputation(cab, k= 5)

#We are going to use the mean method for the missing value imputation

#Outliers
#we use using the univariate method to check the outliers in the following variables 
boxplot(cab$fare_amount,  main = "Fare_Amount", xlab = "numbers", ylab = "counts", col = "orange", horizontal = FALSE, notch = TRUE)
boxplot(cab$pickup_longitude, main = "Pickup_longitude", xlab = "numbers", ylab = "counts", col = "orange", horizontal = FALSE, notch = TRUE)
boxplot(cab$pickup_latitude, main = "Pickup_latitude", xlab = "numbers", ylab = "counts", col = "orange", horizontal = FALSE, notch = TRUE)
boxplot(cab$dropoff_longitude, main = "Dropoff_longitude", xlab = "numbers", ylab = "counts", col = "orange", horizontal = FALSE, notch = TRUE)
boxplot(cab$dropoff_latitude, main = "Dropoff_longitude", xlab = "numbers", ylab = "counts", col = "orange", horizontal = FALSE, notch = TRUE)
boxplot(cab$passenger_count, main = "passenger_count", xlab = "numbers", ylab = "counts", col = "pink", horizontal = FALSE, notch = TRUE)

#decting the outlires
outlier1 = cab$pickup_longitude[cab$pickup_longitude %in% boxplot.stats(cab$previous) $out]
outlier1

outlier2 = cab$pickup_latitude[cab$pickup_latitude %in% boxplot.stats(cab$previous) $out]
outlier2


outlier3 = cab$dropoff_longitude[cab$dropoff_longitude %in% boxplot.stats(cab$previous) $out]
outlier3

outlier4 = cab$dropoff_latitude[cab$dropoff_latitude %in% boxplot.stats(cab$previous) $out]
outlier4

outlier5 = cab$passenger_count[cab$passenger_count %in% boxplot.stats(cab$previous) $out]
outlier5

#removing the outlier from the data

cab = cab[which(!cab$pickup_longitude %in% outlier1), ]
cab = cab[which(!cab$pickup_latitude %in% outlier2), ]
cab = cab[which(!cab$dropoff_longitude %in% outlier3), ]
cab = cab[which(!cab$dropoff_latitude %in% outlier4), ]
cab = cab[which(!cab$passenger_count %in% outlier5), ]

# As, outliers are dected as NULL

#Feature selection
#we are checking the correlation between the varaibles 
library(corrgram)
corrgram(cab, order = F, upper.panel = panel.pie, text.panel = panel.txt, main = "Plot")

#using this plot wee can study that pickup_longitude is positively correlated 
#with dropoff_longitude and negatively correlated with pickup_latitude and also
#pickup_latitude is negatively correlated witb dropoff_longitude and positively correlated with
#dropoff_latitude. so we can drop the pickup_latitude and pickup_longitude variables.

#dropping off the variables
cab = subset(cab, select = -c(pickup_longitude, dropoff_latitude, passenger_count))
head(cab)

#changing dropoff to latitude
#Scatter plot
ggqqplot(cab$fare_amount, ylab = "FARE")
ggqqplot(cab$pickup_longitude, ylab = "pickup_longitude")
ggqqplot(cab$pickup_latitude, ylab = "dropoff_latitude")
ggqqplot(cab$dropoff_longitude, ylab = "dropoff_longitude")
ggqqplot(cab$dropoff_latitude, ylab = "dropoff_latitude")

#Feature Scaling 

#Normalization and standardization 
#checcking the normality of the data 
qqnorm(cab$dropoff_longitude)
hist(cab$fare_amount)
hist(cab$dropoff_latitude)
hist(cab$pickup_latitude)
#this data is not normally distributed, we will use Normalization to scale the data
#normalization method
cnames = c("fare_amount", "pickup_latitude", "dropoff_longitude")
for (i in cnames) {
  cab [,i] = (cab[,i] - min(cab[,i]))/ (max(cab[,i] - min(cab[,i])))
  
}

head(cab)

#Building model
#Decision Tree
#creating smaple for the data using simple sample method because all variables are continous
#we are using only Train data for our testing of data will use Test data later

#train_index = sample(1:nrow(cab), 0.8*nrow(cab))
#train_data = cab[train_index,]
#test_data = cab[-train_index,]

train_data = cab
test_data = cab_pre

#Decision Tree
library(rpart)
fit = rpart(fare_amount ~., data = train_data, method = "anova")
fit
predictons = predict(fit, test_data)
predictons



#Error matrix
#MAPE amount of error we get

mape = function(y, yhat) {
  mean(abs((y - yhat)/y))*100
}

mape(test_data[,1], predictons)

regr.eval(test_data[,1], predictons, stats = c("mae", "mape", "rmse", "mse"))

#MAPE for Decision tree is 99.80% 
#MAPE for liner model is 98.19%

#Liner regression model
install.packages("usdm")
library(usdm)
vif(cab[,-1])
indep_vars = c("pickup_latitude", "dropoff_longitude")

vifcor(subset(cab, select = (indep_vars)), th=0.9)

lm_model = lm(fare_amount ~., data = train_data)

summary(lm_model)


prediction_lm_model =  predict(lm_model, test_data)

mape(test_data[,1], prediction_lm_model)

regr.eval(test_data[,1], predictons, stats = c("mae", "mape", "mae", "mse"))

