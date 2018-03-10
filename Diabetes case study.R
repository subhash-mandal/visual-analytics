
#import the dataset
getwd()
setwd("C:/Subhash/personal/training/Business Analytics/R Studio")
diabetes <- read.csv("C:/Subhash/personal/training/Business Analytics/R progamming/Diabetes.csv")



#find the shape of dataset
dim(diabetes)
#view first 4 rows
head(diabetes,4)
#find datatype of all variables
str(diabetes)


library(plyr)
library(ggplot2)  # required for plotting graph

#use rename function from plyr package
diabetes <- rename(diabetes,c("Pregnancies"="pregnancy","Glucose"="glucose","BloodPressure"="bp","SkinThickness"="skin","Insulin"="insulin","BMI"="bmi", "DiabetesPedigreeFunction"="pedigree","Age"="age","Outcome"="outcome"))

print(diabetes)

#missing value detection
summary(diabetes)

#other opttion to identify missing value is sapply
diabetes.mv<-sapply(diabetes,function(x)sum(is.na(x)))
diabetes.mv

#missing value replacement

diabetes$bp<-replace(diabetes$bp,is.na(diabetes$bp),median(diabetes$bp,na.rm = TRUE))
diabetes$glucose<-replace(diabetes$glucose,is.na(diabetes$glucose),median(diabetes$glucose,na.rm = TRUE))
diabetes$pedigree<-replace(diabetes$pedigree,is.na(diabetes$pedigree),median(diabetes$pedigree,na.rm = TRUE))
diabetes$age<-replace(diabetes$age,is.na(diabetes$age),median(diabetes$age,na.rm = TRUE))
diabetes$outcome<-as.factor(diabetes$outcome)


#draw histrogram to see number of zero values
hist(diabetes$pregnancy)
hist(diabetes$glucose)
hist(diabetes$bp)
hist(diabetes$skin)
hist(diabetes$insulin)
hist(diabetes$bmi)
hist(diabetes$pedigree)
hist(diabetes$age)

#frequency of zero values
table(diabetes$glucose)

#zero value replacement
diabetes$glucose <- replace(diabetes$glucose, diabetes$glucose==0, median(diabetes$glucose, na.rm = TRUE))
table(diabetes$glucose)
table(diabetes$bp)
diabetes$bp<-replace(diabetes$bp,diabetes$bp==0,median(diabetes$bp,na.rm = TRUE))
table(diabetes$skin)
diabetes$skin<-replace(diabetes$skin,diabetes$skin==0,median(diabetes$skin,na.rm = TRUE))
table(diabetes$insulin)
diabetes$insulin<-replace(diabetes$insulin,diabetes$insulin==0,median(diabetes$insulin,na.rm = TRUE))
table(diabetes$bmi)
diabetes$bmi<-replace(diabetes$bmi,diabetes$bmi==0,median(diabetes$bmi,na.rm = TRUE))

table(diabetes$pedigree) # no zero values found
table(diabetes$age)# no zero values found

#find minimum value for each variable

min(diabetes$pregnancy)
min(diabetes$glucose)
min(diabetes$bp)
min(diabetes$skin)
min(diabetes$insulin)
min(diabetes$bmi)
min(diabetes$pedigree)
min(diabetes$age)

#finding quartile for the variable

print(quantile(diabetes$glucose))
print(quantile(diabetes$bp))
print(quantile(diabetes$insulin))
print(quantile(diabetes$bmi))
print(quantile(diabetes$age))

#create box plot
#minimum value in boxplot , Lower fence =Q1-1.5(IQR)
#max value in boxplot, Upper fence = Q3 + 1.5 (IQR)

ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$bp))+geom_boxplot()

## We check the presence of any outlier in pregnancy with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$pregnancy))+geom_boxplot()
## We check the presence of any outlier in glucose with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$glucose))+geom_boxplot()
## We check the presence of any outlier in skin with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$skin))+geom_boxplot()
## We check the presence of any outlier in insulin with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$insulin))+geom_boxplot()
## We check the presence of any outlier in bmi with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$bmi))+geom_boxplot()
## We check the presence of any outlier in pedigree with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$pedigree))+geom_boxplot()
## We check the presence of any outlier in age with respect to response variable(outcome).
ggplot(diabetes,aes(x=diabetes$outcome,y=diabetes$age))+geom_boxplot()

#scatter plot,pair plot
windows(7,7)+pairs(diabetes[,-9])

#visualize variableto check normal distribution
qqnorm(diabetes$age)
qqline(diabetes$age)
qqnorm(diabetes$glucose)
qqline(diabetes$glucose)

#normalisation or standardization of data. rescaling dataset into one scale
scaled_data <- scale(diabetes[,-9])

#Print the mean and standard deviation
print(mean(scaled_data))
print(sd(scaled_data))

#append outcome varibable to scaled data
scaled_data<-data.frame(scaled_data,diabetes$outcome)
head(scaled_data)

#histogram
hist(scaled_data$glucose)

mean(scaled_data$glucose)
sd(scaled_data$glucose)

# with above code the data preprocessing is complete
#now need to split the data into training and test dataset

library(caret)
Training_testing <- createDataPartition(scaled_data$diabetes.outcome, p = .75, list = FALSE) #partion of dataset is done using outcome variable so that equal valid values go into training and testing

training <- diabetes[Training_testing,]  # first set of 75% of rows into training set
testing <- diabetes[-Training_testing,] #remaining set of data into test
dim(training)
dim(testing)
head(training)

#perform 10 fold cross validation (no of validations depends on the no of values in dataset. needs more practice and intution)
fitControl <- trainControl(method = "repeatedcv",number = 10, repeats = 10)


#train naivebayes model
library(naivebayes)
NBFit <- train(outcome~ ., data = training, method = "naive_bayes",trControl = fitControl)
print(NBFit)

#predicting our model on the test set
NB_predict<-predict(NBFit,testing[,-9]) #remove the outcome from test dataset

confusionMatrix(NB_predict,testing$outcome)
table(testing$outcome)
