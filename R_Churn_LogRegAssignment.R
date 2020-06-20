#Churn Assignment
library(dplyr)
library(caret)
install.packages("e1071")
library(e1071)
  
#DATA UNDERSTANDING AND PREPARATION
#import and understand dataset
df = read.csv("C:/Users/Samuel Oluwatoba/Documents/Stat P1/Stat P1/Telco-Customer-Churn.csv")
head(df)
ls(df)

#check for NA and replace withs means if its exist
sapply(df, function(x) sum(is.na(x)))
df$TotalCharges[is.na(df$TotalCharges)] = mean(df$TotalCharges, na.rm=TRUE)
table(is.na(df))

#dataset preparation
head(df)
str(df)
dfx = select(df, -customerID)
summary(dfx)
dfx$Churn = as.factor(dfx$Churn)
unique(df$SeniorCitizen)
dfx$SeniorCitizen = as.factor(dfx$SeniorCitizen)
str(dfx)
unique(Churn)

#MODELLING
#partition to train and test
trainindex = createDataPartition(dfx$Churn, p=0.7, list=FALSE)
dfxTrain= dfx[trainindex, ]
dfxTest = dfx[-trainindex, ]

#train and understand model
logmod = glm(Churn~., data=dfxTrain, family ="binomial")
summary(logmod)


#make predictions on trained model
pred = predict(logmod, newdata=dfxTest, type="response" )
dfxTest$predictions = pred
View(dfxTest)

#EVALUATION
dfxTest$predictions = ifelse(pred>0.5, 1, 0)
str(dfxTest$predictions)
dfxTest$predictions = as.factor(dfxTest$predictions)
confusionMatrix(dfxTest$Churn, dfxTest$predictions)

#SIGNIFICANCE OF MODEL: 
#On a C.I of 95% and Accuracy of 80.12%, 

#1. Based on its p-values, the following factors have;
#No effect on Customer Churn: Gender, SeniorCitizen, Partner, Dependents, phoneservice, multiplelines,OnlineSecurity, internetService, MonthlyCharges 
#"OnlineBackup"     "DeviceProtection" "TechSupport", "StreamingTV"      "StreamingMovies"
#little: MultipleLinesYes, InternetServiceFiber optic, InternetServiceNo, 
#Somewhat: PaperlessBillingYes
#Strong: #Tenure, ContractOneYear, ContractTwoYear, PaymentMethodElectronic check, TotalCharges 

