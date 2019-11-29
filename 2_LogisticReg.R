'05/10/2019 - Saturday at Cattolica

Application churn prevention
Lets begin with some basics. Remember the customer churn case from the video. 
What is the main incentive for an online shop to do churn prevention?

Convincing existing customers to buy again and stay 
loyal to the online shop
Churn prevention is a measure to ensure that customers 
visit the online shop again.'

setwd("~/Carlos/THESIS/Mkt Analytics")
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(rms)
library(MASS)
library(SDMTools)


defaultData <- read.csv("~/3. DATACAMP/Marketing_Analytics/defaultData.txt", 
                        sep=";")

'This dataset is about bank customers and will be
used to predict if customers will default on their loan payments.'

'Data discovery'
# Summary of data
summary(defaultData)

# Look at data structure
str(defaultData)

# Analyze the balancedness of dependent variable
ggplot(defaultData,aes(x = PaymentDefault)) +
  geom_histogram(stat = "count") 

'Peculiarities of the dependent variable
how you can transform the outcome variable (taking only values of 0 and 1)
in order to examine a linear influence of the explanatory variables?

By using the logarithmic odds to transform it to a range from - ??? to + ???
'


'Model specification and estimation
Use the glm() function in order to model the probability that a
customer will default on his payment by using a logistic regression.
'

'Model specification and estimation'

# Build logistic regression model
#glm() stands for generalized linear model and offers a whole family of regression models.
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                        age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                        billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                        payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                      family = binomial, data = defaultData)

# Take a look at the model
summary(logitModelFull)

# Take a look at the odds
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp

'Statistical significance
Everyone is talking about statistical significance, 
but do you know the exact meaning of it? 
What is the correct interpretation of a p value equal to 0.05 
for a variables coefficient, when we have the following null hypothesis:
  
  
H0: The influence of this variable on the payment default 
of a customer is equal to zero.

The probability of finding this coefficients value is only 5%, 
given that our null hypothesis (the respective coefficient is equal to zero) is true.
 That is the right interpretation of a p value.
'


'Model specification
The stepAIC() function gives back a reduced model'
'Model specification'
library(MASS)


#Build the new model 
#Set trace = 0, as you do not want to get an output for the whole model selection process.
logitModelNew <- stepAIC(logitModelFull, trace = 0) 

#Look at the model
summary(logitModelNew) 

# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit


'In-sample fit full model'

# Make predictions using the full Model
str(defaultData)
defaultData$predFull <- predict(logitModelFull, 
                                type = "response", 
                                na.action = na.exclude)

# Construct the in-sample confusion matrix
confMatrixModelFull <- confusion.matrix(defaultData$PaymentDefault,
                                        defaultData$predFull, 
                                        threshold = 0.5)
confMatrixModelFull

# Calculate the accuracy for the full Model
accuracyFull <- sum(diag(confMatrixModelFull)) / sum(confMatrixModelFull)
accuracyFull

'But you have only calculated the accuracy for one of your two models. 
In the next step, do it for the other one as well!
'


#27/10/2019 Sunday

'In-sample fit restricted model'
# Calculate the accuracy for 'logitModelNew'
# Make prediction
defaultData$predNew <- predict(logitModelNew, 
                               type = "response", 
                               na.action = na.exclude)

# Construct the in-sample confusion matrix
confMatrixModelNew <- confusion.matrix(defaultData$PaymentDefault,
                                       defaultData$predNew, 
                                       threshold =0.5)
confMatrixModelNew

# Calculate the accuracy...
accuracyNew <- sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew)
accuracyNew

# and compare it to the full model's accuracy
accuracyFull

'ou calculated the accuracy measures for both model candidates. 
As the accuracy values are approximately the same, 
lets continue with the smaller model logitModelNew'

'Finding the optimal threshold'
library(SDMTools)
# Prepare data frame with threshold values and empty payoff column
payoffMatrix <- data.frame(threshold = seq(from = 0.1, to = 0.5, by = 0.1),
                           payoff = NA) 
payoffMatrix 

for(i in 1:length(payoffMatrix$threshold)) {
  # Calculate confusion matrix with varying threshold
  confMatrix <- confusion.matrix(defaultData$PaymentDefault,
                                 defaultData$predNew, 
                                 threshold = payoffMatrix$threshold[i])
  # Calculate payoff and save it to the corresponding row
  payoffMatrix$payoff[i] <- confMatrix[2,2]*1000 + confMatrix[2, 1]*(-250)
}
confMatrix
payoffMatrix

'That wa not easy
You could see that the optimal threshold is 0.3.'


'Danger of overfitting
The previous videos gave you some insights about 
in-sample fitting and the problem of overfitting. 
Tell me, what is the main reason of overfitting?

The model is highly tailored to the given data and 
not suited for explaining new data.'

'Assessing out-of-sample model fit'
# Split data in train and test set
set.seed(534381) 

defaultData$isTrain <- rbinom(nrow(defaultData), 1, 0.66)
train <- subset(defaultData, defaultData$isTrain == 1)
test <- subset(defaultData, defaultData$isTrain  == 0)

logitTrainNew <- glm(formulaLogit, 
                     family = binomial, 
                     data = train) # Modeling

test$predNew <- predict(logitTrainNew, 
                        type = "response", 
                        newdata = test) # Predictions

# Out-of-sample confusion matrix and accuracy
confMatrixModelNew <- confusion.matrix(test$PaymentDefault, 
                                       test$predNew, 
                                       threshold = 0.3) 

sum(diag(confMatrixModelNew)) / sum(confMatrixModelNew) 
# Compare this value to the in-sample accuracy
'Knowing how to validate in an out-of-sample manner is essential 
for reliable results! 
In case you experience overfitting in the future you would have to 
go back to modeling and build smaller models.
'


'Cross validation: is a clever method to avoid overfitting as you could see.'
library(boot)
# Accuracy function
costAcc <- function(r, pi = 0) {
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm)) / sum(cm)
  return(acc)
}

# Cross validated accuracy for logitModelNew
set.seed(534381)
cv.glm(defaultData, logitModelNew, cost = costAcc, K = 6)$delta[1]

