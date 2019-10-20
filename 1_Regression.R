'05/10/19 - Saturday at Cattolica

Marketing Analytics in R'

'Benefits of knowing CLV
What are some benefits of knowing the customer lifetime value (CLV) of a customer?
Identification of promising customers
Minimization of acquisition costs by specific targeting of customers
Efficient organization of CRM by prioritizing customers


Customer relationship management (CRM) is the combination of practices, 
strategies and technologies that companies use to manage and analyze 
customer interactions and data throughout the customer lifecycle, 
with the goal of improving customer service relationships and 
assisting in customer retention and driving sales
'

setwd("~/3. DATACAMP/Marketing_Analytics")
library(readr)
library(dplyr)
library(corrplot)
library(ggplot2)
library(rms)


salesData <- read.csv("~/3. DATACAMP/Marketing_Analytics/salesData.txt")

# Structure of dataset
str(salesData, give.attr = FALSE)

'Variable	Description
id	            identification number of customer
mostFreqStore	  store person bought mostly from
mostFreqCat	    category person purchased mostly
nCats	          number of different categories
preferredBrand	brand person purchased mostly
nBrands	        number of different brands'

# Visualization of correlations

salesData %>% select_if(is.numeric) %>%
  select(-id) %>%
  cor() %>%
  corrplot()

# Frequent stores
ggplot(salesData) +
  geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))

# Preferred brand
ggplot(salesData) +
  geom_boxplot(aes(x = preferredBrand, y = salesThisMon))

'Now interpret the correlation- and the boxplots: 
Which variables are probably well suited to explain the 
sales of this month? Got an idea? 
Lets move on to the simple linear regression.
'


'Understanding residuals
Which of the below is true?
The residuals are the differences between the predicted and the actual values
The residuals are also called prediction errors
The residuals in a linear model should be uncorrelated
'
#Estimating simple linear regression
# Model specification using lm
salesSimpleModel <- lm(salesThisMon ~ salesLast3Mon, 
                       data = salesData)

# Looking at model summary
summary(salesSimpleModel)
'Since the regression coefficient is greater than 0,
there exists a positive relationship between the explanatory 
variable salesLast3Mon and the dependent variable salesThisMon. 
It explains almost 60 percent of the variation in the sales of this month.'


'Avoiding multicollinearity'
# Estimating the full model,  
#using all variables but the id in order to explain the sales in this month. 
salesModel1 <- lm(salesThisMon ~ . - id, 
                  data = salesData)

# Estimate the variance inflation factors using the vif() function from the rms package.
vif(salesModel1)

# Estimating new model by removing information on brand 
#in order to avoid multicollinearity
salesModel2 <- lm(salesThisMon ~ . - preferredBrand - nBrands -id, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel2)
'Since none of the variance inflation factors is greater than 10 
we can certainly accept the second model. 
So lets move on to the interpretation of the coefficients.'

'Take a look at the line for the meanItemPrice. 
If we assume a level of significance of 0.05, which of the following is true? 
The effect of the mean item price on the sales this month is 
statistically significant. A one-unit increase in the mean item
price leads to a 0.23 Euro increase in the sales of this month.
'

'Future predictions of sales
It contains information on the customers for the months two to four. 
We want to use this information in order to predict the sales for month 5.'

salesData2_4 <- read.csv("~/3. DATACAMP/Marketing_Analytics/salesDataMon2To4.txt")

# getting an overview of new data
summary(salesData2_4)

# predicting sales
predSales5 <- predict(salesModel2, newdata = salesData2_4)

# calculating mean of future sales
mean(predSales5)



' Now you are able to predict future sales. 
What a mighty tool linear regression is!'






