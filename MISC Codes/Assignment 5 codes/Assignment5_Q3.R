library(readr)
data <- read.csv("./A5Q3.csv",header=TRUE)
#View(data)
age <- data$Age
fat <- data$Fat.

#a) Regression: predictor - age, response - fat%

# plot to check is linear relationship present
plot(age,fat,col = "blue",main = "Age vs Fat% Regression")
model <- lm(fat~age)
abline(reg=model) # plot the regression line

model # for coefficients
# (Intercept)     Age  
#  20.1116       0.2401  
# Fat% = 0.2401*Age+20.1116

#b)
summary(model) # for detailed model summary
# R squared: 0.2566 (Poor estimate, not good for prediction)
# P-value: 0.06451 > 0.05, we cannot conclude that the predictor variable age affects the response variable fat %

#c)
# The linear model we tried to build in order to predict fat%(response variable) a function of age(predictor variable),
# we found that a linear relationship could be established by looking at the dataset.
# The relationship that was found was Fat% = 0.2401*Age+20.1116 by regression.
# The model was built with data age range in 23 to 61 and thus if limiting the model's prediction capacity to this age range
# 0.2401 (coefficient of age) is the change in the average fat% for a unit change in age
# The model created had a low R squared of 0.2566 which indicates low prediction power of the model.
# The p-value was also > 0.05 and thus we were unable to conclude that age affects the response variable fat %
