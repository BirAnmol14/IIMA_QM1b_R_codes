library(readr)
library( car ) # for vif
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48) #Y
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131) #X1
bmi <- weight/(height/100)^2 # a poor model, but meh! had to create an example

model <- lm(weight~height+bmi)
model # for coefficients
# (Intercept)     height          bmi  
# -132.9970       0.8507       2.4510  
# Weight =  0.8507*Height+2.4510*bmi-132.9970

vif(model) # Check for multicollineraty 
cooks.distance(model) # for influential observations, if >=1 remove the data

# To understand how good is Regression
plot(model) # for influential obs, linearity, homoscedasticity
# generates 4 plots, analyse each one for understanding regression
# use enter in console window to see each plot one by one
# PLOT 1: Residuals vs fitted: Correlation should be 0 [Linearity assumption]
# PLOT 2: Q-Q plot of residuals: Points should be close to line [normality assumption]
# PLOT 3: Residuals scatter plot: No patter in points, must be scattered [Homoscedasticity assumption]
# PLOT 4: Cook's distance for residuals: No point beyond 1 (dashed line) [Testing influential observations]

summary(model) # for detailed model summary

# To predict weight for height = 170 and bmi=22
predict_data <- data.frame(height=170,bmi=22)
predict(model,predict_data) # predicted weight = 65.547 

# Cannot plot the regression line in multivariate case on a 2D plot

# You should test for linear relationship btw independt variables to see multicollinearity
plot(height,bmi)