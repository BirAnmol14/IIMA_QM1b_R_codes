library(readr)
height <- c(151, 174, 138, 186, 128, 136, 179, 163, 152, 131) #X
weight <- c(63, 81, 56, 91, 47, 57, 76, 72, 62, 48) #Y

model <- lm(weight~height) 
model # for coefficients
# (Intercept)       height  
#  -38.4551         0.6746  
# Weight = 0.6746*Height-38.4551

summary(model) # for detailed model summary

# To predict weight f         or height = 170
predict_data <- data.frame(height=170)
predict(model,predict_data) # predicted weight = 76.22869

# to plot
plot(height,weight,col = "blue",main = "Height & Weight Regression")
abline(reg=model)

# To get detailed plots for analysis Regression model
plot(model)

# generates 4 plots, analyse each one for understanding regression
# use enter in console window to see each plot one by one

# PLOT 1: Residuals vs fitted: Correlation should be 0 [Linearity assumption]
# PLOT 2: Q-Q plot of residuals: Points should be close to line [normality assumption]
# PLOT 3: Residuals scatter plot: No pattern in points, must be scattered [Homoscedasticity assumption]
# PLOT 4: Cook's distance for residuals: No point beyond 1 (dashed line) [Testing influential observations]

# To remove data and cleaning, use Excel