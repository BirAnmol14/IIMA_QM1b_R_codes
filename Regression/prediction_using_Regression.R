attach(faithful) # for eruptions data-set

View(faithful) # look at data-set
eruption.lm=lm(eruptions~waiting) # create linear model

#data for prediction
waiting <- c(80,70)
newData = data.frame(waiting)

predict(eruption.lm,newData,interval="confidence") # 95% CI for E(Y) values for the values of X1,X2,.., moves due to uncertainity in exact values of Betas for these X1,X2,...
predict(eruption.lm,newData,interval="prediction") # 95% CI for predicted Y values, moves due to uncertainty in estimation of the residual ei