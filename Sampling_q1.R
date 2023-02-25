s <- c(8,5,2,6,6,3,8,6,10,7,15,9,15,3,5,6,7,10,14,3,4,17,10,6,14,12,7,8,12,9)
print( mean(s) ) #sample mean estimate for population mean
s_var <- var(s)
pop_var_estimate <- s_var
print(pop_var_estimate)

#Std error of varience
#100 is pop size
est_sd_err <- sqrt(1-length(s)/100)*sd(s)/sqrt(length(s))

print(est_sd_err)

# Option & minus, shortcut for <- <- <- <- <- <- <- <- <- 