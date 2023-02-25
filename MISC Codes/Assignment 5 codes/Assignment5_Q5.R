library(readr)
data <- read.csv("./A5Q5.csv",header=TRUE)
#View(data)
yield_1970 <- data$Yield.1970
yield_1973 <- data$Yield.1973

#a)
# Ho: 𝜇1973 <= 𝜇1970
# H1: 𝜇1973 > 𝜇1970

#b) alpha = 0.05
shapiro.test(yield_1973) #p-val= 0.8309 > 0.1 => normal
shapiro.test(yield_1970) #p-val= 0.5309 > 0.1 => normal
n = length(yield_1973)
n #12 < 30 => use T-tests

d = yield_1973-yield_1970
shapiro.test(d) # p-val 0.1507 > 0.1 => normal.

#Since difference is normal and we are observing before and after values, we will use paired t test

t.test(yield_1973,yield_1970,mu=0,alternative="greater",conf.level = 0.95,paired=TRUE)
# p-val =  0.759 > 0.05 (alpha) => do not reject Ho, insufficient data to conclude

#c)
# On conducting the test to verify that average amount of wheat produced per unit area in year 1973
# is greater that of the year 1970. We found that the test yielded a p-value:  0.759
# This p-value is greater than level of significance of 0.05 used for test.
# The result indicates that we have insufficient data to make such claims and there is
# a possibility of average amount of wheat produced per unit area in 1973 is less than or equal to that of year 1970.




