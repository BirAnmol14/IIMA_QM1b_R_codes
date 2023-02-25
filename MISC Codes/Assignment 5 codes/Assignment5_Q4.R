library(readr)
data <- read.csv("./A5Q4.csv",header=TRUE)
#View(data)
ayrshire <- data$Ayrshire
guernsey <- data$Guernsey
n= length(ayrshire)
n # 10 -> t test if normal

#a) testing for normality of data
shapiro.test(ayrshire) #p-val= 0.3669 > 0.1 => normal

#b) testing for normality of data
shapiro.test(guernsey) #p-val= 0.8269 > 0.1 => normal

#c)
# Ho : 𝜇a ≤ 𝜇g
# Ha: 𝜇a >𝜇g
# Ho : 𝜇a - 𝜇g<=0
# Ha: 𝜇a - 𝜇g > 0
# alpha = 0.05 (5% level of significance)
t.test(ayrshire,guernsey,mu=0,alternative="greater",conf.level = 0.95)
# p-val =  0.9998 > 0.05 (alpha) => do not reject Ho, insufficient data to conclude

#d)
# On conducting the test to verify that average amount of butterfat in Ayrshire cow
# milk is greater that Guernsey cow's milk, we found that the test yielded a p-value: 0.9998
# This p-value is greater than level of significance of 0.05 used for test.
# The result indicates that we have insufficient data to make such claims and there is
# a possibility of average butterfat in Ayrshire cow is less than or equal to Guernsey cow's milk


