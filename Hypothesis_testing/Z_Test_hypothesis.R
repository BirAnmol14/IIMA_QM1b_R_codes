library(BSDA)
library(readr)

data <- c(13.3,6,20,8,14,19,18,25,16,24,15,1,15)

sigma_data = sd(data) #ideally use population sd
n = length(data)
print(n)

# testing for normality of data
shapiro.test(data) #p-val > 0.1 => assume normal

# if n > 30, use z test
#Ho: mu>=20
#Ha: mu<20

z.test(data,mu=20,alternative="less",sigma.x = sigma_data,conf.level = 0.95)
# p-val = 0.003872 < 0.05 (alpha) => reject Ho; accept Ha
