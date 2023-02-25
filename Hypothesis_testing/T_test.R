library( readr )

data <- c(20.6, 19.2, 17, 19.1, 18.7, 22.5, 27.2, 17.9, 22.5, 21.3 )

# testing for normality of data
shapiro.test(data) #p-val > 0.1 => assume normal

# n < 30 therefore use t test

#Ho: mu>=22
#Ha: mu<22

t.test(data,mu=22,alternative="less",conf.level = 0.95)

# p-val = 0.08507 > 0.05 (alpha) => do not reject Ho
