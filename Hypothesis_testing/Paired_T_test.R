library( readr )
data <- read.csv("./stock_data.csv", header = TRUE ) #sample
#View(data) 
open_data <- data$Open
close_Data <- data$Close

d = close_Data-open_data

# testing for normality of data
shapiro.test(d) #p-val > 0.1 => assume normal

# n < 30 therefore use t test
#Ho: mu1-mu2=0
#Ha: mu1-mu2!=0

t.test(d,mu=0,alternative="two.sided",conf.level = 0.95)
# p-val = 0.7411 > 0.05 (alpha) => do not reject Ho

#OR

t.test(close_Data,open_data,mu=0,alternative="two.sided",conf.level = 0.95,paired=TRUE)
# p-val = 0.7411 > 0.05 (alpha) => do not reject Ho

#These two ways give same result
