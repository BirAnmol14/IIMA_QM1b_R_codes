library( readr )

men_data <- c(13.3,6,20,8,14,19,18,25,16,24,15,1,15)
women_data <- c(22,16,21.7,21,30,26,12,23.2,28,23)

# testing for normality of data
shapiro.test(men_data) #p-val > 0.1 => assume normal
shapiro.test(women_data)

# n < 30 therefore use t test

#Ho: mu1-mu2=0
#Ha: mu1-mu2!=0

t.test(men_data,women_data,mu=0,alternative="two.sided",conf.level = 0.95)
# p-val = 0.00865 < 0.05 (alpha) => reject Ho; accept Ha


#CONFIDENCE INTERVAL AT 5% significance for women mu - men mu
t.test(women_data,men_data,mu=0,alternative="two.sided",conf.level = 0.95)
# Confidence Interval = (2.069692,12.618000)