library( readr )
library(BSDA) #for SIGN test
library(boot)
set.seed(123) #for making replication of results

data <- c(8, 9, 10, 10, 10, 10, 10, 10, 11, 11, 11, 11, 12, 12, 13, 13, 13, 14, 14, 14, 15, 15, 15, 15, 15, 15, 15, 15, 16, 16, 16, 17, 17, 17, 17, 18, 18, 20, 22, 25, 27, 35, 38, 40)
n = length(data)
n

#a) testing for normality of data
shapiro.test(data) #p-val= 5.658e-07 < 0.1 => not normal

#b) H0: 𝜇 = 13.1
# H1: 𝜇 ≠ 13.1
# Since data not normal
# Simple sign test is for median thus we will use wilcox test for mean testing for non normal data
wilcox.test(data, mu= 13.1, alternative="two.sided", conf.level = 0.95)
# p-val = 0.04763 < 0.05 (alpha) => H0 rejected => H1 accepted

#c) CLT CI around 𝜇 can be applied n>30
n = length(data)
sample_mean = mean(data)
sprintf("Sample mean: %f",sample_mean)
sample_sd = sd(data)
sprintf("Sample sd: %f",sample_sd)
confidence_level = 0.95
alpha = 1-confidence_level
z = qnorm(1-alpha/2)
CI_lower = sample_mean - z*sample_sd/sqrt(n)
CI_upper = sample_mean + z*sample_sd/sqrt(n)
sprintf("Confidence Interval around mean: [%f,%f]",CI_lower,CI_upper)

#d) 95% bootstrap pivotal confidence interval for μ.
sampmean <- function(data,i){
  df <- data[i]
  return(mean(df))
}
b <- boot(data,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample
sprintf("Mean of sample: %f",mean(data)) # Estimate of population mean
boot.ci(b, conf=0.95, type='basic') # Bootstrap 95% Pivotal Confidence interval