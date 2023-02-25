library( readr )
data <- read.csv("./Asg4Q8.csv", header = TRUE ) #sample
#View(data)

xs <- data$rent
n = length(xs)

#a)
sample_mean = mean(xs)
sprintf("Sample mean: %f",sample_mean)
sample_sd = sd(xs)
sprintf("Sample sd: %f",sample_sd)
confidence_level = 0.95
alpha = 1-confidence_level
z = qnorm(1-alpha/2)
CI_lower = sample_mean - z*sample_sd/sqrt(n)
CI_upper = sample_mean + z*sample_sd/sqrt(n)
sprintf("Confidence Interval around mean: [%f,%f]",CI_lower,CI_upper)

#b)
sampmean <- function(data,i){
  df <- data[i]
  return(mean(df))
}
b <- boot(xs,sampmean,R=5000) # generating 5000 replicates or resample of the sample
sprintf("Median of sample: %f",mean(xs)) # Estimate of population mean
boot.ci(b, conf=0.95, type='basic') # Bootstrap 95% Pivotal Confidence interval

#c)
sampsd <- function(data,i){
  df <- data[i]
  return(sd(df))
}
b <- boot(xs,sampsd,R=5000) # generating 5000 replicates or resample of the sample
sprintf("SD of sample: %f",sd(xs)) # Estimate of population sd
boot.ci(b, conf=0.95, type='basic') # Bootstrap 95% Pivotal Confidence interval