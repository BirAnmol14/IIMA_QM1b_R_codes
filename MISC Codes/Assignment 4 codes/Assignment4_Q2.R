library( readr )
data <- read.csv("./Asg4Q2.csv", header = TRUE ) #sample
#View(data) 
library(boot)
set.seed(123) #for making replication of results

xs <- c(data$x) #The sample drawn from population

sampmedian <- function(data,i){
  df <- data[i]
  return(median(df))
}

b <- boot(xs,sampmedian,R=5000) # generating 5000 replicates or resample of the sample
sprintf("Median of sample: %f",median(xs)) # Estimate of population median
boot.ci(b, conf=0.95, type='basic') # Bootstrap 95% Pivotal Confidence interval