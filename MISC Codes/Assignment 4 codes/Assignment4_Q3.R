library( readr )
data <- read.csv("./Asg4Q2.csv", header = TRUE ) #sample
#View(data) 
library(boot) # for bootstrapping
library(moments) # for skewness

set.seed(123) #for making replication of results

xs <- c(data$x) #The sample drawn from population

sampskew <- function(data,i){
  df <- data[i]
  return(skewness(df))
}

b <- boot(xs,sampskew,R=5000) # generating 5000 replicates or resample of the sample

sprintf("Skewness of sample: %f",skewness(xs)) # Estimate of population skewness
alpha = 0.05
boot.ci(b, conf=1-alpha, type='perc') # Bootstrap 95% Percentile Confidence interval