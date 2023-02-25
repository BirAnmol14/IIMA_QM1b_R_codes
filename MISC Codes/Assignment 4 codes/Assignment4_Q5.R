library( readr )
data <- read.csv("./Asg4Q4.csv", header = TRUE ) #sample
#View(data)
library(boot) # for bootstrapping

set.seed(123) #for making replication of results

xs <- c(data$x) #The sample drawn from population

sampsd <- function(data,i){
  df <- data[i]
  return(sd(df))
}

b <- boot(xs,sampsd,R=5000) # generating 5000 replicates or re-sample of the sample

sprintf("Std dev of sample: %f",sd(xs)) # Estimate of population sd
alpha = 0.05
boot.ci(b, conf=1-alpha, type='basic') # Bootstrap 95% Pivotal Confidence interval