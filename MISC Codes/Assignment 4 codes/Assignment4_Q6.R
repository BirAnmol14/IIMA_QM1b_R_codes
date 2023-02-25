library( readr )
data <- read.csv("./Asg4Q5.csv", header = TRUE ) #sample
#View(data)
library(boot) # for bootstrapping

set.seed(123) #for making replication of results

machine_data = data$machine
expert_data = data$expert

xs = abs(machine_data - expert_data)
sampmean <- function(data,i){
  df <- data[i]
  return(mean(df))
}

b <- boot(xs,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample

sprintf("Mean of sample: %f",mean(xs)) # Estimate of population mean 
alpha = 0.05
boot.ci(b, conf=1-alpha, type='perc') # Bootstrap 95% Percentile Confidence interval