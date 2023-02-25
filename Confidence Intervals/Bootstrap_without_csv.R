library( readr )
library(boot)
xs <- c(1,2,3,4) #The sample drawn from population -- Put sample data here

sampfunc <- function(data,i){
  df <- data[i]
  return(mean(df)) #Change to mean, median, sd etc what you want
}

b <- boot(xs,sampfunc,R=5000) # generating 5000 replicates or resample of the sample
sprintf("Characteristic of sample: %f",mean(xs)) # Estimate of population characteristic
boot.ci(b, conf=0.95) # Bootstrap 95%