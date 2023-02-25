library(readr)
library(boot)

xs <- c(7.9,19.4,12,14.12,14.91,23.75) #The sample drawn from population

sampmean <- function(data,i){
  df <- data[i]
  c(mean(df))
}

b <- boot(xs,sampmean,R=5000) # generating 5000 replicates or resample of the sample

sprintf("Mean of sample: %f",mean(xs)) # Estimate of population means
boot.ci(b) # Bootstrap Confidence intervals