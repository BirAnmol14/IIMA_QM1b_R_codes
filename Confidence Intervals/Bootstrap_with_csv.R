library( readr )
data <- read.csv("./filename.csv", header = TRUE ) #sample
#View(data) 
library(boot)
xs <- c(data$x) #The sample drawn from population -- change data$varibale you want to find

sampfunc <- function(data,i){
  df <- data[i]
  return(median(df)) #Change to mean, median, sd etc what you want
}

b <- boot(xs,sampfunc,R=5000) # generating 5000 replicates or resample of the sample
sprintf("Characteristic of sample: %f",median(xs)) # Estimate of population characteristic
boot.ci(b, conf=0.95) # Bootstrap 95%