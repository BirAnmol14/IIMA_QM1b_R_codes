library( readr )
data <- read.csv("./Asg4Q6.csv", header = TRUE ) #sample
#View(data)
library(boot) # for bootstrapping
set.seed(123) #for making replication of results

a <- c(-1,20)
ai <- 1
b <- c(-1,20)
bi <- 1
c <- c(-1,20)
ci <- 1
d <- c(-1,20)
di <- 1

for(i in 1:80){
  if(data$carrier[i]=='A'){
    a[ai] = data$delay[i]
    ai = ai+1
  }
  else if(data$carrier[i]=='B'){
    b[bi] = data$delay[i]
    bi = bi+1
  }
  else if(data$carrier[i]=='C'){
    c[ci] = data$delay[i]
    ci = ci+1
  }
  else if(data$carrier[i]=='D'){
    d[di] = data$delay[i]
    di = di+1
  }
}

sampmean <- function(data,i){
  df <- data[i]
  return(mean(df))
}

print("For A")
bo <- boot(a,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample
sprintf("Mean of sample: %f",mean(a)) # Estimate of population mean 
alpha = 0.05
boot.ci(bo, conf=1-alpha, type='basic') # Bootstrap 95% Pivotal Confidence interval

print("For B")
bo <- boot(b,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample
sprintf("Mean of sample: %f",mean(b)) # Estimate of population mean 
alpha = 0.05
boot.ci(bo, conf=1-alpha, type='basic') # Bootstrap 95% Pivotal Confidence interval

print("For C")
bo <- boot(c,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample
sprintf("Mean of sample: %f",mean(c)) # Estimate of population mean 
alpha = 0.05
boot.ci(bo, conf=1-alpha, type='basic') # Bootstrap 95% Pivotal Confidence interval

print("For D")
bo <- boot(d,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample
sprintf("Mean of sample: %f",mean(d)) # Estimate of population mean 
alpha = 0.05
boot.ci(bo, conf=1-alpha, type='basic') # Bootstrap 95% Pivotal Confidence interval