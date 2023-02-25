library( readr )
library(BSDA) #for SIGN test
library(boot)
set.seed(123) #for making replication of results

data <- read.csv("./A5Q2.csv", header = TRUE ) #sample
#View(data)

self <- data$Self.Fertilized
cross <- data$Cross.Fertilized

n = length(self)
n # 15 pairs < 30 => t test if normal

#a) 
d = cross-self
# testing for normality of data
shapiro.test(d) #p-val= 0.09319 < 0.1 => not normal

#b) H0 : 𝜇c = 𝜇s against H1 : 𝜇c > 𝜇s at 5% level of significance
# H0 : 𝜇d=0, H1: d > 0 , it is generally equal to H0 : d<=0, H1: d > 0

# Since data not normal
wilcox.test(d, mu= 0, alternative="greater", conf.level = 0.95)
# p-val = 0.02063 < 0.05 (alpha) => H0 rejected => H1 accepted

# Paired test - t-test assuming normal since alpha (0.1) used in shapiro test is only weak evidence
# and n<30
t.test(d,mu=0,alternative="greater",conf.level = 0.95)
# p-val = 0.02512 < 0.05 (alpha) => H0 rejected => H1 accepted

#c) Since n<30, bootstrap for confidence intervals on d = 𝜇c -𝜇s 
sampmean <- function(data,i){
  df <- data[i]
  return(mean(df))
}
b <- boot(d,sampmean,R=5000) # generating 5000 replicates or re-sample of the sample
sprintf("Mean of sample: %f",mean(d)) # Estimate of population mean
boot.ci(b, conf=0.95) # Bootstrap 95% Confidence intervals
