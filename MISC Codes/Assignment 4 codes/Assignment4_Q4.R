library( readr )
data <- read.csv("./Asg4Q3.csv", header = TRUE ) #sample
#View(data) 
set.seed(123) #for making replication of results

xs <- c(data$x) #The sample drawn from population
hist(xs)
n = length(xs)
zcompute <- function(element){
  ifelse(element<=10,1,0)
}
zi <- zcompute(xs)

p = sum(zi)/n
sprintf("Sample proportion: %f",p)
s = sqrt(n/(n-1)*p*(1-p))
sprintf("Sample SD: %f",s)

confidence_level = 0.95
alpha = 1-confidence_level

z = qnorm(1-alpha/2)
CI_lower = p - z*s/sqrt(n)
CI_upper = p + z*s/sqrt(n)

sprintf("Confidence Interval around proportion p: [%f,%f]",CI_lower,CI_upper)