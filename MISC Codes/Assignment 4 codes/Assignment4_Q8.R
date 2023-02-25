library( readr )
data <- read.csv("./Asg4Q7.csv", header = TRUE ) #sample
#View(data)

xs = data$number
n = length(xs)

sample_mean = mean(xs)
sprintf("Sample mean: %f",sample_mean)
sample_sd = sd(xs)
sprintf("Sample sd: %f",sample_sd)

confidence_level = 0.95
alpha = 1-confidence_level

z = qnorm(1-alpha/2)

CI_lower = sample_mean - z*sample_sd/sqrt(n)
CI_upper = sample_mean + z*sample_sd/sqrt(n)
sprintf("Confidence Interval around mean: [%f,%f]",CI_lower,CI_upper)
