library(readr)
data <- c(4.45, 4.02, 5.51, 1.10, 2.62, 2.38, 5.94, 7.64) # sample

xs <- mean(data)
s <- sd(data)
n <- length(data)
N <- 50 # population_size
alpha = 0.01 # level of significance

z_alpha = qnorm(1-alpha,0,1)
print(z_alpha)

z_alpha_by_2 = qnorm(1-alpha/2,0,1)
print(z_alpha_by_2)

# CLT based CI - SRSWR
lb = xs-z_alpha_by_2 * s/sqrt(n)
ub = xs+z_alpha_by_2 * s/sqrt(n)
print(paste("Confidence Interval CLT-SRSWR: [",lb," , ",ub,"]",sep=""))


# CLT based CI - SRSWOR
se = sqrt(1-n/N)* s/sqrt(n)
lb = xs-z_alpha_by_2 * se
ub = xs+z_alpha_by_2 * se
print(paste("Confidence Interval CLT-SRSWOR: [",lb," , ",ub,"]",sep=""))

