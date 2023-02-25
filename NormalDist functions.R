# P(1.8<=X<=2.2), mean = 2, std dev = 1/sqrt(50)
a<-pnorm(2.2,2,1/sqrt(50)) # pnorm same as NORM.DIST
b<-pnorm(1.8,2,1/sqrt(50))
print(a-b)

norm_range <- function(a,b,u,s){
  return ( pnorm(b,u,s) - pnorm(a,u,s) )
}

norm_range(1.8,2.2,2,1/sqrt(50))


# qnorm just like norm.INV
# z val for two-sided 95% interval -> 1.96
qnorm(1-0.05/2,0,1)
