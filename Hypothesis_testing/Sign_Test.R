library( readr )
library(BSDA) #for SIGN test

xs <- c(189,233,195,160,212,176,231,185,199,213,202,193,174,166,248)

#ho: md=210
#ha: md!=210
# Documentation: https://www.rdocumentation.org/packages/BSDA/versions/1.2.1/topics/SIGN.test
# sign test on median
SIGN.test(xs, md= 210, alternative="two.sided", conf.level = 0.95)

#ho: mu=210
#ha: mu!=210
# sign test for mean
wilcox.test(xs, mu= 210, alternative="two.sided", conf.level = 0.95)

qqnorm(xs) #to look at distribution plot
shapiro.test(xs) #to check normailty

t.test(xs,mu=210,alternative="two.sided") #t test as data normal and n<15
