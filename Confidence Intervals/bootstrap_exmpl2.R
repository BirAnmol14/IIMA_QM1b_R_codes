#library(readr)
#AgeBoot <- read_csv("D:/Academic Matl/QM-1B/AgeBoot.csv")
library(boot)
#x<-AgeBoot$Age
#t<-sample(1:539,30,TRUE)
#xs=x[t]
xs<-c(7.9, 19.4, 12, 14.12, 14.91, 23.75)
mean(xs)

sampmean<-function(d,i){
  t2<-d[i]
  return(mean(t2))
}

sampmedian<-function(d,i){
  t2<-d[i]
#  print(t2)
  answ=median(t2)
  return(answ)
}

sampsd<-function(d,i){
  t2<-d[i]
  return(sd(t2))
}

sampiqr<-function(d,i){
  t2<-d[i]
  return(IQR(t2))
}

mean(xs)
ab<-boot(xs,sampmean,R=5000)
hist(ab$t)
sd(ab$t)
boot.ci(ab)
#print(c("Population Mean ", mean(x)))

median(xs)
ab2<-boot(xs, sampmedian, R=5000)
hist(ab2$t)
sd(ab2$t)
boot.ci(ab2)
#print(c("Population Median ", median(x)))

sd(xs)
ab3<-boot(xs, sampsd,R=5000)
hist(ab3$t)
sd(ab3$t)
boot.ci(ab3)
#print(c("Population SD ", sd(x)))

IQR(xs)
ab4<-boot(xs, sampiqr,R=5000)
hist(ab4$t)
sd(ab4$t)
boot.ci(ab4)
#print(c("Population IQR ", IQR(x)))