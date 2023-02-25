library( readr )
sampledata <- read.csv("/Users/biranmolsingh/Coding/R/test_data.csv", header = TRUE )
View(sampledata)
str(sampledata)
population <- sampledata$X #extract header X
popmean <- mean(population)
popdev <- sd(population)

print(c(popmean,popdev))

sampl_means = c(-1,1000) #initialize 1000 places with -1
for ( i in 1:1000){
  sampl <- sample(population,15,replace=FALSE)
  sampl_means[i] <- mean(sampl)
}

# Expectation of population estimator
print(mean(sampl_means))

#Std Error in estimate
print(sd(sampl_means))

hist(sampl_means)