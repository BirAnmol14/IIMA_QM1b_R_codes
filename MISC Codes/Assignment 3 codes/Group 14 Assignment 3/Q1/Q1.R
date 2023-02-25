library( readr )
population_data <- read.csv("./iris.csv", header = TRUE )
#View(population_data)

#Q1(a)
#SRSWOR 30
n = 30 # sample size
data_i <-  population_data$X
N = length(data_i) # population size
sample_i <-  sample(data_i,n,replace=FALSE)
print(sample_i)

sample_sepal_length <- c(-1,n)
sample_sepal_width <- c(-1,n)
sample_petal_length <- c(-1,n)
sample_petal_width <- c(-1,n)

for (i in 1:n){
  sample_sepal_length[i] <- population_data$Sepal.Length[sample_i[i]]
  sample_sepal_width[i] <- population_data$Sepal.Width[sample_i[i]]
  sample_petal_length[i] <- population_data$Petal.Length[sample_i[i]]
  sample_petal_width[i] <- population_data$Petal.Width[sample_i[i]]
}

sample_sepal_length_mean = mean(sample_sepal_length)
std_err_sepal_length_mean = sqrt(1-n/N)*sd(sample_sepal_length)/sqrt(n)
sprintf("Estimate for population sepal length mean: %f",sample_sepal_length_mean)
sprintf("Standard Error for estimate of population sepal length mean: %f",std_err_sepal_length_mean)

sample_sepal_width_mean = mean(sample_sepal_width)
std_err_sepal_width_mean = sqrt(1-n/N)*sd(sample_sepal_width)/sqrt(n)
sprintf("Estimate for population sepal width mean: %f",sample_sepal_width_mean)
sprintf("Standard Error for estimate of population sepal width mean: %f",std_err_sepal_width_mean)

sample_petal_length_mean = mean(sample_petal_length)
std_err_petal_length_mean = sqrt(1-n/N)*sd(sample_petal_length)/sqrt(n)
sprintf("Estimate for population petal length mean: %f",sample_petal_length_mean)
sprintf("Standard Error for estimate of population petal length mean: %f",std_err_petal_length_mean)

sample_petal_width_mean = mean(sample_petal_width)
std_err_petal_width_mean = sqrt(1-n/N)*sd(sample_petal_width)/sqrt(n)
sprintf("Estimate for population petal width mean: %f",sample_petal_width_mean)
sprintf("Standard Error for estimate of population petal width mean: %f",std_err_petal_width_mean)

#Q1(b) stratified sampling
n = 30
data_i <-  population_data$X
N = length(data_i) # population size
setosa_N = 0
versicolor_N = 0
virginica_N = 0
for (i in 1:N){
  type <- population_data$Species[i]
  if(type == 'setosa'){
    setosa_N = setosa_N+1
  }else if(type == 'versicolor'){
    versicolor_N = versicolor_N+1
  }else{
    virginica_N = virginica_N+1
  }
}
stopifnot(setosa_N+virginica_N+versicolor_N == N) 

setosa_population_i <- c(-1,setosa_N)
x <- 1
versicolor_population_i <- c(-1,versicolor_N)
y <- 1
virginica_population_i <- c(-1,virginica_N) 
z <- 1

for (i in 1:N){
  type <- population_data$Species[i]
  if(type == 'setosa'){
    setosa_population_i[x] = population_data$X[i]
    x = x+1
  }else if(type == 'versicolor'){
    versicolor_population_i[y] = population_data$X[i]
    y = y+1
  }else{
    virginica_population_i[z] = population_data$X[i]
    z = z+1
  }
}

setosa_n = setosa_N/N*n
virginica_n = virginica_N/N*n
versicolor_n = versicolor_N/N*n

stopifnot(setosa_n+virginica_n+versicolor_n == n)

setosa_sample_i <- sample(setosa_population_i,setosa_n,replace=FALSE)
print(setosa_sample_i)
virginica_sample_i <- sample(virginica_population_i,virginica_n,FALSE)
print(virginica_sample_i)
versicolor_sample_i <- sample(versicolor_population_i,versicolor_n,FALSE)
print(versicolor_sample_i)

setosa_sample_sepal_length <- c(-1,setosa_n)
setosa_sample_sepal_width <- c(-1,setosa_n)
setosa_sample_petal_length <- c(-1,setosa_n)
setosa_sample_petal_width <- c(-1,setosa_n)
for (i in 1:setosa_n){
  setosa_sample_sepal_length[i] <- population_data$Sepal.Length[setosa_sample_i[i]]
  setosa_sample_sepal_width[i] <- population_data$Sepal.Width[setosa_sample_i[i]]
  setosa_sample_petal_length[i] <- population_data$Petal.Length[setosa_sample_i[i]]
  setosa_sample_petal_width[i] <- population_data$Petal.Width[setosa_sample_i[i]]
}

versicolor_sample_sepal_length <- c(-1,versicolor_n)
versicolor_sample_sepal_width <- c(-1,versicolor_n)
versicolor_sample_petal_length <- c(-1,versicolor_n)
versicolor_sample_petal_width <- c(-1,versicolor_n)
for (i in 1:versicolor_n){
  versicolor_sample_sepal_length[i] <- population_data$Sepal.Length[versicolor_sample_i[i]]
  versicolor_sample_sepal_width[i] <- population_data$Sepal.Width[versicolor_sample_i[i]]
  versicolor_sample_petal_length[i] <- population_data$Petal.Length[versicolor_sample_i[i]]
  versicolor_sample_petal_width[i] <- population_data$Petal.Width[versicolor_sample_i[i]]
}

virginica_sample_sepal_length <- c(-1,virginica_n)
virginica_sample_sepal_width <- c(-1,virginica_n)
virginica_sample_petal_length <- c(-1,virginica_n)
virginica_sample_petal_width <- c(-1,virginica_n)
for (i in 1:virginica_n){
  virginica_sample_sepal_length[i] <- population_data$Sepal.Length[virginica_sample_i[i]]
  virginica_sample_sepal_width[i] <- population_data$Sepal.Width[virginica_sample_i[i]]
  virginica_sample_petal_length[i] <- population_data$Petal.Length[virginica_sample_i[i]]
  virginica_sample_petal_width[i] <- population_data$Petal.Width[virginica_sample_i[i]]
}

estimate_sepal_length_mean = setosa_N/N*mean(setosa_sample_sepal_length) + versicolor_N/N*mean(versicolor_sample_sepal_length) + virginica_N/N*mean(virginica_sample_sepal_length)
std_err_sepal_length_mean = sqrt((setosa_N/N)^2*(1-setosa_n/setosa_N)*sd(setosa_sample_sepal_length)^2/setosa_n + (versicolor_N/N)^2*(1-versicolor_n/versicolor_N)*sd(versicolor_sample_sepal_length)^2/versicolor_n + (virginica_N/N)^2*(1-virginica_n/virginica_N)*sd(virginica_sample_sepal_length)^2/virginica_n )
sprintf("Estimate for population sepal length mean: %f",estimate_sepal_length_mean)
sprintf("Standard Error for estimate of population sepal length mean: %f",std_err_sepal_length_mean)

estimate_sepal_width_mean = setosa_N/N*mean(setosa_sample_sepal_width) + versicolor_N/N*mean(versicolor_sample_sepal_width) + virginica_N/N*mean(virginica_sample_sepal_width)
std_err_sepal_width_mean = sqrt((setosa_N/N)^2*(1-setosa_n/setosa_N)*sd(setosa_sample_sepal_width)^2/setosa_n + (versicolor_N/N)^2*(1-versicolor_n/versicolor_N)*sd(versicolor_sample_sepal_width)^2/versicolor_n + (virginica_N/N)^2*(1-virginica_n/virginica_N)*sd(virginica_sample_sepal_width)^2/virginica_n )
sprintf("Estimate for population sepal width mean: %f",estimate_sepal_width_mean)
sprintf("Standard Error for estimate of population sepal width mean: %f",std_err_sepal_width_mean)

estimate_petal_length_mean = setosa_N/N*mean(setosa_sample_petal_length) + versicolor_N/N*mean(versicolor_sample_petal_length) + virginica_N/N*mean(virginica_sample_petal_length)
std_err_petal_length_mean = sqrt((setosa_N/N)^2*(1-setosa_n/setosa_N)*sd(setosa_sample_petal_length)^2/setosa_n + (versicolor_N/N)^2*(1-versicolor_n/versicolor_N)*sd(versicolor_sample_petal_length)^2/versicolor_n + (virginica_N/N)^2*(1-virginica_n/virginica_N)*sd(virginica_sample_petal_length)^2/virginica_n )
sprintf("Estimate for population petal length mean: %f",estimate_petal_length_mean)
sprintf("Standard Error for estimate of population petal length mean: %f",std_err_petal_length_mean)

estimate_petal_width_mean = setosa_N/N*mean(setosa_sample_petal_width) + versicolor_N/N*mean(versicolor_sample_petal_width) + virginica_N/N*mean(virginica_sample_petal_width)
std_err_petal_width_mean = sqrt((setosa_N/N)^2*(1-setosa_n/setosa_N)*sd(setosa_sample_petal_width)^2/setosa_n + (versicolor_N/N)^2*(1-versicolor_n/versicolor_N)*sd(versicolor_sample_petal_width)^2/versicolor_n + (virginica_N/N)^2*(1-virginica_n/virginica_N)*sd(virginica_sample_petal_width)^2/virginica_n )
sprintf("Estimate for population petal width mean: %f",estimate_petal_width_mean)
sprintf("Standard Error for estimate of population petal width mean: %f",std_err_petal_width_mean)


# Q3
n = 30
data_i <-  population_data$X
N = length(data_i) # population size
setosa_osd = 0.35
versicolor_osd = 0.52 
virginica_osd = 0.64

setosa_N = 0
versicolor_N = 0
virginica_N = 0
for (i in 1:N){
  type <- population_data$Species[i]
  if(type == 'setosa'){
    setosa_N = setosa_N+1
  }else if(type == 'versicolor'){
    versicolor_N = versicolor_N+1
  }else{
    virginica_N = virginica_N+1
  }
}
stopifnot(setosa_N+virginica_N+versicolor_N == N) 

setosa_population_i <- c(-1,setosa_N)
x <- 1
versicolor_population_i <- c(-1,versicolor_N)
y <- 1
virginica_population_i <- c(-1,virginica_N) 
z <- 1

for (i in 1:N){
  type <- population_data$Species[i]
  if(type == 'setosa'){
    setosa_population_i[x] = population_data$X[i]
    x = x+1
  }else if(type == 'versicolor'){
    versicolor_population_i[y] = population_data$X[i]
    y = y+1
  }else{
    virginica_population_i[z] = population_data$X[i]
    z = z+1
  }
}

setosa_n = round(n*(setosa_N*setosa_osd)/(setosa_N*setosa_osd+versicolor_N*versicolor_osd+virginica_N*virginica_osd))
virginica_n = round(n*(virginica_N*virginica_osd)/(setosa_N*setosa_osd+versicolor_N*versicolor_osd+virginica_N*virginica_osd))
versicolor_n = round(n*(versicolor_N*versicolor_osd)/(setosa_N*setosa_osd+versicolor_N*versicolor_osd+virginica_N*virginica_osd))

stopifnot(setosa_n+virginica_n+versicolor_n == n)

setosa_sample_i <- sample(setosa_population_i,setosa_n,replace=FALSE)
print(setosa_sample_i)
virginica_sample_i <- sample(virginica_population_i,virginica_n,FALSE)
print(virginica_sample_i)
versicolor_sample_i <- sample(versicolor_population_i,versicolor_n,FALSE)
print(versicolor_sample_i)

setosa_sample_sepal_length <- c(-1,setosa_n)
for (i in 1:setosa_n){
  setosa_sample_sepal_length[i] <- population_data$Sepal.Length[setosa_sample_i[i]]
}

versicolor_sample_sepal_length <- c(-1,versicolor_n)
for (i in 1:versicolor_n){
  versicolor_sample_sepal_length[i] <- population_data$Sepal.Length[versicolor_sample_i[i]]
}

virginica_sample_sepal_length <- c(-1,virginica_n)
for (i in 1:virginica_n){
  virginica_sample_sepal_length[i] <- population_data$Sepal.Length[virginica_sample_i[i]]
}

estimate_sepal_length_mean = setosa_N/N*mean(setosa_sample_sepal_length) + versicolor_N/N*mean(versicolor_sample_sepal_length) + virginica_N/N*mean(virginica_sample_sepal_length)
std_err_sepal_length_mean = sqrt((setosa_N/N)^2*(1-setosa_n/setosa_N)*sd(setosa_sample_sepal_length)^2/setosa_n + (versicolor_N/N)^2*(1-versicolor_n/versicolor_N)*sd(versicolor_sample_sepal_length)^2/versicolor_n + (virginica_N/N)^2*(1-virginica_n/virginica_N)*sd(virginica_sample_sepal_length)^2/virginica_n )
sprintf("Estimate for population sepal length mean: %f",estimate_sepal_length_mean)
sprintf("Standard Error for estimate of population sepal length mean: %f",std_err_sepal_length_mean)