# Stratas 1877-1899, 1900-1914, 1920-1939, 1946 - 1974, 1975- 1999, and 2000-2019

library( readr )
population_data <- read.csv("./cricktest.csv", header = TRUE )
#View(population_data)

#Q2(a)

n=60
data_i <-  population_data$Player
N = length(data_i)

N_1877_1899 = 0
N_1900_1914 = 0
N_1920_1939 = 0
N_1946_1974 = 0
N_1975_1999 = 0
N_2000_2019 = 0

for (i in 1:N){
  year <- population_data$Start.Year[i]
  if( year>=1877 & year <= 1899){
    N_1877_1899 =  N_1877_1899+1
  }else if( year>=1900 & year<=1914){
    N_1900_1914 = N_1900_1914+1
  }else if(year>=1920 & year<=1939){
    N_1920_1939 = N_1920_1939+1
  } else if(year>=1946 & year<=1974){
    N_1946_1974 = N_1946_1974+1
  }else if(year>=1975 & year<=1999){
    N_1975_1999 = N_1975_1999+1
  }else if(year>=2000 & year<=2019){
    N_2000_2019 = N_2000_2019+1
  }
}

population_N_1877_1899 <- c(-1,N_1877_1899)
x <- 1
population_N_1900_1914 <- c(-1,N_1900_1914)
y <- 1
population_N_1920_1939 <- c(-1,N_1920_1939)
z <- 1
population_N_1946_1974 <- c(-1,N_1946_1974)
a <- 1
population_N_1975_1999 <- c(-1,N_1975_1999)
b <- 1
population_N_2000_2019 <- c(-1,N_2000_2019)
c <- 1

for (i in 1:N){
  year <- population_data$Start.Year[i]
  if( year>=1877 & year <= 1899){
    population_N_1877_1899[x] = data_i[i]
    x = x+1
  }else if( year>=1900 & year<=1914){
    population_N_1900_1914[y] = data_i[i]
    y = y+1
  }else if(year>=1920 & year<=1939){
    population_N_1920_1939[z] = data_i[i]
    z = z+1
  } else if(year>=1946 & year<=1974){
    population_N_1946_1974[a] = data_i[i]
    a = a+1
  }else if(year>=1975 & year<=1999){
    population_N_1975_1999[b] = data_i[i]
    b = b+1
  }else if(year>=2000 & year<=2019){
    population_N_2000_2019[c] = data_i[i]
    c = c+1
  }
}

n_1877_1899 = round(N_1877_1899/N*n)
n_1900_1914 = round(N_1900_1914/N*n) +1 #adjust for rounding loss
n_1920_1939 = round(N_1920_1939/N*n)
n_1946_1974 = round(N_1946_1974/N*n)
n_1975_1999 = round(N_1975_1999/N*n)
n_2000_2019 = round(N_2000_2019/N*n)

sample_1877_1899 = sample(population_N_1877_1899, n_1877_1899, replace=FALSE)
print(sample_1877_1899)
sample_1900_1914 = sample(population_N_1900_1914, n_1900_1914, replace=FALSE)
print(sample_1900_1914)
sample_1920_1939 = sample(population_N_1920_1939, n_1920_1939, replace=FALSE)
print(sample_1920_1939)
sample_1946_1974 = sample(population_N_1946_1974, n_1946_1974, replace=FALSE)
print(sample_1946_1974)
sample_1975_1999 = sample(population_N_1975_1999, n_1975_1999, replace=FALSE)
print(sample_1975_1999)
sample_2000_2019 = sample(population_N_2000_2019, n_2000_2019, replace=FALSE)
print(sample_2000_2019)


#Q2(d) Systematic Sampling
ni = 10

k <- ceiling(N_1877_1899/ni)
sample_1877_1899 = c(-1,ni)
start <- sample(k,1,replace=FALSE)
for (i in 1:ni){
  sample_1877_1899[i] = population_N_1877_1899[start]
  start = (start+k)
  if(start>N_1877_1899){
    start = start - N_1877_1899
  }
}
print(sample_1877_1899)

k <- ceiling(N_1900_1914/ni)
sample_1900_1914 = c(-1,ni)
start <- sample(k,1,replace=FALSE)
for (i in 1:ni){
  sample_1900_1914[i] = population_N_1900_1914[start]
  start = (start+k)
  if(start>N_1900_1914){
    start = start - N_1900_1914
  }
}
print(sample_1900_1914)

k <- ceiling(N_1920_1939/ni)
sample_1920_1939 = c(-1,ni)
start <- sample(k,1,replace=FALSE)
for (i in 1:ni){
  sample_1920_1939[i] = population_N_1920_1939[start]
  start = (start+k)
  if(start>N_1920_1939){
    start = start - N_1920_1939
  }
}
print(sample_1920_1939)

k <- ceiling(N_1946_1974/ni)
sample_1946_1974 = c(-1,ni)
start <- sample(k,1,replace=FALSE)
for (i in 1:ni){
  sample_1946_1974[i] = population_N_1946_1974[start]
  start = (start+k)
  if(start>N_1946_1974){
    start = start - N_1946_1974
  }
}
print(sample_1946_1974)

k <- ceiling(N_1975_1999/ni)
sample_1975_1999 = c(-1,ni)
start <- sample(k,1,replace=FALSE)
for (i in 1:ni){
  sample_1975_1999[i] = population_N_1975_1999[start]
  start = (start+k)
  if(start>N_1975_1999){
    start = start - N_1975_1999
  }
}
print(sample_1975_1999)

k <- ceiling(N_2000_2019/ni)
sample_2000_2019 = c(-1,ni)
start <- sample(k,1,replace=FALSE)
for (i in 1:ni){
  sample_2000_2019[i] = population_N_2000_2019[start]
  start = (start+k) 
  if(start>N_2000_2019){
    start = start - N_2000_2019
  }
}
print(sample_2000_2019)

