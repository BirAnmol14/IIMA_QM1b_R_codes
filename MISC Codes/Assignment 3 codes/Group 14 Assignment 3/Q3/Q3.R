library( readr )
population_data <- read.csv("./clt1.csv", header = TRUE )
#View(population_data)

n=40
data_i <-  population_data$X
N = length(data_i)
sample_i <-  sample(data_i,n,replace=TRUE) #SRSWR
print(sample_i)