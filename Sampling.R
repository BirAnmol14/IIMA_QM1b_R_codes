#Estimate Avg money Section E has during QM1b class

# Population: Section E students

# Sample size = 10

s <- sample(1:89,10,replace = FALSE)
print(s)

# Cmd+Shift+Enter to run entire documents

x <- c(513,850,0,150,0,2735,2103,388,9994,0) #combines to form vector c(row,col)

print(mean(x))

print( mean(s) )
print( mean(s*5) )