data("anscombe") #load dataset
head(anscombe) #look at starting 5 rows of dataset

model <- lm(y1~x1,data=anscombe)
model
summary(model)

plot(anscombe$x1,anscombe$y1)
abline(reg=model)

plot(model)


