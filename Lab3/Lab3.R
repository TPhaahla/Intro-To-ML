## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------
rm(list=ls())


## ----task0---------------------------------------------------------------------------------------------------------------------------------------

library(neuralnet)

data(iris)
attach(iris)

newData = iris
newData$Sepal.Length = scale(newData$Sepal.Length)
newData$Sepal.Width = scale(newData$Sepal.Width)
newData$Petal.Length = scale(newData$Petal.Length)
newData$Petal.Width = scale(newData$Petal.Width)



## ----task1---------------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
neuralMod = neuralnet(Species~ Petal.Length + Petal.Width, data = newData, 
                      hidden = c(5),
                      algorithm = 'backprop',
                      err.fct = 'ce',
                      act.fct = 'logistic',
                      learningrate = 0.05,
                      stepmax = 10^6,
                      linear.output = F, threshold=0.01)
                      #lifesign = 'full') # lifesign full means it will keep reporting as it's fitting the neural network

plot(neuralMod)



## ----task2---------------------------------------------------------------------------------------------------------------------------------------

M=200

x1_dummy = seq(min(newData$Petal.Length), max(newData$Petal.Length), length=M)
x2_dummy = seq(min(newData$Petal.Width), max(newData$Petal.Width), length=M)

x1 = rep(x1_dummy, M)
x2 = rep(x2_dummy, each=M)

Lat = data.frame(Petal.Width = x2, Petal.Length = x1)

pred = predict(neuralMod, Lat)

clss = apply(pred, 1, which.max)

cols = c('blue', 'gray', 'magenta')

plot(Lat$Petal.Length~Lat$Petal.Width, pch=16, col=cols[clss], 
     xlab="Petal Width", ylab = "Petal Length", main = "Learning Rate = 0.05")

text(newData$Petal.Length~newData$Petal.Width, labels= as.numeric(newData$Species))

legend('topright',lty=1, legend = c('Setosa', 'Versicolor', 'Virginica'),
       col = c('blue', 'gray', 'magenta'), bty='o', bg='white')
#text(newData$Petal.Length~newData$Petal.Width, labels= as.numeric(newData$Species))



## ----problem2lab3--------------------------------------------------------------------------------------------------------------------------------

model2 = neuralnet(Species~ Petal.Length + Petal.Width, data = newData,
                   hidden = c(5),
                   stepmax = 10^6,
                   learningrate = 0.01,
                   threshold= 0.01,
                   algorithm = 'backprop',
                   err.fct = 'ce',
                   act.fct = 'logistic',
                   linear.output = F)#,
                   #lifesign = 'full'
                   #)

#plot(model2)

pred1 = predict(model2, Lat)
clss1 = apply(pred1, 1, which.max)

plot(x1~x2, pch=16, col=cols[clss1], xlab="Petal Width",
     ylab = "Petal Height", main = "Learning Rate = 0..01")

text(newData$Petal.Length~newData$Petal.Width, labels= as.numeric(newData$Species))

legend('bottomright',lty=1, legend = c('Setosa', 'Versicolor', 'Virginica'),
       col = c('blue', 'gray', 'magenta'), bty='o', bg='white')



## ----problem3lab3--------------------------------------------------------------------------------------------------------------------------------

model3 = neuralnet(Species~ Petal.Length + Petal.Width, data=newData,
                   hidden = c(5),
                   threshold = 0.1,
                   stepmax = 10^6,
                   learningrate = 0.01,
                   algorithm = 'backprop',
                   act.fct = 'logistic',
                   err.fct = 'ce',
                   linear.output = FALSE)

pred2 = predict(model3, Lat)
clss2 = apply(pred2, 1, which.max)

plot(x1~x2, pch=16, col=cols[clss2], xlab = "Petal Width",
     ylab = "Petal Length", main = "Threshold = 0.1 : Learning Rate = 0.01")

text(newData$Petal.Length~newData$Petal.Width, labels= as.numeric(newData$Species))

legend('topright', lty=1, legend = c("Setosa", "Versicolor", "Virginica"),
       col=c('blue', 'gray', 'magenta'), bg='white')


