## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------
rm(list=ls())
library(h2o)
library(rpart)
library(rpart.plot)

h2o.init()

knitr::opts_chunk$set(echo = TRUE)


## ----task1, message=FALSE, results=FALSE---------------------------------------------------------------------------------------------------------

data(ptitanic)

data = na.omit(ptitanic)

set.seed(2021)

data = data.frame(survived = as.factor(data$survived), pclass = as.factor(data$pclass),
                  sex = as.factor(data$sex), age = as.numeric(data$age))

N = dim(data)[1]

#Sample from the titanic dataset without replacement
set = sample(1:N, floor(0.8*N), replace=FALSE)

data_train = as.h2o(data[set, ])
data_val = as.h2o(data[-set, ])



## ----task2, message=FALSE, results=FALSE---------------------------------------------------------------------------------------------------------

# Create a data frame, split the data and cast the training and validation frames as an h2o objects:
model = h2o.deeplearning(x = 2:4, y=1, standardize =TRUE,
                         training_frame = data_train,
                         validation_frame = data_val,
                         hidden = c(5,3),
                         activation = 'Tanh',
                         distribution = "bernoulli",
                         loss = "CrossEntropy",
                         adaptive_rate = FALSE,
                         rate=0.001, epochs = 1000, l2 =0,
                         reproducible = TRUE, seed=2)

#plot(model)
report = h2o.logloss(model, train=T, valid = T)
report



## ----task3, message=FALSE, results=FALSE---------------------------------------------------------------------------------------------------------

M=100
ageSeq = seq(min(data$age), max(data$age), length =M)

Xnew1 = data.frame(pclass='1st', sex='male', age = ageSeq)
Xnew2 = data.frame(pclass='2nd', sex='male', age = ageSeq)
Xnew3 = data.frame(pclass='3rd', sex='male', age = ageSeq)

# create predictions based on the model:

pred1 = h2o.predict(model, as.h2o(Xnew1))
pred1 = as.data.frame(pred1)

pred2 = h2o.predict(model, as.h2o(Xnew2))
pred2 = as.data.frame(pred2)

pred3 = h2o.predict(model, as.h2o(Xnew3))
pred3 = as.data.frame(pred3)

#Plot the predictions
plot(pred1$survived ~ Xnew1$age, type='l', col="red", ylim=c(0,1), xlab = "Age",
     ylab= "Probability of Survival",
     main = "Probability of Survival with respect to Age")
lines(pred2$survived~Xnew2$age, col="blue")
lines(pred3$survived~Xnew3$age, col="black")



## ----task4, include=FALSE------------------------------------------------------------------------------------------------------------------------

model = h2o.deeplearning(x = 2:4, y=1, standardize =TRUE,
                         training_frame = data_train,
                         validation_frame = data_val,
                         hidden = c(5,3),
                         activation = 'Tanh',
                         distribution = "bernoulli",
                         loss = "CrossEntropy",
                         adaptive_rate = FALSE,
                         rate=0.001, epochs = 1000, l2 =0.01,
                         reproducible = TRUE, seed=2)
M=100
ageSeq = seq(min(data$age), max(data$age), length =M)

Xnew1 = data.frame(pclass='1st', sex='male', age = ageSeq)
Xnew2 = data.frame(pclass='2nd', sex='male', age = ageSeq)
Xnew3 = data.frame(pclass='3rd', sex='male', age = ageSeq)

# create predictions based on the model:

pred1 = h2o.predict(model, as.h2o(Xnew1))
pred1 = as.data.frame(pred1)

pred2 = h2o.predict(model, as.h2o(Xnew2))
pred2 = as.data.frame(pred2)

pred3 = h2o.predict(model, as.h2o(Xnew3))
pred3 = as.data.frame(pred3)

#Plot the predictions






## ----task4_1-------------------------------------------------------------------------------------------------------------------------------------
plot(pred1$survived ~ Xnew1$age, type='l', col="red", ylim=c(0,1), xlab = "Age",
     ylab= "Probability of Survival",
     main = "Probability of Survival with respect to Age")
lines(pred2$survived~Xnew2$age, col="blue")
lines(pred3$survived~Xnew3$age, col="black")
legend('topright', lty=1, legend = c('1st class', '2nd class', '3rd class'),
       col=c('red', 'blue', 'black'), bty='o')

Y = (data$survived == 'survived')

ageBins = seq(0, 80, by=5)

emp1 = rep(NA, length(ageBins)-1)
emp2 = rep(NA, length(ageBins)-1)
emp3 = rep(NA, length(ageBins)-1)

for(i in 2:length(ageBins)){
  emp1[i-1] = mean(Y[(data$sex=='male')&(data$pclass=='1st')&
                       (data$age>=ageBins[i-1])&(data$age<=ageBins[i])])
}
points(emp1~c(ageBins[-1]-2.5), col='red', pch=16)

for(i in 2:length(ageBins)){
  emp2[i-1] = mean(Y[(data$sex=='male')&(data$pclass=='2nd')&
                       (data$age>=ageBins[i-1])&(data$age<=ageBins[i])])
}
points(emp2~c(ageBins[-1]-2.5), col='blue', pch=16)

for(i in 2:length(ageBins)){
  emp3[i-1] = mean(Y[(data$sex=='male')&(data$pclass=='3rd')&
                       (data$age>=ageBins[i-1])&(data$age<=ageBins[i])])
}
points(emp3~c(ageBins[-1]-2.5), col='black', pch=16)


## ----prb1lab4, message=FALSE, results=FALSE------------------------------------------------------------------------------------------------------

Xnew1f = data.frame(pclass='1st', sex='female', age = ageSeq)
Xnew2f = data.frame(pclass='2nd', sex='female', age = ageSeq)
Xnew3f = data.frame(pclass='3rd', sex='female', age = ageSeq)

pred1f = h2o.predict(model, as.h2o(Xnew1f))
pred1f = as.data.frame(pred1f)

pred2f = h2o.predict(model, as.h2o(Xnew2f))
pred2f = as.data.frame(pred2f)

pred3f = h2o.predict(model, as.h2o(Xnew3f))
pred3f = as.data.frame(pred3f)

plot(pred1f$survived~Xnew1f$age, ylim = c(0,1), col='red', type='l',
     ylab = 'Probability of Survival', xlab ='Age',
     main= "The predicted survival rate of females with respect to age")
lines(pred2f$survived~Xnew2f$age, col='blue')
lines(pred3f$survived~Xnew3f$age, col='black')
legend('topright', lty=1, legend = c('1st Class', '2nd Class', '3rd Class'),
       col=c('red', 'blue', 'black'), bty='o', bg='white')

emp1f = rep(NA, length(ageBins)-1)
emp2f = rep(NA, length(ageBins)-1)
emp3f = rep(NA, length(ageBins)-1)

for(i in 2:length(ageBins)){
  emp1f[i-1] = mean(Y[(data$pclass=='1st')&(data$sex=='female')&
                        (data$age>=ageBins[i-1])&(data$age<=ageBins[i])])
}
points(emp1f~c(ageBins[-1]-2.5), pch=16, col='red')

for(i in 2:length(ageBins)){
  emp2f[i-1] = mean(Y[(data$pclass=='2nd')&(data$sex=='female')&
                        (data$age>=ageBins[i-1])&(data$age<=ageBins[i])])
}
points(emp2f~c(ageBins[-1]-2.5), pch=16, col='blue')

for(i in 2:length(ageBins)){
  emp3f[i-1] = mean(Y[(data$pclass=='3rd')&(data$sex=='female')&
                        (data$age>=ageBins[i-1])&(data$age<=ageBins[i])])
}
points(emp3f~c(ageBins[-1]-2.5), pch=16, col='black')



## ----prb2lab4, message=FALSE, results=FALSE------------------------------------------------------------------------------------------------------

model2 = h2o.deeplearning( x = 2:4, y=1, standardize = TRUE,
                           hidden = c(10,10),
                           l2=0.5,
                           activation = 'Tanh',
                           distribution = 'Bernoulli',
                           loss = 'CrossEntropy',
                           reproducible = TRUE,
                           seed = 2,
                           rate = 0.001,
                           epochs = 1000,
                           adaptive_rate = FALSE,
                           training_frame = data_train,
                           validation_frame = data_val)

err.report = h2o.logloss(model2, train = T, valid = T)
err.report

XnewP1 = data.frame(pclass='1st', sex='female', age=ageSeq)
XnewP2 = data.frame(pclass='2nd', sex='female', age=ageSeq)
XnewP3 = data.frame(pclass='3rd', sex='female', age=ageSeq)

predP1 = h2o.predict(model2, as.h2o(XnewP1))
predP1 = as.data.frame(predP1)

predP2 = h2o.predict(model2, as.h2o(XnewP2))
predP2 = as.data.frame(predP2)

predP3 = h2o.predict(model2, as.h2o(XnewP3))
predP3 = as.data.frame(predP3)

plot(predP1$survived~XnewP1$age, type='l', col='red', ylim=c(0,1), xlab = 'Age',
     ylab = 'Probability of Survival',
     main = 'Probability of Sruvival with respect to age')
lines(predP2$survived~XnewP2$age,col='blue')
lines(predP3$survived~XnewP3$age, col = 'black')
legend('topright', lty=1, legend = c('1st Class', '2nd Class', '3rd Class'),
       col = c('red', 'blue', 'black'), bty='o')



## ----prb3lab4, message=FALSE, results=FALSE------------------------------------------------------------------------------------------------------

l2_set = c(0.0001, 0.001, 0.01, 0.1)

val_err = rep(NA, 4)

for(i in 1:4){
  model3 = h2o.deeplearning(x = 2:4, y=1, standardize =TRUE,
                         training_frame = data_train,
                         validation_frame = data_val,
                         hidden = c(5,3),
                         activation = 'Tanh',
                         distribution = "bernoulli",
                         loss = "CrossEntropy",
                         adaptive_rate = FALSE,
                         rate=0.001, epochs = 1000, l2 = l2_set[i],
                         reproducible = TRUE, seed=2)
  v_err = h2o.logloss(model3, valid = T)
  val_err[i] = v_err
}

plot(val_err ~ l2_set, pch=16, ylim = range(val_err), xlab="l2 regularisation value",
     ylab = "Validation Error",
     main = "Validation Error against changing l2 regulartisation values")
lines(val_err ~ l2_set)
text(val_err ~ l2_set, labels=l2_set, pos=4)


