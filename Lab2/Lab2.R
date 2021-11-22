## ----setup, include=FALSE------------------------------------------------------------------------------------------------------------------------
rm(list = ls())
library(rpart)
library(rpart.plot)
#par(col.axis = 'white',col.lab = 'white',col.main = 'white',col.sub = 'white',fg = 'white',bg = 'black', lwd = 2)
knitr::opts_chunk$set(echo = TRUE)


## ----sinusoidal----------------------------------------------------------------------------------------------------------------------------------
set.seed(2021)
N = 250
X = runif(N, -1, 1)
e = rnorm(N, 0, 0.25)
Y = sin(2*pi*X) + e
plot(Y~X, pch=16)



## ----task1---------------------------------------------------------------------------------------------------------------------------------------
res = rpart(Y~X, control = rpart.control(cp=0.01))

plotcp(res, upper=c("splits"))

printcp(res)

res_pruned = prune.rpart(res, cp=0.024)



## ----task2---------------------------------------------------------------------------------------------------------------------------------------

M =100
X_dummy = seq(min(X), max(X), length=M)
Lat = data.frame(X = X_dummy)

plot(Y~X, pch=16)
predY = predict(res, Lat)
predY_pruned = predict(res_pruned, Lat)

lines(predY~Lat$X, col="limegreen", lwd=3)
lines(predY_pruned~Lat$X, col="red", lwd=3)



## ----task1ex2------------------------------------------------------------------------------------------------------------------------------------

data(ptitanic)
attach(ptitanic)

Y = (survived == 'survived')

mean(Y[sex=='male'])
mean(Y[sex=='female'])

mean(Y[pclass=='1st'])
mean(Y[pclass=='2nd'])
mean(Y[pclass=='3rd'])

mean(Y[(pclass=='1st')&(sex=='male')])
mean(Y[(pclass=='2nd')&(sex=='male')])
mean(Y[(pclass=='3rd')&(sex=='male')])

mean(Y[(pclass=='1st')&(sex=='female')])
mean(Y[(pclass=='2nd')&(sex=='female')])
mean(Y[(pclass=='3rd')&(sex=='female')])






## ----task2ex2------------------------------------------------------------------------------------------------------------------------------------

set.seed(2021)
model = rpart(survived~., data = ptitanic, control= rpart.control(cp=0.001))
plotcp(model, upper=c("splits"))

model_pruned = prune.rpart(model, cp=0.013)

rpart.plot(model, type=4, fallen.leaves=F, branch=1)
rpart.plot(model_pruned, type=4, fallen.leaves = F, branch=1)


