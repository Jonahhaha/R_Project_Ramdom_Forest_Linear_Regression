install.packages("randomForest")
install.packages("ISLR2")
library (tree)
library (ISLR2)
attach (Carseats)

guns=read.csv("Guns.csv",header= T)
head(guns)

mod = glm(violent~.,data = guns)
summary(mod)
mod2 = glm (violent~law+income,data = guns)
summary(mod2)
mod3=glm(violent~law+density+income+population+cauc,data= gun)
summary(mod3)
mod4=glm(violent~density+income+cauc+law,data= gun)
summary(mod4)
mod5=glm(violent~law+income+cauc,data= gun)
summary(mod5)
mod6=glm(violent~income,data= guns)
summary(mod6)

model_summ <-summary(mod)
mean(model_summ$deviance.resid^2)

par(mfrow=c(2,2))
plot(mod3)

library(randomForest)
guns= read.csv("Guns.csv",header=T)
summary(guns)
set.seed(100)
rf <-randomForest(violent~cauc+income+state+density+law+population,data=guns, ntree=500) 
print(rf)
floor(sqrt(ncol(guns) - 1))
mtry <- m(guns[-1],guns$violent, ntreeTry=500,
               stepFactor=2,improve=0.05, trace=TRUE, plot=TRUE)
best.m <- mtry[mtry[, 2] == min(mtry[, 2]), 1]
print(mtry)
print(best.m)
rf <-randomForest(violent~cauc+income+state+density+law+population,data=guns, mtry=8, importance=TRUE,ntree=500)
print(rf)
summary(rf)

#install.packages("randomForest")
library(randomForest)
#Evaluate variable importance
importance(rf)
plot(importance(rf))
varImpPlot(rf)
modelalt_summ <-summary(rf)
mean(modalt_summ^2)

summary(gun$law)
text(m3, pretty = 0)
library(tree)
tree.carseats <- tree (gun$violent~gun$income, data=gun)
plot (tree.carseats)
mod=(gun$violent~.)
summary (tree.carseats)
plot (tree.carseats,type = c( "uniform"))
text (tree.carseats , pretty = 0)
tree.carseats

library("lattice")
pairs(~ income + population + cauc + density, data = guns)

with(rf.cv, plot(n.var, error.cv))
install.packages("randomforest")
library("randomforest")
rf.cv <- rf.crossValidation(rf, guns[,3:7], p=0.10, n=99, ntree=500)

library("tidyverse")
library("caret")
set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
model <- train(violent ~., data = guns, method = "lm",
               trControl = train.control)
# Summarize the results
print(model)

set.seed(123) 
train.control <- trainControl(method = "cv", number = 10)
# Train the model
library(rpart)
model <- train(violent ~ .,
                  data = guns,
                  "rpart",
                  tuneLength = 9)
# Summarize the results
print(model)