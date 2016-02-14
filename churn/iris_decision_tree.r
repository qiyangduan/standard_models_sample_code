# http://www.analyticsvidhya.com/blog/2014/06/comparing-cart-random-forest-1/


Sys.setenv(LANG = "en")


library(ggplot2) 
data(iris)
# look at the dataset
summary(iris)
# visually look at the dataset
qplot(Petal.Length,Petal.Width,colour=Species,data=iris)


library(rpart)
library(caret)

train.flag <- createDataPartition(y=iris$Species,p=0.5,list=FALSE)
training <- iris[train.flag,]
Validation <- iris[-train.flag,]


modfit <- train(Species~.,method="rpart",data=training) 
library(rattle)
fancyRpartPlot(modfit$finalModel)


train.cart<-predict(modfit,newdata=training)
table(train.cart,training$Species)
train.cart   setosa versicolor virginica


pred.cart<-predict(modfit,newdata=Validation)
table(pred.cart,Validation$Species)

correct <- pred.cart == Validation$Species
qplot(Petal.Length,Petal.Width,colour=correct,data=Validation)


