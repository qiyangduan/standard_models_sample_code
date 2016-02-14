rm(list=ls())
apply_file = "C:\\qduan\\Stanmo\\git\\bitbucket\\src\\stanmo_website_proj\\app\\static\\data\\churn_sample_apply.csv"
train_file = "C:\\qduan\\Stanmo\\git\\bitbucket\\src\\stanmo_website_proj\\app\\static\\data\\churn_sample_input.csv"
churn_data = read.csv(train_file, fill = TRUE) # 1 column

# Convert 1/0 to T/F since Caret do regression instead of classification over numerical values
churn_data[churn_data$X_churn_flag == 1,]$X_churn_flag = "T"
churn_data[churn_data$X_churn_flag == 0,]$X_churn_flag = "F"
# drop customer id column
drops <- c("X_customer_id")
train_data=churn_data[,!(names(churn_data) %in% drops)]
rm(churn_data)
# head(train_data,3)


# Classification Tree with rpart
library(rpart) 
library(caret)


# grow tree 
churn_model <- train(X_churn_flag~., method="rpart",data=train_data) 
#printcp(churn_model) # display the results 
#summary(churn_model) # detailed summary of splits

# http://topepo.github.io/caret/modelList.html
# choose a better algorithm


# plot tree 
library(rattle)
fancyRpartPlot(churn_model$finalModel)

train.predicted<-predict(churn_model,newdata=train_data)
table(train.predicted,train_data$X_churn_flag)

## save this model
save(churn_model, file = "my_churn_model_dt.rda")
# getwd()
rm(churn_model)


## load the model
load("my_churn_model_dt.rda")

## a month later, we receive customer profile for a new month: 
churn_apply = read.csv(apply_file, fill = TRUE) # 1 column
# Run exactly same transformation as training data.
churn_apply[churn_apply$X_churn_flag == 1,]$X_churn_flag = "T"
churn_apply[churn_apply$X_churn_flag == 0,]$X_churn_flag = "F"
apply_data=churn_apply[,!(names(churn_apply) %in% drops)]


apply.predicted<-predict(churn_model,newdata=apply_data)
table(apply.predicted,apply_data$X_churn_flag)


