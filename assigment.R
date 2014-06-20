trainset <- read.csv("data/pml-training.csv")
testset <- read.csv("data/pml-testing.csv")


# tasks:
# preprocessing:

# 1. identify columns with 100% of NA vals ain test set and remove


# plot distribaution of NA vals by columns:
table(apply(testset, 2, FUN=function(x) sum(is.na(x))))

# get names of columns with complete NA in test set:
ind_not_na_columns <- which(apply(testset, 2, FUN=function(x) sum(is.na(x))) == 0)


trainset <- trainset[ , ind_not_na_columns]
testset <- testset[ ,ind_not_na_columns]

time_diff <- trainset$raw_timestamp_part_1 - trainset$raw_timestamp_part_2
hist(time_diff)

table(trainset$new_window)
table(trainset$classe)
hist(trainset$num_window)

table(trainset$user_name)
table(testset$user_name)

cut_train  <- trainset[ , -c(1:7)]

library(caret)
set.seed(445)

inTrain = createDataPartition(cut_train$classe, p = 0.8, list = FALSE)
train = cut_train[ inTrain,]
cv = cut_train[-inTrain,]

str(train)
model_short <- train(classe ~., data = train, preProcess="pca", method ="lda")
print(model_short$finalModel)

pred_vec <- predict(model_short, cv)
table(pred_vec, cv$classe)
confusionMatrix(pred_vec, cv$classe)

pred_vec_fin <- predict(model_short, testset[ , -c(1:7)])

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(pred_vec_fin)
