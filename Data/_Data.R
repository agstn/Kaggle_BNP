#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_BNP/Data/Raw"
  loc_out  <- "C:/Git/Kaggle_BNP/Data/Derive"
} else {
  loc_in   <- "/home/ubuntu/github/AstraZeneca-Sanger/Results/Final/BASE2"
  loc_out  <- "/home/ubuntu/github/AstraZeneca-Sanger/Results/Final/BASE2"
}

#' libraries
pacman::p_load(h2o,h2oEnsemble,readr,dplyr)

#' Import data
test  <- read_csv(paste0(loc_in,"/test.csv.zip"))
train <- read_csv(paste0(loc_in,"/train.csv.zip"))

#' Combine
comb <- rbind(train[,-2],test)

#' Remove Near Zero
nzv <- caret::nearZeroVar(comb)

#' Start h2o
h2o.init(nthreads=-1)

#' Import Data
comb_h2o <- as.h2o(comb)

#' Impute Median/Mode
for (i in 2:132){
  if(is.factor(comb_h2o[,i])=="FALSE") h2o.impute(data = comb_h2o, column = i, method = "median")
  if(is.factor(comb_h2o[,i])=="TRUE")  h2o.impute(data = comb_h2o, column = i, method = "mode")
}

train <- h2o.uploadFile(paste0(loc_in,"/train.csv"), destination_frame = "train.hex")

train2 <- h2o.splitFrame(train, 0.8, destination_frames=c("trainSplit","testSplit"))
train2 <- train2[[2]]
train <- train2
#test  <- h2o.uploadFile(paste0(loc_in,"/test.csv"),  destination_frame = "test.hex")
#


#' create factor
train$target<-as.factor(train$target)


#' Impute Median/Mode
for (i in 3:133){
  if(is.factor(train[,i])=="FALSE") h2o.impute(data = train, column = i, method = "median")
  if(is.factor(train[,i])=="TRUE")  h2o.impute(data = train, column = i, method = "mode")
  print(i)
}

#' Impute Median/Mode
# for (i in 2:133){
#   if(is.factor(train[,i])=="FALSE") h2o.impute(data = test, column = i, method = "median")
#   if(is.factor(train[,i])=="TRUE")  h2o.impute(data = test, column = i, method = "mode")
#   print(i)
# }

#' Splits
splits = h2o.splitFrame(train, 0.9, destination_frames=c("trainSplit","testSplit"))

#' Setup
y      <- "target"
x      <- setdiff(names(train[,-1]), y)
family <- "binomial"

#' Specify the base learner & the metalearner
learner <- c("h2o.glm.wrapper",
             "h2o.randomForest.wrapper",
             "h2o.gbm.wrapper",
             "h2o.deeplearning.wrapper")

metalearner <- "h2o.glm.wrapper"

#' Ensemble training
fit <- h2o.ensemble(x = x,
                   y = y,
                   training_frame   = splits[[1]],
                   validation_frame = splits[[2]],
                   family = "binomial",
                   learner = learner,
                   metalearner = metalearner,
                   cvControl = list(V=5)
)

perf <- h2o.ensemble_performance(fit, newdata = splits[[2]])

#' Predict
p <- predict(fit, splits[[2]])
labels = as.data.frame(splits[[2]][,"target"])

# p <- as.data.frame(h2o.predict(gbm,test))
# testIds <- as.data.frame(test$ID)
# submission <- data.frame(cbind(testIds,p$p1))
# colnames(submission)<-c("ID","PredictedProb")
# write.csv(submission,"submission.csv",row.names=F)