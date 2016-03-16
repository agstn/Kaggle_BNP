#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_BNP/Data/Data/Raw"
  loc_out  <- "C:/Git/Kaggle_BNP/Data/Anly/S01"
} else {
  loc_in   <- "/home/acalatroni/Kaggle_BNP/Data/Raw"
  loc_out  <- "/home/acalatroni/Kaggle_BNP/Anly/S01"
}

#' Start h2o
h2o.init(nthreads=-1)

#' Import Data
train_h2o <- as.h2o(train2, destination_frame = "train.hex")
test_h2o  <- as.h2o(test2,  destination_frame = "test.hex")

#' Splits
splits = h2o.splitFrame(train_h2o, 0.9, destination_frames=c("trainSplit","testSplit"))

#' Setup
y      <- "target"
x      <- setdiff(names(train_h2o[,-1]), y)
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
print(perf,metric="logloss")

#' Base learner test set AUC (for comparison)
L <- length(learner)
auc <- sapply(seq(L), function(l) perf$base[[l]]@metrics$AUC)
data.frame(learner, auc)

#' Predict
# p <- predict(fit, splits[[2]])
# labels = as.data.frame(splits[[2]][,"target"])

p       <- as.data.frame(predict.h2o.ensemble(fit,test))
testIds <- as.data.frame(test$ID)

submission <- data.frame(cbind(testIds,p$p1))
colnames(submission)<-c("ID","PredictedProb")
write_csv(submission,paste0(loc_out,"/submission.csv"))