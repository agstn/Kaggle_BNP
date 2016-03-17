#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_BNP/Data/Data/Derive"
  loc_out  <- "C:/Git/Kaggle_BNP/Data/Anly/S02"
} else {
  loc_in   <- "/home/acalatroni/Kaggle_BNP/Data/Derive"
  loc_out  <- "/home/acalatroni/Kaggle_BNP/Anly/S02"
}

#' Packages
pacman::p_load(pacman)
p_load(readr,dplyr)
p_load(h2o,h2oEnsemble)

#' Start h2o
h2o.init(nthreads=-1)

#' Import RDS files
train <- readRDS(paste0(loc_in,"/train.rds"))
test  <- readRDS(paste0(loc_in,"/test.rds"))

#' Import Data
train_h2o <- as.h2o(train, destination_frame = "train.hex")
test_h2o  <- as.h2o(test,  destination_frame = "test.hex")

#' Setup
y      <- "target"
x      <- setdiff(names(train_h2o[,-1]), y)
family <- "binomial"

#' Specify the base learner & the metalearner
source(paste0(loc_out,"/",'_base_learners.R'))

learner <- c("h2o.glm.1","h2o.glm.2","h2o.glm.3"
             ,
             "h2o.rf.11","h2o.rf.12","h2o.rf.13",
             "h2o.rf.21","h2o.rf.22","h2o.rf.23",
             "h2o.rf.31","h2o.rf.32","h2o.rf.33",
             "h2o.gbm.11","h2o.gbm.12",
             "h2o.gbm.21","h2o.gbm.22", 
             "h2o.deeplearning.1","h2o.deeplearning.2","h2o.deeplearning.3", 
             "h2o.deeplearning.4", "h2o.deeplearning.5","h2o.deeplearning.6",
             "h2o.deeplearning.7")

metalearner <- "h2o.deeplearning.wrapper"

#' Ensemble training
fit <- h2o.ensemble(x = x,
                    y = y,
                    training_frame   = train_h2o,
                    family = "binomial",
                    learner = learner,
                    metalearner = metalearner,
                    cvControl = list(V=5)
)

#' Predict
p       <- predict.h2o.ensemble(fit,test_h2o)
p1      <- as.vector(p$pred[,"p1"])

submission <- data.frame(ID=test$ID,PredictedProb=p1)
write_csv(submission,paste0(loc_out,"/submission.csv"))
