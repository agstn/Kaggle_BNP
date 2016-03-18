#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_BNP/Data/Raw"
  loc_out  <- "C:/Git/Kaggle_BNP/Data/Derive"
} else {
  loc_in   <- "~/Kaggle_BNP/Data/Raw"
  loc_out  <- "~/Kaggle_BNP/Data/Derive"
}

#' libraries
pacman::p_load(pacman)
p_load(caret)
p_load(readr,dplyr)

#' Import data
test  <- read_csv(paste0(loc_in,"/test.csv.zip"))
train <- read_csv(paste0(loc_in,"/train.csv.zip"))
target <- train$target
train <- select(train,-target)

#' Remove constant columns
train <- train[sapply(train, function(x) length(unique(na.omit(x)))) > 1]

#' Remove duplicate columns
train <- train[!duplicated(lapply(train,summary))]

#' Create Factors
l1 <- apply(train,2,function(x) length(unique(na.omit(x))))
train[,which(l1<10)] <- lapply(train[,which(l1<10)], as.factor)

#' Converts characters to factors
train[sapply(train, is.character)] <- lapply(train[sapply(train, is.character)],as.factor)

#' Impute median/mode
train <- randomForest::na.roughfix(train)

#' Export
test <- test[,names(train)]
saveRDS(test,paste0(loc_out,"/test.rds"))

train <- cbind(train,target)
saveRDS(train,paste0(loc_out,"/train.rds"))
