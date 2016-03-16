#' Parameters
if (Sys.info()['sysname']=="Windows") {
  loc_in   <- "C:/Git/Kaggle_BNP/Data/Raw"
  loc_out  <- "C:/Git/Kaggle_BNP/Data/Derive"
} else {
  loc_in   <- "~/Kaggle_BNP/Data/Raw"
  loc_out  <- "/home/acalatroni/Kaggle_BNP/Data/Derive"
}

#' libraries
pacman::p_load(pacman)
p_load(readr,dplyr)

#' Import data
test  <- read_csv(paste0(loc_in,"/test.csv.zip"))
train <- read_csv(paste0(loc_in,"/train.csv.zip"))

#' Combine
comb <- rbind(train[,-2],test)

#' Remove Near Zero
nzv <- caret::nearZeroVar(comb)
comb <- comb[,-nzv]

#' Create Factors
l1 <- apply(comb,2,function(x) length(unique(x)))
comb[,which(l1<10)] <- lapply(comb[,which(l1<10)], as.factor)

#' Converts characters to factors
comb[sapply(comb, is.character)] <- lapply(comb[sapply(comb, is.character)],as.factor)

#' Impute median/mode
comb <- randomForest::na.roughfix(comb)

#' Export
train2 <- comb[1:nrow(train),]
train2$target <- as.factor(train$target)
saveRDS(train2,paste0(loc_out,"/train.rds"))

test2  <- comb[(nrow(train)+1):nrow(comb),]
saveRDS(test2,paste0(loc_out,"/test.rds"))
