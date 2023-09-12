
## Parallel loan prediction

##read the data

bank.df <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE)
head(bank.df)

##auc function

ComputeAUC <- function(seed){
  set.seed(seed)  
  train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6)  
  train.df <- bank.df[train.index, ]
  valid.df <- bank.df[-train.index, ]
  
  library(party)
  tr <- ctree(Personal.Loan ~ ., data = train.df)
  pred <- predict(tr, newdata = valid.df)
  
  library(AUC)
  r <- roc(pred, as.factor(valid.df$Personal.Loan))
  
  return(auc(r))
}

library(doParallel)

detectCores()
cl <- makeCluster(7) ##in my computer i have 8 cores, select minus 1
registerDoParallel(cl)
getDoParWorkers()

library(foreach)
t1 <- proc.time()
AUC_arr <- foreach(seed = 1:1000, 
                   .combine = "c", 
                   .packages = "forecast") %dopar% ComputeAUC(seed)
t2 <- proc.time()
t2-t1  ## elapsed time 11.5 sec
mean(AUC_arr) ## 0.9932464 - nice value
