

## Big data

## 5 V's - volume, velocity, variety, veracity, value.

## using confidence interval for measures - measuring procedure time in r



## read data 

library(forecast)

toyota.df <- read.csv("ToyotaCorolla.csv", stringsAsFactors = TRUE)


# split the data

set.seed(1)  

train.index <- sample(toyota.df$Id, dim(toyota.df)[1]*0.6)  

train.df <- toyota.df[train.index, ]

valid.df <- toyota.df[-train.index, ]


# fit simple regression

reg <- lm(Price ~ Weight, data = train.df)


# evaluate

pred <- predict(reg, newdata = valid.df)

RMSE <- accuracy(pred, valid.df$Price)[2]

RMSE ## 2,977 dollars



# create function to compute RMSE

ComputeRMSE <- function(seed){
  set.seed(seed)  
  train.index <- sample(toyota.df$Id, dim(toyota.df)[1]*0.6)  
  train.df <- toyota.df[train.index, ]
  valid.df <- toyota.df[-train.index, ]
  
  
  # again fit simple regression
  
  reg <- lm(Price ~ Weight, data = train.df)
  
  
  # evaluate
  pred <- predict(reg, newdata = valid.df)
  
  RMSE <- accuracy(pred, valid.df$Price)[2]
  
  return(RMSE)
}
ComputeRMSE(1)


# compute RMSE 10000 times
t1 <- proc.time()
RMSE_arr = c()
for (seed in c(1:10000)){
  RMSE_arr <- c(RMSE_arr, ComputeRMSE(seed))
}
t2 <- proc.time()
t2-t1  ## 40% time saved
boxplot(RMSE_arr) 


## parallel runs
### do the same - in parallel

library(doParallel)

# find how many cores are available
detectCores()

# create cluster with deired number of cores

cl <- makeCluster(3)

# register cluster

registerDoParallel(cl)

# find how many cores are being userd

getDoParWorkers()

# compute RMSE 1000 times

library(foreach)
t1 <- proc.time()
RMSE_arr <- foreach(seed = 1:10000, 
                    .combine = "c", 
                    .packages = "forecast") %dopar% ComputeRMSE(seed)
t2 <- proc.time()
t2-t1
boxplot(RMSE_arr)



## using Microsoft Azure 

## Map 1 - based optional input ports to variables
toyota.df <- maml.mapInputPort(1) # class: data.frame


library (forecast)

ComputeRMSE <- function(seed){
  set.seed(seed)
  train.index <- sample(toyota.df$Id, dim(toyota.df)[1]*0.6)
  train.df <- toyota.df[train.index, ]
  valid.df <- toyota.df[-train.index, ]
  reg <- lm(Price ~ Weight, data= train.df)
  pred <- predict(reg, newdata=valid.df)
  RMSE <- accuracy(pred, valid.df$Price)[2]
  return (RMSE)
}

RMSE_arr = c()

for (seed in c(1:1000){
  RMSE_arr <- c(RMSE_arr, ComputeRMSE(seed))
  
}

boxplot(RMSE_arr)


RMSE.df <- as.data.frame(RMSE_arr)
#Select data.frame to be sent to the output Dataset port
maml.mapOutputPort("RMSE_df");


