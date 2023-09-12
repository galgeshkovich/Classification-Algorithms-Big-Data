
## Automatic hyperparameter tuning
##using caret library 



## read the data

bank.df <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE)
head(bank.df)
bank.df$Personal.Loan <- as.factor(bank.df$Personal.Loan)


## split the data

set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) #take 3000 numbers between 1:5000
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]


## check list of parameters available

library(caret)
modelLookup("nnet")


# create a control object

ctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3, selectionFunction = "oneSE") #10-fold cross validation, repeat 3 times

# set the parameters grid

grid <- expand.grid(size = c(3, 5, 10), decay = c(0.5, 0.1, 1e-2, 1e-3, 1e-4)) #another option that takes less time - decay = c(0.5, 0.1, 1e-2)
#grid <- expand.grid(size = seq(from = 2, to = 5, by = 1), #example of sequence
#decay = seq(from = 0.01, to = 0.1, by = 0.01)) 


#build the model using the control and grid we created before

set.seed(1)
m <- train(Personal.Loan ~ ., data = train.df, method = "nnet",
           metric = "Kappa",
           trControl = ctrl,
           tuneGrid = grid,
           maxit = 500)

m$bestTune # optimal model parameters


# predict on new data

pred <- predict(m, newdata = valid.df)

# confusion matrix

confusionMatrix(pred, valid.df$Personal.Loan)
