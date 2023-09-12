

## Neural Networks - Deep Learning


## example - predicting cheese preference (like/dislike) by consumers.

df <- read.csv("example1.csv", stringsAsFactors = TRUE)



# library neuralnet - multipe hidden layers + flexible number of nodes in layer
# Fitting NN to data


library(neuralnet)
df$Like <- df$Acceptance == "like"
set.seed(1)
nn <- neuralnet(Like ~ Salt + Fat,
                data = df, linear.output = F, 
                hidden = 3)

plot(nn, rep="best") ## allows ploting the network


# prediction 


pred <- compute(nn, df[, 2:3])
pred.value <- pred$net.result

library(caret)
confusionMatrix(as.factor(ifelse(pred.value>0.5, "like", "dislike")), as.factor(df$Acceptance))
## Specificity of 1 !!! (for cutoff value 0.5)
## output - 0.67% - nice result.


# library nnet
## no plot and only one hidden layer
##but fast and standard R convention

library(nnet)
set.seed(1)
nn <- nnet(as.factor(Acceptance) ~ Salt + Fat,
           data = df, linout = F, 
           size = 3, 
           decay = 0.01, maxit = 200)

summary(nn) ## "plot" the network


# prediction

pred <- predict(nn, df)
library(caret)
confusionMatrix(as.factor(ifelse(pred>0.5, "like", "dislike")), as.factor(df$Acceptance))

## now both sensitivity and specificity are 1.




## universal bank data

bank.df <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE)
head(bank.df)

set.seed(1)
train.index <- sample(1:dim(bank.df)[1], dim(bank.df)[1]*0.6)
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]


## try 1: use Income and CCAvg, with 3 hidden nodes
set.seed(1)
nn1 <- nnet(Personal.Loan ~ Income + CCAvg, 
            data = train.df, size = 3, 
            decay = 0.01, maxit = 200) ## 140 iterations needed
pred1 <- predict(nn1, valid.df)

library(AUC)
r <- roc(pred1, as.factor(valid.df$Personal.Loan))
plot(r)
auc(r) ##0.9244


## try 2: use Income and CCAvg, normalize first
set.seed(1)
nn2 <- nnet(Personal.Loan ~ Income + CCAvg, 
            data = train.df, size = 10, 
            decay = 0.01, maxit = 200)  ## 130 iterations needed
pred2 <- predict(nn2, valid.df)

library(AUC)
r <- roc(pred2, as.factor(valid.df$Personal.Loan))
plot(r)
auc(r) ## 0.9239


## try 3: use all variables, 10 hidden nodes
set.seed(1)
nn3 <- nnet(Personal.Loan ~ ., 
            data = train.df, size = 10, 
            decay = 0.01, maxit = 500) ## 420 iterations
pred3 <- predict(nn3, valid.df)

library(AUC)
r <- roc(pred3, as.factor(valid.df$Personal.Loan))
plot(r)
auc(r) ## 0.98 - model fits well.
