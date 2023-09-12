
## Classification Trees



## The universal bank runs a campaign to offer a personal loan to customers.
## The bank would like to improve its campaign strategy, based the acceptance data from previous campaign
## In the previous campaign only 4.8% of the customers accepted the loan.
## Data was collected on a sample of 5,000 customers.



## read the data
bank.df <- read.csv("UniversalBank.csv", stringsAsFactors = TRUE)
head(bank.df)



## split the data
set.seed(1)  
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]



## build tree based on Recursive Partition
library(party)
tr <- ctree(Personal.Loan ~ ., data = train.df)
plot(tr, type = "simple") ## Splits based on the association and Chi squared test.
pred <- predict(tr, newdata = valid.df)



## confusion matrix
## Summarize the correct and incorrect classification that a classifier produced for a certain dataset.
library(caret)
confusionMatrix(as.factor(ifelse(pred>=0.5, 1, 0)), as.factor(valid.df$Personal.Loan))

# The sensitivity - the ability to detect the important class members only (0.9955)
# The specificity - the ability to rule out the other's group members correctly (0.8439)
# Based on their tradeoff I can decide the cutoff value (0.5 in this time)


## ROC curve
## ROC - Reciever Operating Characteristic
## AUC - Area under the curve


library(AUC)
r <- roc(pred, as.factor(valid.df$Personal.Loan))
auc(r) # 0.99363 - good result. higher auc is better.
plot(r)




## Random Forest

## Solves the Heuristic and Local optimization probmels.
## Using k-fold cross validation, fit multiple trees to different subsets of the data, then aggregate performance measures.

library (party)
library (AUC)

set.seed(2)
train.index <- sample(c(1:dim(bank.df)[1]), dim(bank.df)[1]*0.6) 
train.df <- bank.df[train.index, ]
valid.df <- bank.df[-train.index, ]

rf <- cforest(Personal.Loan ~ ., data = train.df)
pred <- predict(rf, newdata = valid.df)
r <- roc(pred, as.factor(valid.df$Personal.Loan))
auc(r) ## 0.9972 - better than classification tree.
plot(r)

varImp(rf)


