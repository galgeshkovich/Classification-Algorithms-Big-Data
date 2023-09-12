
## mini assignment

library(AUC)

traindata.df <- read.csv("traindata.csv", stringsAsFactors = TRUE)

# remove missing values

traindata.df <- traindata.df[is.na(traindata.df$income) == F,]

# create new variable

traindata.df$HighIncome <- ifelse(traindata.df$income > mean(traindata.df$income), 1, 0)

# partition the data

set.seed(1)
train.index <- sample(1:dim(traindata.df)[1], dim(traindata.df)[1]*0.6)
train.df <- traindata.df[train.index, ]
valid.df <- traindata.df[-train.index, ]

# classification tree

library(party)
tr <- ctree(HighIncome ~ gender + age, data = train.df)
pred <- predict(tr, valid.df)
r <- roc(pred, as.factor(valid.df$HighIncome))
auc(r)

# neural network

library(nnet)
set.seed(1)
nn <- nnet(HighIncome ~ gender + age, 
           data = train.df, size = 3, 
           decay = 0.01, maxit = 400)
pred <- predict(nn, valid.df)
r <- roc(pred, as.factor(valid.df$HighIncome))
auc(r)

# logistic regression

reg <- glm(HighIncome ~ gender + age, 
           data = train.df, 
           family = "binomial")
pred <- predict(reg, valid.df, type = "response")
r <- roc(pred, as.factor(valid.df$HighIncome))
auc(r) #=1