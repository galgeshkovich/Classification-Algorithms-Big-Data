
## Prediction of casualties in road accidents

##read the data

accidents.df <- read.csv("accidents.csv", stringsAsFactors = TRUE)

## Data partition

set.seed(1)

train.index <- sample(1:dim(accidents.df)[1], dim(accidents.df)[1]*0.6)

train.df <- accidents.df[train.index, ]

valid.df <- accidents.df[-train.index, ]


## create binary variable for prediction

train.df$Casualties <- ifelse(train.df$MAX_SEV_IR > 0, 1, 0)

valid.df$Casualties <- ifelse(valid.df$MAX_SEV_IR > 0, 1, 0)



# classification tree


library(party)

tr <- ctree(Casualties ~ ALCHL_I + SUR_COND + PROFIL_I_R + VEH_INVL, data = train.df)

plot(tr , type ="simple")

## for example - in node 13, there are 132 observations where the road profile is 1.

## in node 11, there are 29 observations with a chance of 0.655% to casualties.

pred <- predict(tr, valid.df)


library(AUC)

r <- roc(pred, as.factor(valid.df$Casualties))


plot(r)

auc(r) ## 0.971


# neural network

library(nnet)

set.seed(1)

nn <- nnet(Casualties ~ ALCHL_I + SUR_COND + PROFIL_I_R + VEH_INVL, 
           data = train.df, size = 3, 
           decay = 0.01, maxit = 500) ## 130 iterations


pred2 <- predict(nn, valid.df)

r2 <- roc(pred2, as.factor(valid.df$Casualties))


plot(r2)

auc(r2)  ## 0.972048


# Random Forest

set.seed(1)

rf <- cforest(Casualties ~ ALCHL_I + SUR_COND + PROFIL_I_R + VEH_INVL, data = train.df)

pred3 <- predict(rf, newdata = valid.df)

r3 <- roc(pred3, as.factor(valid.df$Casualties))

plot(r3)

auc(r3) ## 0.971



# Confusion matrix


library(caret)

confusionMatrix(as.factor(ifelse(pred2>=0.5, 1, 0)), as.factor(valid.df$Casualties))

## model accuracy - 0.945

## model sensitivity - 0.9585

## model specificity - 0.9290


## conclusion - neural network gave the best prediction - AUC of 0.972 !!



