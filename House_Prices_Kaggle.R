# house price data from  Kaggle

# lots of missing data
sum(is.na(train))

# also in this data set there is a mix of numeric and a factor or categorial features
# so you need a 

install.packages('randomForest')

library(randomForest)
   
train <- read.csv('~/House Prices/train.csv', stringsAsFactors = FALSE)
test <- read.csv('~/House Prices/test.csv', stringsAsFactors = FALSE)

# missing values


df <- rbind(train[-81],test)

NAcol <- which(colSums(is.na(df))>0)

# sums all the missing values
sort(colSums(sapply(df[NAcol],is.na)),decreasing = TRUE)

# quick way to fix them


# remember 'SalesPrice' is the resonse variable and is only in
# the training set

variables <- names(train)
variables <- variables[variables != 'SalePrice']
length(variables)
print(variables)

# replace missing values in the test and train data

for(variable in variables)
{
  if(any(is.na(train[[variable]])))
  {
    print(paste(variable,"-",class(train[[variable]])))
    if(is.character(train[[variable]]))
    {
      train[[variable]][is.na(train[[variable]])] <- "Missing"
    }
    else
    {
      train[[variable]][is.na(train[[variable]])] <- mean(train[[variable]],na.rm=TRUE)
    }
  }
  if(any(is.na(test[[variable]])))
  {
    if(is.character(test[[variable]]))
    {
      test[[variable]][is.na(test[[variable]])] <- "Missing"
    }
    else
    {
      test[[variable]][is.na(test[[variable]])] <- mean(test[[variable]],na.rm=TRUE)
    }
  }
}

# lets check data set for missing values

df <- rbind(train[-81],test)

NAcol <- which(colSums(is.na(df))>0)

# sums all the missing values

sum(is.na(df)) # if is 0 then we have no missing values

# Deal wit the factors

for(variable in variables)
{
  if(is.character(train[[variable]]))
  {
    levels <- sort(unique(c(train[[variable]],test[[variable]])))
    train[[variable]] <- factor(train[[variable]],levels=levels)
    test[[variable]] <- factor(test[[variable]],levels=levels)
  }
}

# so we add the factors back in!

str(train)
str(test)

# lets build a model

# first randomForest model

rf <- randomForest(SalePrice~.,train)

# test prediction on the test set

# this produces a vector of predicted sales values based on test set of predictors

p <- predict(rf,test)

# submitting to kaggle

# remember this is a competition so you dont see the actual dep var
# you simply predict the dep var on the test set by using a model
# trained on the training set
# if you avoided overfitting your training set you should get a better
# score using your model on the test set, you submit and see yout score

getwd()
setwd('~/House Prices')
getwd()

#example of format needed to submit to kaggle
submission <- read.csv('sample_submission.csv',stringsAsFactors = FALSE)

#overwrite column of data

submission$SalePrice <- p

write.csv(submission,file='submission.csv',row.names = FALSE)

# now for some fine tuning

# lets use random forest to recomend important features

varImpPlot(rf)

# lets build an OLS model based on the variable imporatance plot from randomForest package






