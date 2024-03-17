#' ---
#' title: "Week 9 knn lab"
#' author: "Jonathan Forte"
#' ---
#' 
#' ## Perform a k-NN prediction on a new data after training the training data with all 11 predictors (ignore the MEDV
#' ##column), trying values of k from 1 to 10. Make sure to normalize the data, and
#' ##choose function knn() from package class rather than package FNN. To make sure
#' ##R is using the class package (when both packages are loaded), use class::knn(). 
#' ##What is the best k? What does it mean? 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(caret)
df <-read.csv("BostonHousing.csv")
#delete MEDV
df <-df[,-13]
#delete RAD
df <-df[,-9]

#delete CHAS
df <- df[,-4]


# set.seed value as 1
set.seed(1)

# ----------Q3: make line-by-line annotation for the following five lines of codes.--------------

# This line takes a random sampling of 60% of the integer values up to the number of rows in the df dataframe and stores them in this index matrix
train.index <- sample(row.names(df), 0.6*dim(df)[1]) 

#This takes the remaining integer values that the train.index matrix did not take.
valid.index <- setdiff(row.names(df), train.index)  

# This takes the selected rows from df and the training index and assigns them to a training data frame.
train.df <- df[train.index, ]

# This takes the selected rows from df and the validation index and assigns them to a validation data frame.
valid.df <- df[valid.index, ]

# This transforms the CAT..MEDV character column variable into a factor column variable
valid.df$CAT..MEDV <-as.factor(valid.df$CAT..MEDV)

#' 
#' 
#' new data 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
new.df <- data.frame(
CRIM=0.10469,
ZN=0,
INDUS=15,
NOX=0.4,
RM=7,
AGE=49,
DIS=4.78,
TAX=254,
PTRATIO=21,
LSTAT=12
)


#' 
#' 
#' prepare for normalizing training, validation data, complete data frames to originals
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
train.norm.df <- train.df
valid.norm.df <- valid.df
norm.df <- df

#' 
#' use preProcess() from the caret package to normalize predictor variables. Check out the normalized data.
#' method = "center" subtracts the mean of the predictor's data (again from the data in x) from the predictor values while method = "scale" divides by the standard deviation. https://www.rdocumentation.org/packages/caret/versions/6.0-90/topics/preProcess 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# ----------Q4: write your code to preProcess the predictor variables from the train data---------------------------------   
norm.values <- preProcess(train.df[,1:10], method=c("center", "scale"))



train.norm.df[, 1:10] <- predict(norm.values, train.df[, 1:10])
valid.norm.df[, 1:10] <- predict(norm.values, valid.df[, 1:10])
norm.df[, 1:10] <- predict(norm.values, df[, 1:10])
new.norm.df <- predict(norm.values, new.df)

#' 
#' use knn() to compute knn. 

 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(class)

pred_nn <- knn(train = train.norm.df[, 1:10], test = new.norm.df, 
          cl = train.norm.df[, 11], k = 2)

pred_nn


## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# initialize a data frame with two columns: k, and accuracy.
accuracy.df <- data.frame(k = seq(1, 10, 1), accuracy = rep(0, 10))

# ----------Q5: annotate the following lines of code--------------------------------------------------

# This line creates a vector of values where each value is created by using the three closest data points 
knn.pred <- knn(train.norm.df[, 1:10], valid.norm.df[, 1:10],
           cl = train.norm.df[, 11], k = 3)

# This line transforms the CAT..MEDV variable into a factor
valid.norm.df$CAT..MEDV <-as.factor(valid.norm.df$CAT..MEDV)

# This measures the accuracy when k = 3. k is how many closest data points are used the predict classification
accuracy.df[3, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 11])$overall[1]

#' 
#' compute knn for different k on validation. 
#' 
## -------------------------------------------------------------------------------------------------------------------------------------------------------------------------
for(i in 1:20) {
  knn.pred <- knn(train.norm.df[, 1:10], valid.norm.df[, 1:10], 
                  cl = train.norm.df[, 11], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, valid.norm.df[, 11])$overall[1] 
}
accuracy.df

#-----------Q6: based on this accuracy.df matrix, what is the best value of k?----------------
# k = 4 is the best value.

#-----------Q7: using the optimal value of k, run the knn again (write your code below).  
knn(train.norm.df[, 1:10], valid.norm.df[, 1:10], cl = train.norm.df[, 11], k = 4)

#preparing for predicting the housing price of the new unknown data

new.rec <- data.frame(
  CRIM=0.53,
  ZN=0,
  INDUS=6.18,
  NOX=0.5,
  RM=8,
  AGE=83,
  DIS=3,
  TAX=307,
  PTRATIO=17,
  LSTAT=5
)

new.norm.rec <- predict(norm.values, new.rec)
new.norm.rec

#Q7.1 write your code to run knn one more time using the best k value, and predict the housing price of the new.rec data. 
bestKNN <- knn(train.norm.df[, 1:10], new.norm.rec[,1:10], cl = train.norm.df[, 11], k = 4)
bestKNN

#Q7.2 Answer this question: How does this new model predict the housing value of the "new.df" introduced early on? 
  #State your prediction results using dollars format instead of 0 or 1 values. 
  #Example: the new model predicted 0 (or 1) for the new.df, which means the the housing price of the new data is less/higher than $XXX.

knn(train.norm.df[, 1:10], new.norm.df[,1:10], cl = train.norm.df[, 11], k = 4)
