"""Consider the Default data set already analyzed in Tutorato #1.
. Plot some relevant numerical or graphical summaries of the data, showing the discriminative power of
the predictors.
. Split the data randomly into train/test.
. Perform a logistic regression model on the training data to predict default and calculate the test error.
. Perform LDA on the training data to predict default and calculate the test error of this model.
. Perform QDA on the training data to predict default and calculate the test error of this model.
. You will notice that the error rates are all rather small. Is this a sign of very good models? Reflect on
this by: 1) Calculating the performance of a null model that predicts always to the non-default class 2)
Calculating specificity and sensitivity of the models from the confusion matrix. What do you conclude?
  . Given that the class variable is very unbalanced, it is more informative to compare the methods based
on the ROC curve. Plot the ROC curves of each model, computing also the areas under the curves
(AUCs). Based on the results, which method would you use for future predictions?
Hints
. LDA and QDA are implemented in the functions lda(), qda() within the package MASS. Their syntax
is the same as that of glm or lm."""

library(ISLR)
df <-  ISLR::Default
attach(df)
summary(df)
par(mfrow=c(1,2))
boxplot(default,balance)
boxplot(student,balance)
######################
size_tt <-  floor(0.5*nrow(df))
set.seed(1)
train_ind <- sample(seq_len(nrow(df)), size = size_tt)
train <-  df[train_ind,]
test <-  df[-train_ind,]
########################
glm.fits <-  glm(default~.,data = train, family = binomial)
summary(glm.fits)
prediction <-  predict(glm.fits, test, type = 'response')
glm.pred <-  rep('No', nrow(test))
glm.pred[prediction > 0.5] <-  'Yes'
table(glm.pred, test$default)
mean(glm.pred == test$default)
test_error_1 <-  1 - mean(glm.pred == test$default)
############################
library(MASS)
lda.fits <-  lda(default~.,data = train)
lda.fits
prediction_2 <-  predict(lda.fits, test, type = 'response')
prediction_2$posterior
prediction_2$class
lda.pred <- rep('No', nrow(test))
lda.pred[prediction_2$posterior[,2] > 0.5] <-  'Yes'
table(lda.pred, test$default)
mean(prediction_2$class == test$default)
contrasts(prediction_2$class)

#mean(lda.pred == test$default )
############################
qda.fits <-  qda(default~.,data = train)
qda.fits
prediction_3 <-  predict(qda.fits, test, type = 'response')
prediction_3$
qda.pred <- rep('No', nrow(test))
qda.pred[prediction_3$posterior[,2] > 0.5] <-  'Yes'
table(qda.pred, test$default)
mean(prediction_3$class == test$default)
#mean(qda.pred == test$default)
######################à####     
lda.fits_ <-  lda(default~.,data = train)
lda.fits_
prediction_4 <-  predict(lda.fits_, test, type = 'response')
prediction_4$posterior
lda.pred_ <- rep('No', nrow(test))
table(lda.pred_, test$default)
mean(lda.pred_ == test$default)
#################
sensitivity <-   116/(41+116) # TP/(TP+FN)
specificity <- 11/(11+4832)  #TN/(TN+FP)
############################
par(mfrow=c(2,2))
install.packages('ROCR')
library(ROCR)
predict1 <-  ROCR::prediction(prediction, test$default)
perf <-  ROCR::performance(predict1,'tpr','fpr')
plot(perf)
abline(0,1,col='blue', lwd=3,lty=2)

predict2 <-  ROCR::prediction(prediction_2$x, test$default)
perf2 <-  ROCR::performance(predict2,'tpr','fpr')
plot(perf2)
abline(0,1,col='red', lwd=3,lty=2)

predict3 <-  ROCR::prediction(prediction_3$posterior[,2], test$default)
perf3 <-  ROCR::performance(predict3,'tpr','fpr')
plot(perf3)
abline(0,1,col='green', lwd=3,lty=2)

predict4 <-  ROCR::prediction(prediction_4$x, test$default)
perf4 <-  ROCR::performance(predict4,'tpr','fpr')
plot(perf4)
abline(0,1,col='purple', lwd=3,lty=2)



