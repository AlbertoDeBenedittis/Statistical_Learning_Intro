---
title: "Exercise 5"
output:
  html_document:
    df_print: paged
---
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

```

# Exercise 1 
Use the validation set approach to estimate the test error of a logistic regression model that predicts
default from income and balance. Follow these steps:
## 1. Split the data into training and validation;
```{r}
library(ISLR)
df1 <-  ISLR::Default
set.seed(99)
train_in <- sample(nrow(df1), 0.5*nrow(df1))
train <-  df1[train_in,]
test <-  df1[-train_in,]
```
## 2. Fit a model using only the training observations;
```{r}
glm.fit <- glm(default ~ income + balance, family = 'binomial', data = train)
summary(glm.fit)
```
From the analysis of the results of the logistic regression we deduce that both the income and the balance are relevant factor for the prediction of the default.  

# 3. Obtain predictions on the validation set (ie compute the posterior probabilities and threshold them using a chosen threshold);
```{r}
glm.prob <-  predict(glm.fit, test, type = 'response')
treshold <-  .5
glm.pred <-  rep('No', nrow(test))
glm.pred[glm.prob > treshold] <-  'Yes'
table(glm.pred, test$default)
mean(glm.pred == test$default)
```

4. Compute the validation set error.
```{r}
mean(glm.pred != test$default)
```
5. Repeat 1-3 a number of times, using different training/validation splits. If you can, try to write this as a for loop. Inspect the different test errors and discuss the results.
```{r}
accuracies <-  c()
errors <-  c()
ticks <-  c()
for (i in 1:5){
  set.seed(i)
  ticks <-  append(ticks, sprintf("Try %d",i))
  train_in <- sample(nrow(df1), 0.5*nrow(df1))
  train <-  df1[train_in,]
  test <-  df1[-train_in,]
  glm.fit <- glm(default ~ income + balance, family = 'binomial', data = train)
  glm.prob <-  predict(glm.fit, test, type = 'response')
  treshold <-  .5
  glm.pred <-  rep('No', nrow(test))
  glm.pred[glm.prob > treshold] <-  'Yes'
  table(glm.pred, test$default)
  accuracies <- append(accuracies, mean(glm.pred == test$default))
  errors <-  append(errors, mean(glm.pred != test$default))
  
}

print(cbind(ticks,accuracies,errors))
mean_accuracy <- (mean(accuracies))
print(mean_accuracy)
print(sd(accuracies))**2
```
As we can see, the results are pretty similar with each other the variace between the 5 results is pretty low and we are quite satisfied with this result.  


• Now consider a logistic regression model that predicts the probability of default from income, balance, and a dummy variable for student. Estimate the average test error using the validation set approach as before. Is there an advantage in including student?

```{r}
accuracies <-  c()
errors <-  c()
ticks <-  c()
for (i in 1:5){
  set.seed(i)
  ticks <-  append(ticks, sprintf("Try %d",i))
  train_in <- sample(nrow(df1), 0.5*nrow(df1))
  train <-  df1[train_in,]
  test <-  df1[-train_in,]
  glm.fit <- glm(default ~ income + balance + student, family = 'binomial', data = train)
  glm.prob <-  predict(glm.fit, test, type = 'response')
  treshold <-  .5
  glm.pred <-  rep('No', nrow(test))
  glm.pred[glm.prob > treshold] <-  'Yes'
  table(glm.pred, test$default)
  accuracies <- append(accuracies, mean(glm.pred == test$default))
  errors <-  append(errors, mean(glm.pred != test$default))
  
}

print(cbind(ticks,accuracies,errors))
mean_accuracy <- (mean(accuracies))
print(mean_accuracy)
print(sd(accuracies))**2
```
Well, from the results we can spot any difference in the accuracy between the logistc model with the dummy variable student. 
Hence, we prefer the first model since it is less complex and produce the same results.
   
## Excercise 2  
In this exercise, you will explore cross-validation in a regression context.
A) Simulate a data set using randomly generated numbers from a normal distribution with mean 0 and variance 1, as follows:
```{r}
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)
```
Write down the model used to generate data in equation form. What is n and what is p?  
__COMMENT__
B) Create a scatterplot of x vs y. Comment on the relationship that you see
```{r}
plot(x,y)
lines(mean(x), mean(y))
```
As we can see from the plot the points follow a normal distirbution as we expected since they were created from a normal distribution. <br>
We can also spot 





