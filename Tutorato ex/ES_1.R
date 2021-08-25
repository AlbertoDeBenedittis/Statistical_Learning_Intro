"""Exercise 1
In this exercise, you will investigate the use of a k-NN classifier on the Stock Market data used in the lab
lesson, and in particular you will investigate the bias/variance trade-off behind the choice of k.
. Split the Stock Market data into a train and test partition, using the same criterion adopted in the lab
lesson.
. Using Lag1 and Lag2 as predictors, fit a k-NN classifier with different values of k (for example, from 1
                                                                                       to 100).
. Evaluate the train and test errors of each model and plot them as a function of 1/k.
Hints
. To compute the train accuracy, you need to evaluate the knn on the data used for fitting the model.
Also, consider that knn() does not have distinct functions for fit and predict.
. The plot should be similar to Figure 2.17 in the textbook.
. You may also repeat the exercise using all of the Lag1 to Lag5 predictors."""

library(ISLR)
Smarket <-  ISLR::Smarket
train <-  (Year< 2005)
Smarket.2005 <-  Smarket[!train,]
Direction.2005 <-  Direction[!train]
library(class)
train.X <-  cbind(Lag1,Lag2)[train,]
train.X
test.x <-  cbind(Lag1, Lag2)[!train,]
train.Direction <-  Direction[train]
train.Direction
set.seed(1)
###
knn_pred <- function(tra, te, dir, n ){
  knn.pred <- knn(tra , te, dir, k = n )
  return(mean(knn.pred == Direction.2005))
}
knn_pred(train.X, test.X, train.Direction, 2)
tavola <- c()
for(i in 1:100){
  tavola <- c(tavola,knn_pred(train.X, test.X, train.Direction, i) )
} 
errore <- 1-tavola
errore
par(mfrow = c(1,1))
x <- c()
for(i in 1:100){
  x <- c(x, 1/i )
}
plot(x, errore, type='l',xlab = '1/k', ylab='Error rate')
plot(tavola, col = 'red')
lines(tavola)
lines(errore, col = 'purple')
##################################################################################
"""
In this exercise, you will investigate the use of interaction terms within a logistic regression model, in the
context of predicting the fate of passengers aboard the RMS Titanic.
. Use the R library "titanic", which contains a titanic_train dataframe.
. Select the variables "Pclass", "Sex", "Age" as predictors, and "Survived" as target.
. Explore the data: look especially for missing values (NAs) and remove rows containing them.
. Further split the titanic_train into train and test partitions using a simple criterion (e.g., into random
                                                                                           halves).
. With a logistic regression model, predict the fate of passengers (Survived) using age as a predictor.
. Plot the probability of survival on a new vector of ages.
. But, "women and children first"! It could be that age is not the only factor affecting survival.
- Revise the model using both age and gender as predictors; compute model performance on the
test set;
- Revise the model considering an interaction term between age and gender;
- For both revised models, plot the survival probabilities vs age stratified by gender: compare the
plots for the two different models. What happened?
Hints
. To remove rows containing missing values, use the function na.omit().
. Encode the target variable as a factor.
. An interaction term between var1 and var2 can be added to a model using the notation var1:var2
in the formula: target ~ var1:var2. A convenient way to simultaneously include var1, var2, and
1
their interaction as predictors is to use the notation var1*var2: target ~ var1*var2 is equivalent to
target ~ var1 + var2 + var1:var2.
"""
install.packages('titanic')
library('titanic')
df <- titanic::titanic_train
names(df)
summary(df)
df <-  na.omit(df)

714/2

df$Survived_status <-  'Not_Surivived'
df$Survived_status[df$Survived == 1] <-  'Survived'
df$Survived_status <-  as.factor(df$Survived_status)
titanic_train <-  df[1:357,]
titanic_test <-  df[358:714,]
#df$survived_status <- ifelse(df$Survived == 1, 'Survived' , 'Not_survived')
summary(titanic_test)
glm.fits <- glm(Survived_status ~ Age, data = titanic_train, family = binomial)  
summary(glm.fits)
# The result of the logistic regression suggests that there is a negative relation between age and the survived 
# status. This result may look reasonable because as shown in the movie titanic women and kids were rescued first. 
plot(Survived_status~Age, data = titanic_train)
abline(glm.fits)
glm.fits <- glm(Survived ~ Age+Sex, data = titanic_train, family = binomial) 
summary(glm.fits)
# Sex is definitly effecting the probability of surviving or not. Indeed, being a woman increased the probability to survive
glm.probs <-  predict(glm.fits, type = 'response')
glm.probs

glm.pred <-  rep('Not_Survived', 357)
glm.pred[glm.probs>.5] = 'Survived' 
table(glm.pred, titanic_test)

"""
Exercise 3
In this exercise, you will explore the bias/variance trade-off, that you have studied in Exercise 1 for a
classification context, within a regression context. In particular, we use the Wage data set from the ISLR
library. The dataset includes information about wage and other variables (e.g., age,. marital status, education)
for 3,000 male workers in the U.S. mid-atlantic region.
. Load the data set, attach it if necessary, and start looking at the variables, as usual.
. Explore the use of polynomial regression to predict wage using age. Consider using a 4th-degree
polynomial in the model.
. Use the model you created to predict wages for some values of age.
. Plot the data and the predictions, including confidence intervals.
. Explore different degrees for the polynomial: split the dataset into a train and test partition; fit the
models on the training set and calculate the (mean-squared) error on the training and test sets. Plot
the errors against the degrees, what do you observe?
. Optionally, you can perform the same analysis on the Auto data set, included in the ISLR library, to
estimate fuel consumption (mpg variable) from horsepower.
Hints
. poly(x, N) creates a polynomial of degree N over the set of points in x.
. Build confidence intervals extending +/- 2*SE around the value (SE: standard error).
. The computation of standard error has to be enabled in the predict() function.
"""
wage <-  ISLR::Wage
summary(wage)
names(wage)
dim(wage)
lm_poly <-  lm(wage~ poly(age, degree= 4), data = wage)
summary(lm_poly)
range(wage$age)
#new_age <-  list(as.integer(runif(63, min = 18, max = 80)))
new_age <-  data.frame('age' = seq(18,80, length.out = 63))
pred_wage <-  predict(lm_poly, new_age, se = T)
# Pred ora è un data frame, aggiungendo il se
# Non serve il type = 'response' perchè sto usando un modello lineare e non un 
# KNN  o altro 
summary(pred_wage$fit)
plot(wage$age, wage$wage)
lines(new_age$age, pred_wage$fit, col='red')
# Confidence interval 
se_bound <- cbind(pred_wage$fit + 2*pred_wage$se.fit, pred_wage$fit + 2*pred_wage$se.fit)  

matlines(new_age$age, se_bound, col = 'blue')

# vediamo se un polinomio di 4 grado funziona 




"""
Exercise 4
The output of regression models and classifiers depends on the choice of predictors that are included in the
model. There could well be other predictors (so called confounders) that are not in the model and that would
capture more clearly the relationship between predictors and response. In this exercise, we explore this aspect
using the Default dataset from the ISLR library. The dataset contains simulated data with information on
10,000 customers: the goal is to predict whether a customer will default on their credit card debt, given
the predictors student (Yes/No), balance (avg. balance remaining on the credit card), income (income of
the customer). Recall that student is therefore a dummy variable (also: indicator variable), i.e., a variable
taking only binary values (0/1, Yes/No) to represent the presence or absence of something.
. After the usual data exploration, fit a logistic regression model to predict default using student only.
Discuss the results.
. Fit another model to predict default using all predictors:
- Discuss the influence of the predictors on the outcome by examining the model coefficients; compare
with the single-predictor results.
- Use the fitted model to predict the probability of default of a student with a balance=$1, 500 and
an income of $40, 000 (textbook p137 (4.8))
- Compute the probability of default of a non-student with the same balance and income as above
(textbook p137 (4.9).
. To understand the phenomenon of confounding, compare the model with student only and the full
model. What can you notice about the coefficient for the variable student and how can you explain
this?
. In order to see it better, reproduce the left and right panel of Figure 4.3.
2
Hints
. Use the newdata=list(var1=value1, var2=value2, ...) argument in predict() for passing values
to predictors and getting predictions.
. For the left-hand plot of Fig. 4.3, consider the posterior probabilities of your models. Horizontal lines
represent the overall default rates (i.e., defaulted to non defaulted ratio) stratified by student status.
. The boxplot function accepts a formula argument.
"""
