
  
# siamo in un problema di classificazione dobbiamo riuscire ad creare un 
# modello che ci permetta di capire se una persona dati dei valori 
# rischi il default o meno.
library(ISLR)
 
def <-  ISLR::Default
size_tt <- floor(0.6 * nrow(def))
train_ind <- sample(seq_len(nrow(def)), size = size_tt)
train <- def[train_ind,]
test <-  def[-train_ind,]
#train_perc <-  0.5 
#ttrain_in <- sample(nrow(def), train_perc*nrow(def))
set.seed(99)
glm.fit <-  glm(default ~ income + balance ,data = train, family = 'binomial')
glm.prob <-  predict(glm.fit, test, type = 'response')
head(glm.prob)
print(max(glm.prob))
glm.pred <-  rep('No', nrow(test))
glm.pred[glm.prob > 0.5] <-  'Yes'
#View(glm.prob)
max(glm.prob)
table(glm.pred,test$default)
glm.pred
test$default
mean( glm.pred != test$default)
train.perc <-  0.5
for(i in 1:10){
  train_ <-  sample(nrow(def), train.perc*nrow(def))
  dataf.v <- def[-train_,]
  fit <-  glm(default~ income + balance ,data = train, family = 'binomial')
  prob <-  predict(fit, dataf.v, type = 'response')
  pred <-  ifelse(prob>.5, 'Yes', 'No')
  table(pred, dataf.v$default)
  err.M1[i] <-  mean(dataf.v$default != pred)
}
