#Pacotes 
install.packages("rpart.plot")
install.packages("C50")
install.packages('caret')
install.packages('e1071')

require(rpart.plot)
require(rpart)
library(C50)
library('caret')
library('e1071')
library('gbm')
system('shutdown -a')
system('shutdown -s -t 1800')
str(iris)
set.seed(1955)
g<-runif(nrow(iris))
irisr <- iris[order(g),]
str(irisr)
#formar a arvore usando 100 observações
mC5.0<-C5.0(irisr[1:100,-5],irisr[1:100,5])
mC5.0
#backtest da arvore
summary(mC5.0)
#teste na base de validação
p1 <- predict(mC5.0,irisr[101:150,])
p1
#Matriz de confusão
tbl_c5.0<-table(Observados=irisr[101:150,5],"Preditos para o C5.0"=p1)
#Achar a melhor combinação de hiperparametros via caret
mGBM <- train(Species~.,data=iris,method="gbm")
mGBM$bestTune
#Modelar nosso gbm com os resultados do acima
GBM_tree<-gbm(Species~.,data=irisr[1:100,],distribution="multinomial",n.trees=50,interaction.depth=2,shrinkage=.1)
pred_GBM_tree<-predict(GBM_tree,n.trees=50,newdata=irisr[101:150,],type="response")
p.pred_GBM_tree<-apply(pred_GBM_tree,1,which.max)
p.pred_GBM_tree<-as.matrix(p.pred_GBM_tree)
pred_gbm<-apply(p.pred_GBM_tree,1,function(x) switch(x,"setosa","versicolor","virginica"))
tbl_gbm<-table(Observados=irisr[101:150,5],"Preditos para o GBM"=pred_gbm)
summary(GBM_tree)
tbl_gbm
tbl_c5.0
#Plot da arvore
plot(mC5.0)