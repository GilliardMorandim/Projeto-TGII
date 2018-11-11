library(readr)
library(InformationValue)
library("dplyr")
library(gbm)
library(broman)
library('devtools')
install_github('diegomattozo/categorization',force=TRUE)
library("multdiscretization", lib.loc="~/R/win-library/3.4")
x<-1
cat(x,"wine
  ")
des<-function(t){
  system(as.character(cat("shutdown -s -t",t)))
}
des(2000)
cartao_orig_desenv <- read_csv("C:/Users/euamo carnaval/Desktop/Bases TG I/cartao_orig_desenv.csv")
bd_cartao<-cartao_orig_desenv
bd_cartao$RESP_CA<-1*(bd_cartao$RESP_CA=="mau")
set.seed(8457)

amost <- sample(1:nrow(bd_cartao), 0.6*nrow(bd_cartao))
table(as.factor(bd_cartao$RESP_CA))
names(bd_cartao)
train <- bd_cartao[amost,-1]
test <- bd_cartao[-amost,-1]
amost <- sample(1:nrow(test), 0.5*nrow(test))
length(amost)
vali<-test[amost,]
test<-test[-amost,]
prop.table(table(train$RESP_CA))
prop.table(table(vali$RESP_CA))
prop.table(table(test$RESP_CA))
table(train$RESP_CA)
table(vali$RESP_CA)
table(test$RESP_CA)
names(train)
nobsM<-round(.05*234)
nt<-50
gbmMod2 <- gbm::gbm(RESP_CA~., 
                    data=train, 
                    n.trees=nt, 
                    distribution="bernoulli",
                    interaction.depth = 8,
                    n.minobsinnode = 2,
                    shrinkage = 0.1)
summary(gbmMod2)


preds <- predict(gbmMod2, newdata = train, n.trees=nt)
pi<-1/(1+exp(-preds))
prev<-(pi>=0.5)*1
table(prev,train$RESP_CA)
ks_stat(train$RESP_CA, pi)
# t<-ks_stat(train$RESP_CA, prev, returnKSTable = T)
# t

preds <- predict(gbmMod2, newdata = test, n.trees=nt)
pi<-1/(1+exp(-preds))
prev<-(pi>=0.5)*1
table(prev,test$RESP_CA)
ks_stat(test$RESP_CA,pi)
# t<-ks_stat(test$RESP_CA, prev, returnKSTable = T)
# t

preds <- predict(gbmMod2, newdata = vali, n.trees=nt)
pi<-1/(1+exp(-preds))
table(prev,vali$RESP_CA)
ks_stat(vali$RESP_CA,pi)


hyper_grid <- expand.grid(
  shrinkage = c(.05, .06, .08, .09, .1),
  interaction.depth = c(4, 5, 6, 7),
  n.minobsinnode = c(5, 10, 15, 20),
  bag.fraction = c(.65, .75, 1),
  ntre = c(50,70,80,100)
)
head(hyper_grid,30)
#360 modelos
nrow(hyper_grid)

for(i in 1:nrow(hyper_grid)) {
  set.seed(1234)
  gbm.tune <- gbm(formula = RESP_CA ~ .,
                  distribution = "bernoulli",
                  data = train,
                  n.trees = hyper_grid$ntre[i],
                  interaction.depth = hyper_grid$interaction.depth[i],
                  shrinkage = hyper_grid$shrinkage[i],
                  n.minobsinnode = hyper_grid$n.minobsinnode[i],
                  bag.fraction = hyper_grid$bag.fraction[i],
                  train.fraction = .75,
                  # n.cores = NULL, # will use all cores by default
                  verbose = FALSE
              )
  preds <- predict(gbm.tune, newdata = train, n.trees=hyper_grid$ntre[i])
  pi<-1/(1+exp(-preds))
  hyper_grid$KS_des[i]<-ks_stat(train$RESP_CA, pi)
  
  preds <- predict(gbm.tune, newdata = test, n.trees=hyper_grid$ntre[i])
  pi<-1/(1+exp(-preds))
  hyper_grid$KS_vali[i]<-ks_stat(test$RESP_CA, pi)
  
  hyper_grid$optimal_trees[i] <- which.min(gbm.tune$valid.error)
  
  hyper_grid$min_RMSE[i] <- sqrt(min(gbm.tune$valid.error))
  
  hyper_grid$DIFF_DES_VALI <- hyper_grid$KS_des[i] - hyper_grid$KS_vali[i]
  
  hyper_grid$id_modelo[i] <- paste("Modelo_intera_",i) 
  
  print(i)
}

write.csv(hyper_grid, file = "result_model.csv")

hyper_grid[i,]
hyper_grid %>% 
  dplyr::arrange(KS_vali) %>%
  head(30)


i=226
set.seed(1234)
View(hyper_grid[i,])
train$RESP_CA
gbm.tune <- gbm(formula = RESP_CA ~ .,
                distribution = "bernoulli",
                data = train,
                n.trees = hyper_grid$ntre[i],
                interaction.depth = hyper_grid$interaction.depth[i],
                shrinkage = hyper_grid$shrinkage[i],
                n.minobsinnode = hyper_grid$n.minobsinnode[i],
                bag.fraction = hyper_grid$bag.fraction[i],
                train.fraction = .75,
                n.cores = NULL, # will use all cores by default
                verbose = FALSE
)


preds <- predict(gbm.tune, newdata = train, n.trees=hyper_grid$ntre[i])
score_train<-round(1000*((preds+6)/12))
score_train<- (score_train>=1000)*1000+
              (score_train<=0)*0+
              (score_train<1000&score_train>0)*score_train
ks_stat(train$RESP_CA, score_train)
t<-ks_stat(train$RESP_CA, score_train, returnKSTable = T)
pi<-1/(1+exp(-preds))
plotROC(train$RESP_CA, pi)
View(t)

preds <- predict(gbm.tune, newdata = test, n.trees=hyper_grid$ntre[i])
score_test<-round(1000*((preds+6)/12))
score_test<- (score_test>=1000)*1000+
  (score_test<=0)*0+
  (score_test<1000&score_test>0)*score_test
ks_stat(test$RESP_CA, score_test)
t<-ks_stat(test$RESP_CA, score_test, returnKSTable = T)
pi<-1/(1+exp(-preds))
plotROC(test$RESP_CA, pi)
View(t)


preds <- predict(gbm.tune, newdata = vali, n.trees=hyper_grid$ntre[i])
score_vali<-round(1000*((preds+6)/12))
score_vali<- (score_vali>=1000)*1000+
  (score_vali<=0)*0+
  (score_vali<1000&score_vali>0)*score_vali
ks_stat(vali$RESP_CA, score_vali)
t<-ks_stat(vali$RESP_CA, score_vali, returnKSTable = T)
pi<-1/(1+exp(-preds))
plotROC(vali$RESP_CA, pi)
View(t)

t<-summary(
  gbm.tune, 
  cBars = 6,
  method = relative.influence, # also can use permutation.test.gbm
  las = 2
  
)
dd<-t$var
View(t$rel.inf)
View(t$var)
barplot(t$rel.inf, names.arg=dd,horiz=T)
# plot(gbm.tune$train.error)
# plot(gbm.tune$valid.error)
log 
??(x)
1 ??? ??(x)

= ??1 + ??2x2 + ? ? ? + ??pxp,

# Diego
disc_cartao_desenv<-discretize(db=train,
                              meth=5,
                              alpha=0.05,
                              n=100)
cuts_desenv<-disc_cartao_desenv$cuts
disc_cartao_desenv<-disc_cartao_desenv$data
names(disc_cartao_desenv)
quantile_cuts<-quantile_discretization(db=train,
                                       n=100)$cuts
quantile_cartao_test<-disc_from_cuts(db=vali,
                                     cutpoints = quantile_cuts)
disc_cartao_test<-disc_from_cuts(db=quantile_cartao_test,
                                 cutpoints = cuts_desenv)
names(disc_cartao_desenv)
names(disc_cartao_test)
fit <- glm(resp~., family = binomial,
           data = disc_cartao_desenv)
predValid <- predict(fit, disc_cartao_test, type = "response")

ks_stat(disc_cartao_test$RESP_CA, predValid)
t<-ks_stat(disc_cartao_test$RESP_CA, predValid,returnKSTable = T)
View(t)
plotROC(disc_cartao_test$RESP_CA, predValid)
giniCoef<-logistic_reg_giniCoef(train=disc_cartao_desenv,
                                test=disc_cartao_test,
                                respName = 'resp')
giniCoef
