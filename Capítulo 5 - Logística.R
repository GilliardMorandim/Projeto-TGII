# Ajuste modelo log?stico Diego

library('devtools')
install_github('diegomattozo/categorization',force=TRUE)
library("multdiscretization", lib.loc="~/R/win-library/3.4")
set.seed(12341235)
bd_cartao_log<-cartao_orig_desenv[,-1]
nrow(bd_cartao_log)
amost <- sample(1:nrow(bd_cartao_log), 0.8*nrow(bd_cartao_log))
cheque_test<-bd_cartao_log[-amost,]
cheque_desenv<-bd_cartao_log[amost,]
disc_cartao_desenv<-discretize(db=cheque_desenv,
                               meth=5,
                               alpha=0.05,
                               n=100)
cuts_desenv<-disc_cartao_desenv$cuts
disc_cartao_desenv<-disc_cartao_desenv$data
names(disc_cartao_desenv)
quantile_cuts<-quantile_discretization(db=cheque_desenv,
                                        n=100)$cuts
quantile_cartao_test<-disc_from_cuts(db=cheque_test,
                                     cutpoints = quantile_cuts)
disc_cartao_test<-disc_from_cuts(db=quantile_cartao_test,
                                 cutpoints = cuts_desenv)
names(disc_cartao_desenv)
names(disc_cartao_test)
fit <- glm(resp~., family = binomial,
           data = disc_cartao_desenv)
predValid <- predict(fit, disc_cartao_test, type = "response")
disc_cartao_test$RESP_CA<-(disc_cartao_test$RESP_CA=="mau")*1

ks_stat(disc_cartao_test$RESP_CA, predValid)
t<-ks_stat(disc_cartao_test$RESP_CA, predValid,returnKSTable = T)
View(t)
plotROC(disc_cartao_test$RESP_CA, predValid)
giniCoef<-logistic_reg_giniCoef(train=disc_cartao_desenv,
                                test=disc_cartao_test,
                                respName = 'resp')
giniCoef
table(bd_cartao_log$X206,bd_cartao_log$RESP_CA)
prop.table(table(bd_cartao_log$X206,bd_cartao_log$RESP_CA),1)
system("shutdown -s -t 2000")
