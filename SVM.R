mytrain.re<-mytrain.re[,-1]
mytest.re<-mytest.re[,-1]
library(e1071)
#default mytrain
#sth is wrong, try mytrain.re
svm.0  <- svm(CoverType~., data = mytrain.re)
summary(svm.0)
svm.pred.0<-predict(svm.0,mytest.re[,-13])
tab0<-table(pred = svm.pred.0, true =mytest.re[,13])
classAgreement(tab0)

tuned <- tune.svm(CoverType~., data = mytrain.re, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
#Parameter tuning of ‘svm’:  
#  - sampling method: 10-fold cross validation 
#- best parameters:
#  gamma cost
#0.1  100
#- best performance: 0.1730741 
#- Detailed performance results:
#  gamma cost     error  dispersion
#1  1e-06   10 0.8706652 0.006307872
#2  1e-05   10 0.5594865 0.017011850
#3  1e-04   10 0.3512591 0.012855334
#4  1e-03   10 0.3032663 0.011764651
#5  1e-02   10 0.2533013 0.014278346
#6  1e-01   10 0.1830015 0.009285892
#7  1e-06  100 0.5596751 0.017211253
#8  1e-05  100 0.3513544 0.012509550
#9  1e-04  100 0.3075207 0.010542930
#10 1e-03  100 0.2785348 0.008877590
#11 1e-02  100 0.2226824 0.007494504
#12 1e-01  100 0.1730741 0.010226023

svm.best <- svm(CoverType~ ., data = mytrain.re, cost = 100, gamma = 0.1)
summary(svm.best)
svm.pred.best<-predict(svm.best,mytest.re[,-13])
tab.best<-table(pred = svm.pred.best, true =mytest.re[,13])
classAgreement(tab0)