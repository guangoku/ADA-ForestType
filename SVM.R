mytest.re<-read.csv("mytest.regular.csv")
head(mytest.re)
str(mytest.re)
levels(mytest.re$SoilType)<-c(levels(mytest.re$SoilType),"Soil_Type7","Soil_Type8","Soil_Type15","Soil_Type25")

mytrain.re<-mytrain.re[,-1]
mytest.re<-mytest.re[,-1];mytest.re<-mytest.re[,-1]
mytest.re$CoverType<-as.factor(mytest.re$CoverType)
levels(mytrain.re$SoilType)<-c("Soil_Type1","Soil_Type2","Soil_Type3","Soil_Type4","Soil_Type5","Soil_Type6","Soil_Type7","Soil_Type8","Soil_Type9","Soil_Type10",
                             "Soil_Type11","Soil_Type12","Soil_Type13","Soil_Type14","Soil_Type15","Soil_Type16","Soil_Type17","Soil_Type18","Soil_Type19","Soil_Type20",
                             "Soil_Type21","Soil_Type22","Soil_Type23","Soil_Type24","Soil_Type25","Soil_Type26","Soil_Type27","Soil_Type28","Soil_Type29","Soil_Type30",
                             "Soil_Type31","Soil_Type32","Soil_Type33","Soil_Type34","Soil_Type35","Soil_Type36","Soil_Type37","Soil_Type38","Soil_Type39","Soil_Type40")

library(e1071)
#default mytrain
svm.0  <- svm(CoverType~., data = mytrain.re)
summary(svm.0)
svm.pred.0<-predict(svm.0,mytest.re[,-13])
tab0<-table(pred = svm.pred.0, true =mytest.re[,13])
classAgreement(tab0)
#$diag[1] 0.6439594
#$kappa[1] 0.5845764
#$rand[1] 0.8503297
#$crand[1] 0.4095087
tuned <- tune.svm(CoverType~., data = mytrain.re, gamma = 10^(-6:-1), cost = 10^(1:2))
summary(tuned)
#Overfit best one
svm.best <- svm(CoverType~ ., data = mytrain.re, cost = 100, gamma = 0.1)
summary(svm.best)
svm.pred.best<-predict(svm.best, mytest.re[,-13])
tab.best<-table(pred = svm.pred.best, true =mytest.re[,13])
classAgreement(tab.best)
#$diag#[1] 0.6296296
#$kappa#[1] 0.5679225
#$rand#[1] 0.8400523
#$crand#[1] 0.3691007

tuned2<-tune.svm(CoverType~., data = mytrain.re, gamma = 10^(-4:4), cost = 10^(-4:3))
summary(tuned2)
#Parameter tuning of ‘svm’:
#  
#  - sampling method: 10-fold cross validation #
#
#- best parameters:
#  gamma cost
#0.1  100
#
#- best performance: 0.1719656 
#
#- Detailed performance results:
#  gamma  cost     error  dispersion
#1  1e-04 1e-04 0.8680051 0.007819869
#2  1e-03 1e-04 0.8680051 0.007819869
#3  1e-02 1e-04 0.8680051 0.007819869
#4  1e-01 1e-04 0.8680051 0.007819869
#5  1e+00 1e-04 0.8680051 0.007819869
#6  1e+01 1e-04 0.8680051 0.007819869
#7  1e+02 1e-04 0.8680051 0.007819869
#8  1e+03 1e-04 0.8680051 0.007819869
#9  1e+04 1e-04 0.8680051 0.007819869
#10 1e-04 1e-03 0.8680051 0.007819869
#11 1e-03 1e-03 0.8680051 0.007819869
#12 1e-02 1e-03 0.8680051 0.007819869
#13 1e-01 1e-03 0.8680051 0.007819869
#14 1e+00 1e-03 0.8680051 0.007819869
#15 1e+01 1e-03 0.8680051 0.007819869
#16 1e+02 1e-03 0.8680051 0.007819869
#17 1e+03 1e-03 0.8680051 0.007819869
#18 1e+04 1e-03 0.8680051 0.007819869
#19 1e-04 1e-02 0.8680051 0.007819869
#20 1e-03 1e-02 0.8680051 0.007819869
#21 1e-02 1e-02 0.5947602 0.019636189
#22 1e-01 1e-02 0.4436009 0.019036655##
#23 1e+00 1e-02 0.8680051 0.007819869
#24 1e+01 1e-02 0.8680051 0.007819869
#25 1e+02 1e-02 0.8680051 0.007819869
#26 1e+03 1e-02 0.8680051 0.007819869
#27 1e+04 1e-02 0.8680051 0.007819869
#28 1e-04 1e-01 0.8680051 0.007819869
#29 1e-03 1e-01 0.5694668 0.016043979
#30 1e-02 1e-01 0.3521385 0.013518531
#31 1e-01 1e-01 0.2921668 0.012358110##
#32 1e+00 1e-01 0.5012612 0.016736485
#33 1e+01 1e-01 0.8680051 0.007819869
#34 1e+02 1e-01 0.8680051 0.007819869
#35 1e+03 1e-01 0.8680051 0.007819869
#36 1e+04 1e-01 0.8680051 0.007819869
#37 1e-04 1e+00 0.5688962 0.015121875
#38 1e-03 1e+00 0.3513817 0.010842937
#39 1e-02 1e+00 0.2939638 0.014014201
#40 1e-01 1e+00 0.2194906 0.012963788###
#41 1e+00 1e+00 0.1895460 0.012236891###
#42 1e+01 1e+00 0.4900135 0.013973741
#43 1e+02 1e+00 0.8201085 0.022123635
#44 1e+03 1e+00 0.8673431 0.007968790
#45 1e+04 1e+00 0.8680051 0.007819869
#46 1e-04 1e+01 0.3515751 0.011537315
#47 1e-03 1e+01 0.3043555 0.016876805
#48 1e-02 1e+01 0.2517223 0.014449176
#49 1e-01 1e+01 0.1820735 0.008220208###
#50 1e+00 1e+01 0.1890788 0.014269528###
#51 1e+01 1e+01 0.4605371 0.015009353
#52 1e+02 1e+01 0.8093308 0.021323244
#53 1e+03 1e+01 0.8669661 0.008132275
#54 1e+04 1e+01 0.8680051 0.007819869
#55 1e-04 1e+02 0.3084066 0.015983647
#56 1e-03 1e+02 0.2790312 0.013769687
#57 1e-02 1e+02 0.2193961 0.014332915
#58 1e-01 1e+02 0.1719656 0.010048858#
#59 1e+00 1e+02 0.1916241 0.013387954
#60 1e+01 1e+02 0.4605371 0.015009353
#61 1e+02 1e+02 0.8093308 0.021323244
#62 1e+03 1e+02 0.8669661 0.008132275
#63 1e+04 1e+02 0.8680051 0.007819869
#64 1e-04 1e+03 0.2882759 0.012424617
#65 1e-03 1e+03 0.2479510 0.013199737
#66 1e-02 1e+03 0.1991667 0.010001239
#67 1e-01 1e+03 0.1889836 0.013393771
#68 1e+00 1e+03 0.1920024 0.012926892
#69 1e+01 1e+03 0.4605371 0.015009353
#70 1e+02 1e+03 0.8093308 0.021323244
#71 1e+03 1e+03 0.8669661 0.008132275
#72 1e+04 1e+03 0.8680051 0.007819869

#models
#22 1e-01 1e-02 0.4436009 0.019036655##not good obviously with 0.5436508
svm.1 <- svm(CoverType~ ., data = mytrain.re, gamma = 1e-01, cost =1e-02)
summary(svm.1)
svm.pred.1<-predict(svm.1, mytest.re[,-13])
tab.1<-table(pred = svm.pred.1, true =mytest.re[,13]);tab.1;classAgreement(tab.1)
#31 1e-01 1e-01 0.2921668 0.012358110##fine i guess, with 0.6505732###################
svm.2 <- svm(CoverType~ ., data = mytrain.re, gamma = 1e-01, cost =1e-01)
summary(svm.2)
svm.pred.2<-predict(svm.2, mytest.re[,-13])
tab.2<-table(pred = svm.pred.2, true =mytest.re[,13]);tab.2;classAgreement(tab.2)
#40 1e-01 1e+00 0.2194906 0.012963788###slightly better with 0.6609347 ###################
svm.3 <- svm(CoverType~ ., data = mytrain.re, gamma = 1e-01, cost =1e+00)
summary(svm.3)
svm.pred.3<-predict(svm.3, mytest.re[,-13])
tab.3<-table(pred = svm.pred.3, true =mytest.re[,13]);tab.3;classAgreement(tab.3)
#41 1e+00 1e+00 0.1895460 0.012236891### worst 0.4102734
svm.4 <- svm(CoverType~ ., data = mytrain.re, gamma = 1e+00, cost =1e+00)
summary(svm.4)
svm.pred.4<-predict(svm.4, mytest.re[,-13])
tab.4<-table(pred = svm.pred.4, true =mytest.re[,13]);tab.4;classAgreement(tab.4)
#49 1e-01 1e+01 0.1820735 0.008220208###so so with 0.6417549
svm.5 <- svm(CoverType~ ., data = mytrain.re, gamma = 1e-01, cost =1e+01)
summary(svm.5)
svm.pred.5<-predict(svm.5, mytest.re[,-13])
tab.5<-table(pred = svm.pred.5, true =mytest.re[,13]);tab.5;classAgreement(tab.5)
#50 1e+00 1e+01 0.1890788 0.014269528### worst 0.4190917
svm.6 <- svm(CoverType~ ., data = mytrain.re, gamma = 1e+00, cost =1e+01)
summary(svm.6)
svm.pred.6<-predict(svm.6, mytest.re[,-13])
tab.6<-table(pred = svm.pred.6, true =mytest.re[,13]);tab.6;classAgreement(tab.6)
############################################################
#svm(x, y = NULL, scale = TRUE, type = NULL, kernel = "radial", degree = 3, 
#    gamma = if (is.vector(x)) 1 else 1 / ncol(x),    coef0 = 0, cost = 1, nu = 0.5,
#    class.weights = NULL, cachesize = 40, tolerance = 0.001, epsilon = 0.1,
#    shrinking = TRUE, cross = 0, probability = FALSE, fitted = TRUE,
#    ..., subset, na.action = na.omit)

##Use of other Kernels:linear
#linear, cost=10, not good 0.5189594
svm.l<-svm(CoverType~ ., data = mytrain.re,kernel="linear", cost =1e+01)
summary(svm.l)
svm.pred.l<-predict(svm.l, mytest.re[,-13])
tab.l<-table(pred = svm.pred.l, true =mytest.re[,13]);tab.l;classAgreement(tab.l)
#linear, cost=1, not good 0.5244709
svm.l.1<-svm(CoverType~ ., data = mytrain.re,kernel="linear", cost =1)
summary(svm.l.1)
svm.pred.l.1<-predict(svm.l.1, mytest.re[,-13])
tab.l.1<-table(pred = svm.pred.l.1, true =mytest.re[,13]);tab.l.1;classAgreement(tab.l.1)
#linear, cost=0.1, not good 0.5551146
svm.l.2<-svm(CoverType~ ., data = mytrain.re,kernel="linear", cost =0.1)
summary(svm.l.2)
svm.pred.l.2<-predict(svm.l.2, mytest.re[,-13])
tab.l.2<-table(pred = svm.pred.l.2, true =mytest.re[,13]);tab.l.2;classAgreement(tab.l.2)

###Use of other Kernels:sigmoid 
#sigmoid, default coef=0,gamma=0.01886792 ,c=0.1,not bad 0.5846561
svm.s1<-svm(CoverType~ ., data = mytrain.re,kernel="sigmoid", cost =0.1)
summary(svm.s1)
svm.pred.s1<-predict(svm.s1, mytest.re[,-13])
tab.s1<-table(pred = svm.pred.s1, true =mytest.re[,13]);tab.s1;classAgreement(tab.s1)
#sigmoid, default ,c=1, similar, 0.574515
svm.s2<-svm(CoverType~ ., data = mytrain.re,kernel="sigmoid", cost =1)
summary(svm.s2)
svm.pred.s2<-predict(svm.s2, mytest.re[,-13])
tab.s2<-table(pred = svm.pred.s2, true =mytest.re[,13]);tab.s2;classAgreement(tab.s2)
#sigmoid, default ,c=10, similar, worse 0.4823633
svm.s3<-svm(CoverType~ ., data = mytrain.re,kernel="sigmoid", cost =10)
summary(svm.s3)
svm.pred.s3<-predict(svm.s3, mytest.re[,-13])
tab.s3<-table(pred = svm.pred.s3, true =mytest.re[,13]);tab.s3;classAgreement(tab.s3)

##Use of other Kernels:polynomial
#polynomial, default, coef=0,gamma=0.01886792 ,degree=3,c=0.1, worst ever!!!! 0.22
svm.p1<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =0.1)
summary(svm.p1)
svm.pred.p1<-predict(svm.p1, mytest.re[,-13])
tab.p1<-table(pred = svm.pred.p1, true =mytest.re[,13]);tab.p1;classAgreement(tab.p1)
#pllynomial, default c=1, greatly improved, 0.456
svm.p2<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =1)
summary(svm.p2)
svm.pred.p2<-predict(svm.p2, mytest.re[,-13])
tab.p2<-table(pred = svm.pred.p2, true =mytest.re[,13]);tab.p2;classAgreement(tab.p2)
#polynomial, default,c=10, keep improving, 0.6148589
svm.p3<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =10)
summary(svm.p3)
svm.pred.p3<-predict(svm.p3, mytest.re[,-13])
tab.p3<-table(pred = svm.pred.p3, true =mytest.re[,13]);tab.p3;classAgreement(tab.p3)
#tuning for polynomial a subset
#subset for tuning
train.tune<-mytrain.re[sample(1:nrow(mytrain.re), nrow(mytrain.re)*0.4,replace=FALSE),]
tuned.p<-tune.svm(CoverType~., kernel="polynomial",data =train.tune, 
                  coef=0:1, gamma = 10^(-4:4), cost =10^(-1:1) )

summary(tuned.p)
#Parameter tuning of ‘svm’:  
#  - sampling method: 10-fold cross validation 
#- best parameters:
#  gamma coef0 cost
#0.1     1   10
#- best performance: 0.2260575 
#- Detailed performance results:
#  gamma coef0 cost     error dispersion
#1  1e-04     0  0.1 0.8528220 0.02466818
#2  1e-03     0  0.1 0.8528220 0.02466818
#3  1e-02     0  0.1 0.8528220 0.02466818
#4  1e-01     0  0.1 0.3793867 0.01441005
#5  1e+00     0  0.1 0.2394942 0.02409682
#6  1e+01     0  0.1 0.2773010 0.02330424
#7  1e+02     0  0.1 0.2796645 0.02344754
#8  1e+03     0  0.1 0.2791950 0.02291173
#9  1e+04     0  0.1 0.2791906 0.02376241
#10 1e-04     1  0.1 0.8454680 0.03687147
#11 1e-03     1  0.1 0.6930373 0.04012779
#12 1e-02     1  0.1 0.3770019 0.02272401
#13 1e-01     1  0.1 0.2769095 0.02252604
#14 1e+00     1  0.1 0.2378276 0.02714002
#15 1e+01     1  0.1 0.2768315 0.02345944
#16 1e+02     1  0.1 0.2791928 0.02342000
#17 1e+03     1  0.1 0.2791950 0.02315323
#18 1e+04     1  0.1 0.2791928 0.02328767
#19 1e-04     0  1.0 0.8528220 0.02466818
#20 1e-03     0  1.0 0.8528220 0.02466818
#21 1e-02     0  1.0 0.8351114 0.03885258
#22 1e-01     0  1.0 0.2674154 0.01972159
#23 1e+00     0  1.0 0.2581617 0.02046230
#24 1e+01     0  1.0 0.2794309 0.02304812
#25 1e+02     0  1.0 0.2796645 0.02344754
#26 1e+03     0  1.0 0.2791950 0.02291173
#27 1e+04     0  1.0 0.2791906 0.02376241
#28 1e-04     1  1.0 0.6949241 0.03935132
#29 1e-03     1  1.0 0.3862219 0.01926147
#30 1e-02     1  1.0 0.3094860 0.01622926
#31 1e-01     1  1.0 0.2364843 0.01332496
#32 1e+00     1  1.0 0.2595723 0.02330944
#33 1e+01     1  1.0 0.2784830 0.02397275
#34 1e+02     1  1.0 0.2791928 0.02342000
#35 1e+03     1  1.0 0.2791950 0.02315323
#36 1e+04     1  1.0 0.2791928 0.02328767
#37 1e-04     0 10.0 0.8528220 0.02466818
#38 1e-03     0 10.0 0.8528220 0.02466818
#39 1e-02     0 10.0 0.5874905 0.01976423
#40 1e-01     0 10.0 0.2274703 0.01964143
#41 1e+00     0 10.0 0.2728080 0.02449001
#42 1e+01     0 10.0 0.2794309 0.02304812
#43 1e+02     0 10.0 0.2796645 0.02344754
#44 1e+03     0 10.0 0.2791950 0.02291173
#45 1e+04     0 10.0 0.2791906 0.02376241
#46 1e-04     1 10.0 0.3878790 0.01850038
#47 1e-03     1 10.0 0.3165531 0.01307760
#48 1e-02     1 10.0 0.2743079 0.02076314
#49 1e-01     1 10.0 0.2260575 0.01688420
#50 1e+00     1 10.0 0.2688103 0.02251754
#51 1e+01     1 10.0 0.2784830 0.02397275
#52 1e+02     1 10.0 0.2791928 0.02342000
#53 1e+03     1 10.0 0.2791950 0.02315323
#54 1e+04     1 10.0 0.2791928 0.02328767

#polynomial,best turned, coef=1,gamma=0.1,degree=3,c=10, 0.5954586
svm.p.b1<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =10, gamma=0.1,coef0=1,degree=3)
summary(svm.p.b1)
svm.pred.p.b1<-predict(svm.p.b1, mytest.re[,-13])
tab.p.b1<-table(pred = svm.pred.p.b1, true =mytest.re[,13]);tab.p.b1;classAgreement(tab.p.b1)
#degree=2, 0.5983245
svm.p.b2<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =10, gamma=0.1,coef0=1,degree=2)
summary(svm.p.b2)
svm.pred.p.b2<-predict(svm.p.b2, mytest.re[,-13])
tab.p.b2<-table(pred = svm.pred.p.b2, true =mytest.re[,13]);tab.p.b2;classAgreement(tab.p.b2)
#degree=4, 0.602072
svm.p.b3<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =10, gamma=0.1,coef0=1,degree=4)
summary(svm.p.b3)
svm.pred.p.b3<-predict(svm.p.b3, mytest.re[,-13])
tab.p.b3<-table(pred = svm.pred.p.b3, true =mytest.re[,13]);tab.p.b3;classAgreement(tab.p.b3)

#degree=5, 0.5989859
svm.p.b4<-svm(CoverType~ ., data = mytrain.re,kernel="polynomial", cost =10, gamma=0.1,coef0=1,degree=5)
summary(svm.p.b4)
svm.pred.p.b4<-predict(svm.p.b4, mytest.re[,-13])
tab.p.b4<-table(pred = svm.pred.p.b4, true =mytest.re[,13]);tab.p.b4;classAgreement(tab.p.b4)
