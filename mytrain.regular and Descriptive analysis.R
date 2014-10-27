#Get Regular Dataset
head(mytrain)
mytrain.sort <- mytrain[order(row.names(mytrain)),]
#deal with Wilderness_Areas
temp.wa<-cbind(row.names(mytrain),mytrain[,12:15])
names(temp.wa)<-c("rn",names(temp.wa)[-1])
WArea<-melt(temp.wa, id=c("rn"))
WArea<-WArea[which(WArea$value=='1'),1:2]
WArea<-WArea[order(WArea$rn),]
#deal with Soil_Types
temp.st<-cbind(row.names(mytrain),mytrain[,16:55])
names(temp.st)<-c("rn",names(temp.st)[-1])
ST<-melt(temp.st, id=c("rn"))
ST<-ST[which(ST$value=='1'),1:2]
ST<-ST[order(ST$rn),]
#new data
mytrain.re<-cbind(mytrain.sort[,1:11],WArea[,2],ST[,2],mytrain.sort[,56])
names(mytrain.re)<-c("id","elevation","aspect","slope","hd.h","vd.h","hd.r",
                     "hs.9","hs.n","hs.3","hd.f","WArea","SoilType","CoverType")
mytrain.re$CoverType<-as.factor(mytrain.re$CoverType)
write.csv(mytrain.re,"mytrain.regular.csv")


#Get Regular Dataset for mytest
head(mytest)
mytest.sort <- mytest[order(row.names(mytest)),]
#deal with Wilderness_Areas
temp.wa.t<-cbind(row.names(mytest),mytest[,12:15])
names(temp.wa.t)<-c("rn",names(temp.wa.t)[-1])
WArea.t<-melt(temp.wa.t, id=c("rn"))
WArea.t<-WArea.t[which(WArea.t$value=='1'),1:2]
WArea.t<-WArea.t[order(WArea.t$rn),]
#deal with Soil_Types
temp.st.t<-cbind(row.names(mytest),mytest[,16:55])
names(temp.st.t)<-c("rn",names(temp.st.t)[-1])
ST.t<-melt(temp.st.t, id=c("rn"))
ST.t<-ST.t[which(ST.t$value=='1'),1:2]
ST.t<-ST.t[order(ST.t$rn),]
#new data
mytest.re<-cbind(mytest.sort[,1:11],WArea.t[,2],ST.t[,2],mytest.sort[,56])
names(mytest.re)<-c("id","elevation","aspect","slope","hd.h","vd.h","hd.r",
                     "hs.9","hs.n","hs.3","hd.f","WArea","SoilType","CoverType")
mytest.re$CoverType<-as.factor(mytest.re$CoverType)
write.csv(mytest.re,"mytest.regular.csv")
#Descriptive Data Analysis
attach(mytrain.re)
summary(mytrain.re$CoverType)
#1    2    3    4    5    6    7 
#1519 1507 1517 1520 1498 1501 1522
corr<-cor(mytrain.re[,2:11])
            #elevation       aspect       slope        hd.h        vd.h         hd.r         hs.9         hs.n        hs.3        hd.f
#elevation  1.000000000 -0.007295716 -0.30996046  0.42030716  0.12417658  0.576555161  0.098857311  0.220361226  0.09054617  0.43948587
#aspect    -0.007295716  1.000000000  0.02927343  0.04917738  0.06386967  0.067054082 -0.593798248  0.319918531  0.63566622 -0.05456021
#slope     -0.309960457  0.029273431  1.00000000 -0.05788387  0.26168274 -0.275927796 -0.206923453 -0.615845363 -0.32110685 -0.23566081
#hd.h       0.420307156  0.049177381 -0.05788387  1.00000000  0.65401463  0.212445209 -0.037386139  0.083905952  0.08628569  0.15599595
#vd.h       0.124176575  0.063869670  0.26168274  0.65401463  1.00000000  0.011010172 -0.101974169 -0.133615365 -0.02967289 -0.01643619
#hd.r       0.576555161  0.067054082 -0.27592780  0.21244521  0.01101017  1.000000000 -0.004225309  0.239702828  0.17443605  0.47914472
#hs.9       0.098857311 -0.593798248 -0.20692345 -0.03738614 -0.10197417 -0.004225309  1.000000000 -0.004651506 -0.78012110  0.07836192
#hs.n       0.220361226  0.319918531 -0.61584536  0.08390595 -0.13361537  0.239702828 -0.004651506  1.000000000  0.60719784  0.11776473
#hs.3       0.090546172  0.635666216 -0.32110685  0.08628569 -0.02967289  0.174436053 -0.780121095  0.607197840  1.00000000  0.04023379
#hd.f       0.439485874 -0.054560206 -0.23566081  0.15599595 -0.01643619  0.479144716  0.078361923  0.117764733  0.04023379  1.00000000
library(ellipse)
plotcorr(corr)
# Do the same, but with colors corresponding to value
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(corr, col=rgb(colorfun((corr+1)/2), maxColorValue=255))