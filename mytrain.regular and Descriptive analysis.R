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
#Descriptive Data Analysis
attach(mytrain.re)
summary(mytrain.re$CoverType)
#1    2    3    4    5    6    7 
#1519 1507 1517 1520 1498 1501 1522
summary(mytrain.re$WArea)
summary(mytrain.re$SoilType)
pairs(mytrain.re)
cor(mytrain.re[,1:11])
