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
head(mytrain.re)
head(mytest.re)
total<-rbind(mytrain.re,mytest.re)
summary(total$CoverType)
#1    2    3    4    5    6    7 
#2160 2160 2160 2160 2160 2160 2160 
corr<-cor(total[,1:10])
#          elevation      aspect       slope        hd.h        vd.h         hd.r         hs.9        hs.n        hs.3        hd.f
#elevation  1.00000000 -0.01109605 -0.31263950  0.41271152  0.12209160  0.578658991  0.097900204  0.21578205  0.08951758  0.44356343
#aspect    -0.01109605  1.00000000  0.02814799  0.04073212  0.05641234  0.066184194 -0.593997428  0.32491229  0.63502236 -0.05216918
#slope     -0.31263950  0.02814799  1.00000000 -0.05597593  0.26531398 -0.277048754 -0.200072122 -0.61261287 -0.32688704 -0.23952722
#hd.h       0.41271152  0.04073212 -0.05597593  1.00000000  0.65214247  0.203397008 -0.033802678  0.08004720  0.08083254  0.15881693
#vd.h       0.12209160  0.05641234  0.26531398  0.65214247  1.00000000  0.011554979 -0.095929918 -0.13294827 -0.03555906 -0.01504833
#hd.r       0.57865899  0.06618419 -0.27704875  0.20339701  0.01155498  1.000000000 -0.003398026  0.24068384  0.17388200  0.48638565
#hs.9       0.09790020 -0.59399743 -0.20007212 -0.03380268 -0.09592992 -0.003398026  1.000000000 -0.01371238 -0.77996474  0.07814412
#hs.n       0.21578205  0.32491229 -0.61261287  0.08004720 -0.13294827  0.240683844 -0.013712384  1.00000000  0.61452639  0.12209774
#hs.3       0.08951758  0.63502236 -0.32688704  0.08083254 -0.03555906  0.173881997 -0.779964742  0.61452639  1.00000000  0.04316247
#hd.f       0.44356343 -0.05216918 -0.23952722  0.15881693 -0.01504833  0.486385645  0.078144122  0.12209774  0.04316247  1.00000000
library(ellipse)
plotcorr(corr)
# Do the same, but with colors corresponding to value
colorfun <- colorRamp(c("#CC0000","white","#3366CC"), space="Lab")
plotcorr(corr, col=rgb(colorfun((corr+1)/2), maxColorValue=255))
#Radar Plot
test0<-by(total[,c(1:10)],total$CoverType , FUN=colMeans)
test1<-do.call(rbind,test0)
stars(test1, locations = c(0, 0),  radius  =  FALSE, col.lines = 1:10, key.loc = c(0, 0))
#histograms
require(ggplot2)
library(grid)
library(gridExtra)

head(total)
d1<-ggplot(total, aes(x = elevation, fill = CoverType)) + geom_density(alpha = 0.3)
d2<-ggplot(total, aes(x = aspect, fill = CoverType)) + geom_density(alpha = 0.3)
d3<-ggplot(total, aes(x = slope, fill = CoverType)) + geom_density(alpha = 0.3)
d4<-ggplot(total, aes(x = hd.h, fill = CoverType)) + geom_density(alpha = 0.3)
d5<-ggplot(total, aes(x = vd.h, fill = CoverType)) + geom_density(alpha = 0.3)
d6<-ggplot(total, aes(x = hd.r, fill = CoverType)) + geom_density(alpha = 0.3)
d7<-ggplot(total, aes(x = hs.9, fill = CoverType)) + geom_density(alpha = 0.3)
d8<-ggplot(total, aes(x = hs.n, fill = CoverType)) + geom_density(alpha = 0.3)
d9<-ggplot(total, aes(x = hs.3, fill = CoverType)) + geom_density(alpha = 0.3)
d10<-ggplot(total, aes(x = hd.f, fill = CoverType)) + geom_density(alpha = 0.3)
d123<-grid.arrange(d1, d2,d3,ncol = 3, main = "Density Plot for elevation,aspect and slope")
d45610<-grid.arrange(d4, d5,d6,d10,ncol = 2, main = "Density Plot for hd.h,vd.h,hd.r and hd.f")
d789<-grid.arrange(d7,d8,d9,ncol = 3, main = "Density Plot Hillshade at 9am, noon and 3pm ")
ggplot(total, aes(CoverType, elevationt)) + geom_boxplot(aes(fill=CoverType))
