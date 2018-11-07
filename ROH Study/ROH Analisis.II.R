library(vioplot)
library(readr)
library(stringi)
library(plyr)
library(matrixStats)
#########################################################################################################
################# 1.  OPEN DATA
ACB <- read.csv("ROH/ACB.csv")
ASW <- read.csv("ROH/ASW.csv")
CDX <- read.csv("ROH/CDX.csv")
CEU <- read.csv("ROH/CEU.csv")
CHB <- read.csv("ROH/CHB.csv")
CHS <- read.csv("ROH/CHS.csv")
CLM <- read.csv("ROH/CLM.csv")
FIN <- read.csv("ROH/FIN.csv")
GBR <- read.csv("ROH/GBR.csv")
IBS <- read.csv("ROH/IBS.csv")
JPT <- read.csv("ROH/JPT.csv")
KHV <- read.csv("ROH/KHV.csv")
MXL <- read.csv("ROH/MXL.csv")
PEL <- read.csv("ROH/PEL.csv")
PUR <- read.csv("ROH/PUR.csv")
TSI <- read.csv("ROH/TSI.csv")
LWK <- read.csv("ROH/LWK.csv")
YRI <- read.csv("ROH/YRI.csv")
GIH <- read.csv("ROH/GIH.csv")
########################################################################################################

###################################################################################################
############## 2.  ARRANGING DATA FOR FIGURE 1. sum of ROH by ROH class

world=c("CEU","GBR","FIN","TSI","IBS","CHB","CHS","CDX","JPT","KHV","GIH","MXL","PUR","CLM","PEL","ACB","ASW","LWK","YRI")


for (i in world){  
  df<-get(i)
  df<-data.frame(df[,-c(1,2,3,4,5,12,13)])
  mean<-colMeans(df,na.rm=TRUE)
  assign(paste0(i,"."), mean)
}

Africa<-data.frame(t(rbind(YRI.,LWK.)))

Europe.north<-data.frame(t(rbind(CEU.,GBR.,FIN.)))                      
Europe.south<-data.frame(t(rbind(IBS.,TSI.)))

Asia.china<-data.frame(t(rbind(CHB.,CHS.,CDX.)))
Asia.southeast<-data.frame(t(rbind(KHV.)))
Asia.japan<-data.frame(t(rbind(JPT.)))
Asia.india<-data.frame(t(rbind(GIH.)))

America.south<-data.frame(t(rbind(PEL.)))

Mixed.hips.lat<-data.frame(t(rbind(MXL.,PUR.,CLM.)))
Mixed.afri<-data.frame(t(rbind(ASW.,ACB.)))
########################################################################################################

###################################################################################################
############## 3.  FIGURE 1. SUM OF ROH BY 6 CLASSES: 0.3-0.5, 0.5-1, 1-2, 2-4, 4-8, >8 Mb.
jpeg("ROH/Fig.1.World.fig.jpeg",units="px",width=2500,height=2000,res=300)
plot(NULL,xlim=c(1,6),ylim=c(0,155),xaxt='n',xlab="ROH length category (Mb)", ylab="Mean total length of ROH per population (Mb)")
points(Africa$YRI./1000,type="b",pch=15,col="gold3")
points(Africa$LWK./1000,type="b",pch=19,col="gold3")
points(Mixed.afri$ASW./1000,type="b",pch=17,col="royalblue4")
points(Mixed.afri$ACB./1000,type="b",pch=19,col="royalblue4")
points(Europe.north$CEU./1000,type="b",pch=17,col="Aquamarine2")
points(Europe.north$GBR./1000,type="b",pch=15,col="Aquamarine2")
points(Europe.north$FIN./1000,type="b",pch=16,col="Aquamarine2")
points(Europe.south$TSI./1000,type="b",pch=18,col="Aquamarine2")
points(Europe.south$IBS./1000,type="b",pch=8,col="Aquamarine2")
points(Asia.india$GIH./1000,type="b",pch=17,col="beige")
points(Asia.southeast$KHV./1000,type="b",pch=17,col="darksalmon")
points(Asia.japan$JPT./1000,type="b",pch=15,col="darksalmon")
points(Asia.china$CHS./1000,type="b",pch=18,col="darksalmon")
points(Asia.china$CHB./1000,type="b",pch=16,col="darksalmon")
points(Asia.china$CDX./1000,type="b",pch=8,col="darksalmon")
points(America.south$PEL./1000,type="b",pch=17,col="darkorange3")
points(Mixed.hips.lat$MXL./1000,type="b",pch=17,col="greenyellow")
points(Mixed.hips.lat$CLM./1000,type="b",pch=15,col="greenyellow")
points(Mixed.hips.lat$PUR./1000,type="b",pch=16,col="greenyellow")
#segments(1,49.662,1,60.933,col="black",lty=1,lwd=2)
#segments(2,48.8,2,36.7,col="black",lty=1,lwd=2)
axis(1, at = c(1,2,3,4,5,6), 
     labels = c("[0.3-0.5)", "[0.5-1)", "[1-2)", "[2-4)", "[4-8)", "> 8"),las=1)

legend(4,160, c("TSI","IBS","CEU","GBR","FIN","GIH","KHV","JPT","CHS","CHB","CDX",
                   "YRI","LWK","PEL","CLM","PUR","MXL","ASW","ACB"),
       
       pch = c(18,8,17,15,16,17,17,15,18,16,8,15,19,17,15,16,17,17,19),
       lty=c(1,1,1,1),
       col = c("aquamarine2","aquamarine2","aquamarine2","aquamarine2","aquamarine2","beige",
               "darksalmon","darksalmon","darksalmon","darksalmon","darksalmon","gold3","gold3",
               "darkorange3","greenyellow","greenyellow","greenyellow","royalblue4","royalblue4"))
dev.off()
########################################################################################################

###################################################################################################
############## 4.  VIOLIN PLOT
all=c("CEU","GBR","FIN","TSI","IBS","CHB","CHS","CDX","JPT",
      "KHV","GIH","MXL","PUR","CLM","PEL","ACB","ASW","LWK","YRI")

for (i in all){  
  df<-get(i)
  clean<-df$mean[!is.na(df$mean)]
  assign(paste0(i,".mean"), clean)
}

for (i in all){  
  df<-get(i)
  clean<-df$length[!is.na(df$length)]
  assign(paste0(i,".len"), clean)
}

for (i in all){  
  df<-get(i)
  clean<-df$sum[!is.na(df$sum)]
  assign(paste0(i,".sum"), clean)
}

#Obtaining sum of Cl1, Cl2, Cl3 for all the populations. Cleaning the NA
for (i in all){  
  df<-get(i)
  clean<-df$cl1+df$cl2+df$cl3
  assign(paste0(i,"."), clean)
}

all=c("CEU.","GBR.","FIN.","TSI.","IBS.","CHB.","CHS.","CDX.","JPT.","KHV.","GIH.","MXL.","PUR.","CLM.","PEL.",
      "ACB.","ASW.","LWK.","YRI.")

for (i in all){  
  df<-get(i)
  clean<-df[!is.na(df)]
  assign(paste0(i,"cl"), clean)
}
#FINAL FIGURE. world. HORIZONTAL. 4 GRAPHS TOGETHER
jpeg("C:/Users/Rembukai/Desktop/plot.3.jpeg",units="px",width=2700,height=2000,res=300)
mat<-(matrix(1:4,ncol=4))
layout(mat,widths = rep.int(1, ncol(mat)),heights = rep.int(1, nrow(mat)),respect =F)
layout.show(n = 4)
par(mar=c(4, 0.2, 2, 0.2), oma=c(0, 7, 0, 0.5))
plot(1, 1, ylim = c(0, 26), xlim = range(c(CLM.len,PEL.len,PUR.len)), type = 'n', 
     main="N. ROH >1.5Mb", xlab = '', ylab = '', yaxt = 'n')
vioplot(MXL.len,horizontal=TRUE,at=1,add=T,col="greenyellow")
vioplot(PUR.len,horizontal=TRUE,at=2,add=T,col="greenyellow")
vioplot(CLM.len,horizontal=TRUE,at=3,add=T,col="greenyellow")
vioplot(PEL.len,horizontal=TRUE,at=5,add=T,col="darkorange3")
vioplot(LWK.len,horizontal=TRUE,at=7,add=T,col="cyan3")
vioplot(YRI.len,horizontal=TRUE,at=8,add=T,col="burlywood4")
vioplot(ACB.len,horizontal=TRUE,at=10,add=T,col="royalblue4")
vioplot(ASW.len,horizontal=TRUE,at=11,add=T,col="royalblue4")
vioplot(CDX.len,horizontal=TRUE,at=13,add=T,col="darksalmon")
vioplot(CHB.len,horizontal=TRUE,at=14,add=T,col="darksalmon")
vioplot(CHS.len,horizontal=TRUE,at=15,add=T,col="darksalmon")
vioplot(JPT.len,horizontal=TRUE,at=16,add=T,col="darksalmon")
vioplot(KHV.len,horizontal=TRUE,at=17,add=T,col="darksalmon")
vioplot(GIH.len,horizontal=TRUE,at=19,add=T,col="beige")
vioplot(FIN.len,horizontal=TRUE,at=21,add=T,col="aquamarine2")
vioplot(GBR.len,horizontal=TRUE,at=22,add=T,col="aquamarine2")
vioplot(CEU.len,horizontal=TRUE,at=23,add=T,col="aquamarine2")
vioplot(IBS.len,horizontal=TRUE,at=24,add=T,col="aquamarine2")
vioplot(TSI.len,horizontal=TRUE,at=25,add=T,col="aquamarine2")
axis(2, at = c(1,2,3,5,7,8,10,11,13,14,15,16,17,19,21,22,23,24,25), 
     labels = c("MXL","PUR","CLM","PEL","LWK","YRI","ACB","ASW","CDX","CHB","CHS","JPT","KHV","GHI","FIN","GBR","CEU","IBS","TSI"),las=2)

plot(1, 1, ylim = c(0, 26), xlim = range(c(CLM.mean/1000,PEL.mean/1000,PUR.mean/1000,GBR.mean/1000)), type = 'n', 
     main="Mean ROH>1.5Mb", xlab = 'Mb', ylab = '', yaxt = 'n')
vioplot(MXL.mean/1000,horizontal=TRUE,at=1,add=T,col="greenyellow")
vioplot(PUR.mean/1000,horizontal=TRUE,at=2,add=T,col="greenyellow")
vioplot(CLM.mean/1000,horizontal=TRUE,at=3,add=T,col="greenyellow")
vioplot(PEL.mean/1000,horizontal=TRUE,at=5,add=T,col="darkorange3")
vioplot(LWK.mean/1000,horizontal=TRUE,at=7,add=T,col="cyan3")
vioplot(YRI.mean/1000,horizontal=TRUE,at=8,add=T,col="burlywood4")
vioplot(ACB.mean/1000,horizontal=TRUE,at=10,add=T,col="royalblue4")
vioplot(ASW.mean/1000,horizontal=TRUE,at=11,add=T,col="royalblue4")
vioplot(CDX.mean/1000,horizontal=TRUE,at=13,add=T,col="darksalmon")
vioplot(CHB.mean/1000,horizontal=TRUE,at=14,add=T,col="darksalmon")
vioplot(CHS.mean/1000,horizontal=TRUE,at=15,add=T,col="darksalmon")
vioplot(JPT.mean/1000,horizontal=TRUE,at=16,add=T,col="darksalmon")
vioplot(KHV.mean/1000,horizontal=TRUE,at=17,add=T,col="darksalmon")
vioplot(GIH.mean/1000,horizontal=TRUE,at=19,add=T,col="beige")
vioplot(FIN.mean/1000,horizontal=TRUE,at=21,add=T,col="aquamarine2")
vioplot(GBR.mean/1000,horizontal=TRUE,at=22,add=T,col="aquamarine2")
vioplot(CEU.mean/1000,horizontal=TRUE,at=23,add=T,col="aquamarine2")
vioplot(IBS.mean/1000,horizontal=TRUE,at=24,add=T,col="aquamarine2")
vioplot(TSI.mean/1000,horizontal=TRUE,at=25,add=T,col="aquamarine2")

plot(1, 1, ylim = c(0, 26), xlim = range(c(CLM.sum/1000000,PEL.sum/1000000,PUR.sum/1000000)), type = 'n', 
     main="Total Sum of ROH", xlab = 'Gb', ylab = '', yaxt = 'n')
vioplot(MXL.sum/1000000,horizontal=TRUE,at=1,add=T,col="greenyellow")
vioplot(PUR.sum/1000000,horizontal=TRUE,at=2,add=T,col="greenyellow")
vioplot(CLM.sum/1000000,horizontal=TRUE,at=3,add=T,col="greenyellow")
vioplot(PEL.sum/1000000,horizontal=TRUE,at=5,add=T,col="darkorange3")
vioplot(LWK.sum/1000000,horizontal=TRUE,at=7,add=T,col="cyan3")
vioplot(YRI.sum/1000000,horizontal=TRUE,at=8,add=T,col="burlywood4")
vioplot(ACB.sum/1000000,horizontal=TRUE,at=10,add=T,col="royalblue4")
vioplot(ASW.sum/1000000,horizontal=TRUE,at=11,add=T,col="royalblue4")
vioplot(CDX.sum/1000000,horizontal=TRUE,at=13,add=T,col="darksalmon")
vioplot(CHB.sum/1000000,horizontal=TRUE,at=14,add=T,col="darksalmon")
vioplot(CHS.sum/1000000,horizontal=TRUE,at=15,add=T,col="darksalmon")
vioplot(JPT.sum/1000000,horizontal=TRUE,at=16,add=T,col="darksalmon")
vioplot(KHV.sum/1000000,horizontal=TRUE,at=17,add=T,col="darksalmon")
vioplot(GIH.sum/1000000,horizontal=TRUE,at=19,add=T,col="beige")
vioplot(FIN.sum/1000000,horizontal=TRUE,at=21,add=T,col="aquamarine2")
vioplot(GBR.sum/1000000,horizontal=TRUE,at=22,add=T,col="aquamarine2")
vioplot(CEU.sum/1000000,horizontal=TRUE,at=23,add=T,col="aquamarine2")
vioplot(IBS.sum/1000000,horizontal=TRUE,at=24,add=T,col="aquamarine2")
vioplot(TSI.sum/1000000,horizontal=TRUE,at=25,add=T,col="aquamarine2")

plot(1, 1, ylim = c(0, 26), xlim = c(0, 0.5), type = 'n', 
     main="Sum ROH<1.5Mb", xlab = 'Gb', ylab = '', yaxt = 'n')
vioplot(MXL.cl/1000000,horizontal=TRUE,at=1,add=T,col="greenyellow")
vioplot(PUR.cl/1000000,horizontal=TRUE,at=2,add=T,col="greenyellow")
vioplot(CLM.cl/1000000,horizontal=TRUE,at=3,add=T,col="greenyellow")
vioplot(PEL.cl/1000000,horizontal=TRUE,at=5,add=T,col="darkorange3")
vioplot(LWK.cl/1000000,horizontal=TRUE,at=7,add=T,col="cyan3")
vioplot(YRI.cl/1000000,horizontal=TRUE,at=8,add=T,col="burlywood4")
vioplot(ACB.cl/1000000,horizontal=TRUE,at=10,add=T,col="royalblue4")
vioplot(ASW.cl/1000000,horizontal=TRUE,at=11,add=T,col="royalblue4")
vioplot(CDX.cl/1000000,horizontal=TRUE,at=13,add=T,col="darksalmon")
vioplot(CHB.cl/1000000,horizontal=TRUE,at=14,add=T,col="darksalmon")
vioplot(CHS.cl/1000000,horizontal=TRUE,at=15,add=T,col="darksalmon")
vioplot(JPT.cl/1000000,horizontal=TRUE,at=16,add=T,col="darksalmon")
vioplot(KHV.cl/1000000,horizontal=TRUE,at=17,add=T,col="darksalmon")
vioplot(GIH.cl/1000000,horizontal=TRUE,at=19,add=T,col="beige")
vioplot(FIN.cl/1000000,horizontal=TRUE,at=21,add=T,col="aquamarine1")
vioplot(GBR.cl/1000000,horizontal=TRUE,at=22,add=T,col="aquamarine2")
vioplot(CEU.cl/1000000,horizontal=TRUE,at=23,add=T,col="aquamarine2")
vioplot(IBS.cl/1000000,horizontal=TRUE,at=24,add=T,col="aquamarine2")
vioplot(TSI.cl/1000000,horizontal=TRUE,at=25,add=T,col="aquamarine2")
dev.off()
########################################################################################################

###################################################################################################
############## 5.  FIGURE NROH VS SROH.
jpeg("C:/Users/Rembukai/Desktop/Fig.4.jpeg",units="px",width=1900,height=1900,res=300)
par(mar=c(4,4,1,1))
plot(mean(LWK$sum/1000,na.rm=TRUE),mean(LWK$length,na.rm=TRUE),ylim=c(1,18),xlim=c(1,47),
     xlab="Total Sum of ROH (Mb)",ylab="Number of ROH",
     pch = 2 ,col= "brown3")
points(mean(YRI$sum/1000,na.rm=TRUE),mean(YRI$length,na.rm=TRUE), pch = 2 ,col= "brown3" )
points(mean(CEU$sum/1000,na.rm=TRUE),mean(CEU$length,na.rm=TRUE), pch = 0 ,col= "darkorchid1")
points(mean(GBR$sum/1000,na.rm=TRUE),mean(GBR$length,na.rm=TRUE), pch = 0 ,col= "darkorchid1")
points(mean(FIN$sum/1000,na.rm=TRUE),mean(FIN$length,na.rm=TRUE), pch = 0 ,col= "darkorchid1")
points(mean(IBS$sum/1000,na.rm=TRUE),mean(IBS$length,na.rm=TRUE), pch = 1 ,col= "darkorchid1")
points(mean(TSI$sum/1000,na.rm=TRUE),mean(TSI$length,na.rm=TRUE), pch = 1 ,col= "darkorchid1")
points(mean(CHB$sum/1000,na.rm=TRUE),mean(CHB$length,na.rm=TRUE), pch = 0 ,col= "tan3")
points(mean(CHS$sum/1000,na.rm=TRUE),mean(CHS$length,na.rm=TRUE), pch = 0 ,col= "tan3")
points(mean(CDX$sum/1000,na.rm=TRUE),mean(CDX$length,na.rm=TRUE), pch = 0 ,col= "tan3")
points(mean(KHV$sum/1000,na.rm=TRUE),mean(KHV$length,na.rm=TRUE), pch = 1 ,col= "tan3")
points(mean(JPT$sum/1000,na.rm=TRUE),mean(JPT$length,na.rm=TRUE), pch = 2 ,col= "tan3")
points(mean(GIH$sum/1000,na.rm=TRUE),mean(GIH$length,na.rm=TRUE), pch = 5 ,col= "deeppink3")
points(mean(PEL$sum/1000,na.rm=TRUE),mean(PEL$length,na.rm=TRUE), pch = 0 ,col= "olivedrab3")
points(mean(MXL$sum/1000,na.rm=TRUE),mean(MXL$length,na.rm=TRUE), pch = 0 ,col= "royalblue3")
points(mean(PUR$sum/1000,na.rm=TRUE),mean(PUR$length,na.rm=TRUE), pch = 0 ,col= "royalblue3")
points(mean(CLM$sum/1000,na.rm=TRUE),mean(CLM$length,na.rm=TRUE), pch = 0 ,col= "royalblue3")
points(mean(ASW$sum/1000,na.rm=TRUE),mean(ASW$length,na.rm=TRUE), pch = 1 ,col= "royalblue3")
points(mean(ACB$sum/1000,na.rm=TRUE),mean(ACB$length,na.rm=TRUE), pch = 1 ,col= "royalblue3")
ASW$S<-ASW$sum/1000
ASW$L<-ASW$length
abline(reg<-lm(ASW$L ~ ASW$S), col="red")
legend(30,7,
       c("Europe-North","Europe-South","Nat.American","Asia-China","Asia-Japan","Asia-S.E","Asia-India",
         "Africa","Mixed-Hisp.Amer","Mixed-Afri.Amer"),
       pch=c(0,1,0,0,2,1,5,2,0,1),
       col=c("darkorchid1","darkorchid1","olivedrab3","tan3","tan3","tan3","deeppink3",
             "brown3","royalblue3","royalblue3"))
dev.off()
########################################################################################################



