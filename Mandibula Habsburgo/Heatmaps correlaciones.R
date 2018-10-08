############################################################################################
#
# 1. Correlaciones entre los distintos valores de MD y MP. Construccion de un heatmap
#         . P-valores (correlacion de pearson) 
#         . Correlacion de pearson
#
#############################################################################################
install.packages("readr")
library(readr)

# Abro los documentos separados por ; 
heatmap.cor <- read.csv("~/heatmap.cor.csv", sep=";")
heatmap.pval <- read.csv("~/heatmap.pval.csv", sep=";")

#Preparo los datos de probabilidad para representarlos
pval<-as.data.frame(pval)
lev<-c("MD1","MD2","MD3","MD4","MD5","MD6","MD7","MD8","MD9","MD10","MD11",
       "MP1","MP2","MP3","MP4","MP5","MP6","MP7")
pval$X1 <- factor(pval$X1, levels = lev)
pval$X2 <- factor(pval$X2, levels = lev)
pval$value<-cut(pval$value, breaks=c(-Inf,0.05,Inf),right=F)
levels(pval$value) = c("(-Inf,0.05]","(0.05,1]")

#Represento el heatmap de los pe valores
jpeg("~/F1.jpeg",units="px",width=2000,height=2000,res=300)
ggplot(data = pval, aes(X1, X2, fill = value))+
  ggtitle("")+
  xlab("")+
  ylab("")+
  geom_tile(aes(fill=pval$value),color="white")+
  scale_fill_brewer(palette="Reds", direction = -1,drop=FALSE,name="p-Val")+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
dev.off()
################

#Preparo los datos de las correlaciones para representarlos
cor<-as.data.frame(cor)
lev<-c("MD1","MD2","MD3","MD4","MD5","MD6","MD7","MD8","MD9","MD10","MD11",
       "MP1","MP2","MP3","MP4","MP5","MP6","MP7")
cor$X1 <- factor(cor$X1, levels = lev)
cor$X2 <- factor(cor$X2, levels = lev)
cor$value<-cut(cor$value, breaks=c(-1,0.25,0.5,0.75,1),right=F)
levels(cor$value) = c("<+0.25","[0.25-0.5)","[0.5-0.75)","[0.75-1)")

#Represento el heatmap de las correlaciones
jpeg("~/F2.jpeg",units="px",width=2000,height=2000,res=300)
ggplot(data = cor, aes(X1, X2, fill = value))+
  ggtitle("")+
  xlab("")+
  ylab("")+
  geom_tile(aes(fill=cor$value),color="white")+
  scale_fill_brewer(palette="Blues", direction = +1,drop=FALSE,name="Cor")+
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()
dev.off()
