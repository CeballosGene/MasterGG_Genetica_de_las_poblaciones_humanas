############################################################################################
#
# 2. REGRESIONES Y FOREST PLOT
#          
#############################################################################################
install.packages("readr")
install.packages("ggplot2")
install.packages("forestplot")

library(readr)
library(ggplot2)
library(forestplot)

# Abro los documentos separados por ;
r.md.mp.ajus<-read.csv("C:/Users/Rembukai/Desktop/Habsburg Jaw/regresion.md.mp.ajustado.csv", sep=";")
reg <- read.csv("C:/Users/Rembukai/Desktop/Habsburg Jaw/regresion.f.mp.md.csv", sep=";")

#Regresiones
a.md<- lm (md.a ~ fcoef , data=r.md.mp.ajus)
a.mp<- lm (mp.a ~ fcoef , data=r.md.mp.ajus)
summary(a.md);summary(a.mp)

#Representacion De las Regresiones
########################################################################################################################
# Multiple plot function
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  if (numPlots==1) {
    print(plots[[1]])
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
######################################################################

a.1<-ggplot(r.md.mp.ajus, aes(x=fcoef,y=md.a))+
  geom_point(color="black")+
  geom_smooth(method = "lm", se = TRUE,color="red")+
  scale_x_continuous(name="")+
  scale_y_continuous(name="MD")+
  theme_bw()+
  theme(panel.background = element_rect(colour="black",size=1))

a.2<-ggplot(r.md.mp.ajus, aes(x=fcoef,y=mp.a))+
  geom_point(color="black")+
  geom_smooth(method = "lm", se = TRUE,color="red")+
  scale_x_continuous(name="Inbreeding coefficient (F)")+
  scale_y_continuous(name="MP")+
  theme_bw()+
  theme(panel.background = element_rect(colour="black",size=1))

jpeg("C:/Users/Rembukai/Desktop/reg.jpeg",units="px",width=2000,height=2000,res=300)
multiplot(a.1,a.2,cols=2)
dev.off()
###################################################################

#Forest Plot

mp1<- lm (MP1 ~ inbcoef , data=reg);summary(md1);confint(md1,level=0.95)

var<-c("","MD1","MD2","MD3","MD4","MD5","MD6","MD7","MD8","MD9","MD10","MD11","","MP1","MP2","MP3","MP4","MP5","MP6","MP7")
tau<-c("Tau",0.499,0.153,0.181,-0.143,0.048,-0.232,0.143,0.172,-0.077,0.290,0.555,"",0.410,0.134,0.390,0.352,0.402,0.371,0.459)
tau.pval<-c("P",0.011,0.214,0.174,0.229,0.402,0.116,0.229,0.186,0.346,0.068,0.002,"",0.017,0.224,0.021,0.034,0.019,0.027,0.009)
space<-c("","","","","","","","","","","","","","","","","","","","")
beta<-c("Reg. coef.",0.481,0.448,0.118,-0.706,-0.079,-1.027,1.332,0.317,-0.247,0.602,1.737,"",0.820,0.382,1.818,1.193,1.484,1.252,1.247)
beta.pval<-c("P",0.087,0.224,0.428,0.098,0.460,0.183,0.124,0.123,0.354,0.138,0.011,"",0.066,0.313,0.030,0.061,0.009,0.095,0.042)
text<-cbind(var,tau,tau.pval,space,beta,beta.pval)

mean<- c(NA,0.481,0.4483,0.11807,-0.7064,-0.0785,-1.0267,1.3318,0.3168,-0.247,0.601,1.736,NA,0.8199,0.3819,1.818,1.192,1.483,1.252,1.2466)
lower<-c(NA,-0.242,-0.6728,-1.258,-1.823,-1.705,-3.389,-1.042,-0.241,-1.636,-0.537,0.3128,NA,-0.283,-1.266,-0.0008,-0.3603,0.2966,-0.7023,-0.195)
upper<- c(NA,1.204,1.5694,1.494,0.4112,1.5488,1.3364,3.706,0.8807,1.142,1.740,3.160,NA,1.922,2.0306,3.725,2.754,2.6704,3.206,2.68)
data<-cbind(mean,lower,upper)

jpeg("~/forest.jpeg",units="px",width=2500,height=2000,res=300)
forestplot(text,data,
           hrzl_lines = gpar(col="#444444"),
           lwd.zero = gpar(cex=2),
           is.summary = c(TRUE,rep(FALSE,19)),
           txt_gp = fpTxtGp(xlab=gpar(cex =1),
                            ticks=gpar(cex=1),
                            label=gpar(cex=0.8)),
           col=fpColors(box="royalblue",line="darkblue"),
           ci.vertices = TRUE,boxsize = 0.4,
           xlab="Regression Coef. and 95% CI")
dev.off()

