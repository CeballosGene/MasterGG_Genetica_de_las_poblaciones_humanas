library(plyr)
library(matrixStats)
library(readr)

#########################################################################################################
################# 1.  PROCESSING THE HOM FILES FROM PLINK
rm(list=ls())
results_kb <- function(class)  {
  this_iids_roh <- dat[class,]
  my_list<-c("mean"=mean(this_iids_roh$KB[this_iids_roh$KB>=1500]),
             "sum"=sum(this_iids_roh$KB[this_iids_roh$KB>=1500]),
             "length"=length(this_iids_roh$KB[this_iids_roh$KB>=1500]),
             "cl1"=sum(this_iids_roh$KB[this_iids_roh$KB<=500])-sum(this_iids_roh$KB[this_iids_roh$KB<300]),
             "cl2"=sum(this_iids_roh$KB[this_iids_roh$KB<=1000])-sum(this_iids_roh$KB[this_iids_roh$KB<500]),
             "cl3"=sum(this_iids_roh$KB[this_iids_roh$KB<=2000])-sum(this_iids_roh$KB[this_iids_roh$KB<1000]),
             "cl4"=sum(this_iids_roh$KB[this_iids_roh$KB<=4000])-sum(this_iids_roh$KB[this_iids_roh$KB<2000]),
             "cl5"=sum(this_iids_roh$KB[this_iids_roh$KB<=8000])-sum(this_iids_roh$KB[this_iids_roh$KB<4000]),
             "cl6"=sum(this_iids_roh$KB[this_iids_roh$KB>8000]),
             "Froh"=(sum(this_iids_roh$KB[this_iids_roh$KB>=1500]))/2881033)
  return(my_list)
}
files_list <- list.files("ROH/hom", full.names=TRUE)
dat <- data.frame()                          
for (i in 1:length(files_list)) {
  dat <- rbind(dat, read.table((files_list[i]),header=TRUE))
}
library(data.table)
dat <- as.data.table(dat)
dat$IID<- sub("^", "SC", dat$IID)
dat$IID<-as.factor(dat$IID)
setkey(dat,"IID")
results <- c()
nLevels <- length(levels(dat$IID))
start <- proc.time()
pb <- txtProgressBar(min = 0, max = nLevels, style = 3)
for (i in 1:nLevels){
  this_iid <- levels(dat$IID)[i]
  results <- rbind(results,results_kb(this_iid))
  setTxtProgressBar(pb,i)
}
close(pb)
proc.time()-start
results<-data.frame(levels(dat$IID),results)
results$IID<-results$levels.dat.IID.
results[results==0] <- NA

write.csv(results, file="ROH/CDX.csv")
########################################################################################################


###################################################################################################
############## 2.   OBTAINING THE SUMMARY STATISTICS. FOR WORLD POPULATIONS AND AFRICAN POPULATIONS (1KGENOMES, AGVP)
files<-dir("ROH",pattern="*.csv")
ids=gsub("","",gsub(".csv","",files))

lst<-list()
for (i in ids){
  lst[[i]]<-read.csv(paste0("ROH/",i,".csv"))
}

l1 <- lapply(lst, function(i) rowSums(i[,c(6,7,8)]))
l2 <- lapply(l1, function(i) cbind(mea = mean (i),sds = sd(i)))
l3 <- lapply(lst, function(i) t(cbind(Mean = colMeans(i[, c(3,4,5,12)], na.rm = TRUE), 
                                      Sds = colSds(as.matrix(i[, c(3,4,5,12)]), na.rm = TRUE),
                                      N = length(i[,2]))))

appendList <- function (x, val) 
{
  stopifnot(is.list(x), is.list(val))
  xnames <- names(x)
  for (v in names(val)) {
    x[[v]] <- if (v %in% xnames && is.list(x[[v]]) && is.list(val[[v]])) 
      appendList(x[[v]], val[[v]])
    else c(x[[v]], val[[v]])
  }
  x
}
l4<- appendList(l3,l2)
wo<-do.call(rbind, l4)
world<-wo[,-c(6,9,12)]
colnames(world) = c("mean","mean.sd","N","sum","sum.sd", "number","number.sd",
                    "Froh","Froh.sd","sum.low","sum.low.sd")


write.csv(world, file="ROH/World.csv")
########################################################################################################

###################################################################################################
############## 3.   INBREEDING COEFFICIENTS SUMMARY STATISTICS. FOR WORLD POPULATIONS AND AFRICAN POPULATIONS (1KGENOMES, AGVP)
files<-dir("ROH",pattern="*.csv")
ids=gsub("","",gsub(".csv","",files))

lst<-list()
for (i in ids){
  lst[[i]]<-read.csv(paste0("ROH/",i,".csv"))
}

a1 <- lapply(lst, function(i) t(cbind(f.max = max(i[, 12], na.rm = TRUE),
                                      fir.cous = length(which(i[,12]>=0.0625)),
                                      sec.cous = length(which(i[,12]>=0.0125)))))

world.F<-do.call(rbind, a1)
rownames(world.F)<-c("acb.max","acb.1cou","acb.2cou","asw.max","asw.1cou","asw.2cou","cdx.max","cdx.1cou","cdx.2cou","ceu.max","ceu.1cou","ceu.2cou",
                     "chb.max","chb.1cou","chb.2cou","chs.max","chs.1cou","chs.2cou","clm.max","clm.1cou","clm.2cou","fin.max","fin.1cou","fin.2cou",
                     "gbr.max","gbr.1cou","gbr.2cou","gih.max","gih.1cou","gih.2cou","ibs.max","ibs.1cou","ibs.2cou","jpt.max","jpt.1cou","jpt.2cou",
                     "khv.max","khv.1cou","khv.2cou","mxl.max","mxl.1cou","mxl.2cou","pel.max","pel.1cou","pel.2cou","pur.max","pur.1cou","pur.2cou",
                     "tsi.max","tsi.1cou","tsi.2cou")

write.csv(world.F, file="ROH/World.F.csv")





