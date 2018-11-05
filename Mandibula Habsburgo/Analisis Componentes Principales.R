############################################################################################
#
# 3. ANALISIS DE COMPONENTES PRINCIPALES
#          
#############################################################################################
install.packages("readr")
install_github("kassambara/factoextra")

library(readr)
library(factoextra)

# Abro los documentos separados por ; 
md <- read.csv("~/md.csv", sep=";")
mp <- read.csv("~/mp.csv", sep=";")

#Preparo los datos
Fped<-mp[,12]
mp1<-mp[,1:11]
class(Fped)
Fped=as.factor(Fped)
##############################################################################

#Analisis de Componentes Principales
pca.mp<-princomp(mp1, cor = TRUE, scores = TRUE, scale=FALSE)
??pricomp #scale=FALSE: the variable standard deviations (the scaling applied to each variable )
#Scores: coordinates on each principal component

#Scores
ind.scores <- pca.mp$scores
ind.scores[,1:3]
pca.mp$loadings #the matrix of variable loadings (columns are eigenvectors, i.e. rotations)
mean(ind.scores[,1])
var(ind.scores[,1])

#Eigenvalues
get_eigenvalue(pca.mp)
Eigen <- eigen(cor(mp1))

# Resultados para las Variables!
var <- get_pca_var(pca.mp)
var

#Coordenadas de las variables para las 4 primeras componentes
var$coord[,1:4] #esto es: correlacion entre las variables y los componentes

#Otro metodo... a pinrel!
var_cor_func <- function(var.loadings, comp.sdev){
  var.loadings*comp.sdev
}
loadings <- pca.mp$loadings
sdev <- pca.mp$sdev
var.coord <- var.cor <- t(apply(loadings, 1, var_cor_func, sdev))
head(var.coord[, 1:4])
##########################################################################
############################################################################

########################################################################
#REPRESENTACIONES GRAFICAS

#Eigenvalues
windows()
fviz_screeplot(pca.mp,choice="eigenvalue")
###########################

#INDIVIDUOS
windows()
fviz_pca_ind(pca.mp)+labs(title ="PCA", x = "PC1", y = "PC2")

windows()
fviz_pca_ind(pca.mp, col.ind="cos2")
# cos2 = calidad de los individuos en el mapa factor
windows()
fviz_pca_ind(pca.mp, col.ind="cos2") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=0.5)
# Color por la contribucion individual
windows()
fviz_pca_ind(pca.mp, col.ind="contrib") +
  scale_color_gradient2(low="white", mid="blue",
                        high="red", midpoint=5)
# Color por grupo de consanguinidad
windows()
fviz_pca_ind(pca.mp, label="none", habillage=md$F)
windows()
p <- fviz_pca_ind(pca.mp, label="none", habillage=mp$F,
                  addEllipses=TRUE, ellipse.level=0.95)
p + scale_color_brewer(palette="Dark2") +
  theme_minimal()
print(p)
###########################

#VARIABLES!
#Correlacion entre variables y componentes principales como coordenadas:
windows()
fviz_pca_var(pca.mp)
windows()
fviz_pca_var(pca.mp, geom = c("point", "text"))

#value of cos2. The contribution of a variable to a given principal component is (in percentage)

#Contribucion de la variable a la componente principal
windows()
fviz_pca_var(pca.mp, col.var="contrib")+
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=7) + theme_minimal()
#valor de cos2: contribucion de la variable al componente principal correspondiente.
var$cos2[,1:4]
##############################

#VARIABLES E INDIVIDUOS
windows()
fviz_pca_biplot(pca.mp, label ="var")
windows()
fviz_pca_biplot(pca.mp, label ="ind")
windows()
fviz_pca_biplot(pca.mp, label =c("ind","var"))

windows()
fviz_pca_biplot(pca.mp, label="var", habillage=md$F,
                addEllipses=TRUE, ellipse.level=0.95)

biplot(pca.mp)

########################################################
#Regression F with MP

ind.coord.mp<-pca.mp$scores[,1:3]
id<-1:16
a1.mp<-cbind(ind.coord.mp,id)
a2<-cbind(mp$Inb,id);colnames(a2)=c("inb","id")
inb.dep.mp<-merge(a1.mp,a2,by="id")

pca1.mp<-lm(Comp.1 ~ inb, data = inb.dep.mp)
summary(pca1.mp)

pca2.mp<-lm(Comp.2 ~ inb, data = inb.dep.mp)
summary(pca2.mp)





