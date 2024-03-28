
# seminar 1 ---------------------------------------------------------------

setwd("T:\\adsemiar")
X <- read.table(file="AD_final.txt",header=T,sep="\t",dec=",",row.names=1)
#X <- read.csv("ADfinal.csv", header = T, sep = ",", dec =".")
summary(X)
dim(X)
rows <- c(234,233,232,231)
X <- X[-rows,]

names(X)
#data_frame <- as.data.frame(X)
#hist(data_frame$X1,col="#909865")  
#boxplot(data_frame$X1, col ="#243574")

hist(X$X1,col="#456627")
boxplot(X$X1,col="#456627",horizontal=TRUE)

hist(X$X3,col="#953627")
boxplot(X$X3,col="#953627",horizontal=T)

hist(X$X4,col="#123628")
boxplot(X$X4,col="#123628",horizontal=T)

hist(X$X5,col="#987627")
boxplot(X$X5,col="#987627", horizontal=T)

hist(X$X6,col="#123427")
boxplot(X$X6,col="#123427",horizontal=T)

hist(X$X7,col="#998327")
boxplot(X$X7,horizontal=TRUE,col="#998327")

hist(X$X8,col="#977327")
boxplot(X$X8,horizontal=TRUE,col="#977327")

hist(X$X9,col="#798237")
boxplot(X$X9,horizontal=TRUE,col="#798237")

hist(X$X10,col="#9981A0")
boxplot(X$X10,horizontal=TRUE,col="#9981A0")

#Punctul 3
## extragere vector cu abatere standard maxima si minima
abateri <- apply(X,2,sd)
abateri
which.max(abateri)
X_col1 <- X[,which.max(abateri)]
## vector coloana 1
X_col2 <- X[,which.min(abateri)]
## vector coloana 2

## ?norm

n1 <- norm(X_col1,type="2")
n2 <- norm(X_col2,type="2")
## type 2 - norma euclidiana (2-norm)

## CENTRARE VECTORI
## centram prin scale, adica se scade media 
##(ca sa devina de medie 0 si abatere standard 1)
X1_centrat <- scale(X_col1,center=T,scale=F)
X2_centrat <- scale(X_col2,center=T,scale=F)

#calcul cosinus unghi
n1 <- norm(X1_centrat,type="2")
n2 <- norm(X2_centrat,type="2")

dim(X1_centrat)
dim(X2_centrat)

cos <- (t(X1_centrat)%*%X2_centrat) / (n1 * n2)
cos

cor(X1_centrat,X2_centrat)


# Calcul distanta euclidiana regiuni RO -----------------------------------

reRO<-c("RO11","RO12","RO21","RO22","RO31","RO32","RO41","RO42")
which(row.names(X) %in% reRO)
dist(rbind(X[205,],X[206,]),method="euclidian")

# install.packages("extrafont")
library(extrafont)
font_import()
loadfonts()
fontFamily <- "Times New Roman"
par(family = fontFamily)

# Pas 6
#install.packages("corrplot")
library(corrplot)
corel <- cor(X)
colors <- colorRampPalette(c("#D8D2FA", "white", "#C6E284"))(100)
corrplot(corel,method="number",type="upper",bg='darkgray',family=fontFamily,col=colors,outline=T,sig.level = 0.05,shade.col = "white",addCoef.col=T,  tl.col = "BLACK", number.font = 10)

## Matricea de covarianta
cov(X)

# Pas 7 - matricea produselor incrucisate
## XC este matricea cu variabilele centrate
XC <- scale(X,center=T,scale=F)
XC
## pi este matricea produselor incrucisate ==> este simetrica
## pi is used to summarize the relationships between variables in a dataset.
## matricea produselor incrucisate pe date centrate este matricea de covarianta
n <- dim(X)[1]-1
pi <- t(XC)%*%XC/n
pi

# Pas 8 - matricea de covarianta pentru variabilele centrate
## S is used to find the principal components of a dataset. 
CC <- cov(XC)
CC
round(CC-pi)

## XS este matricea cu variabilele standardizate
XS <- scale(X, center=T, scale=T)
## matricea de covarianta pentru variabilele standardizate
## corel este matricea de corelatie calculata anterior
CS <- cov(XS)
round(CS-corel)
## daca datele sunt standardizate, S2 si corel sunt egale

# Pas 9 - valori proprii
## e = valorile proprii ale matricii de covarianta pentru variabilele standardizate
e <- eigen(CS)
e$values

# Pas 10
#install.packages("ggplot2")
library("ggplot2")
ggplot(X,aes(x=X1,y=X2))+geom_point(shape=13,alpha=.4)+geom_text(label=row.names(X),vjust=0,hjust=0,size=4)+ labs(x="X1",y="X2")

# seminar 4 ---------------------------------------------------------------
XF <- X
R <- cor(XF)
R

corel <- cor(X)
corel
corrplot(corel,method="color",type="upper",bg="black",outline=T,title="Matrice de corelatie",sig.level = 0.05,shade.col = "white",addCoef.col=T,  tl.col = "BLACK", number.font = 10)


# seminar 5 ---------------------------------------------------------------
XC <- scale(XF,center=T,scale=F)
n <- dim(XF)[1]
PI <- (t(XC)%*%XC)/(n-1)
PI
C <- cov(XC)
round(PI-C)

#valori proprii
e <- eigen(C)
e
lambda <- e$values
lambda
## V = matrice cu vectorii proprii
V <- e$vectors ## uneori sunt coeficientii "loadings" cu semnul schimbat
V

## descompunerea
descompunereJ <- V%*%diag(lambda)%*%t(V)
descompunereJ

#primul vector propriu are norma 1?
v1 <- V[,1]
norm(V[,1],type="2")
sum(v1^2)

# Folosire functie princomp()
#?princomp()

acp <- princomp(XF,cor=F,scores=T)

# identificare elemente output
summary(acp)
sqrt(lambda) ## abaterile standard ale componentelor principale
sqrt(lambda[1])
sqrt(lambda[2])

# ce sunt loadings? comp cu V
# ce observati?
coef <- acp$loadings
coef
V

# loadings- avem vectori proprii ai matricii de cov
# inmultirea cu o constanta a unui vector propriu conduce tot la un vector propriu
# inmultirea cu -1 conduce tot la un vect propriu

#ce reprezinta scores?
scoruri <- acp$scores
scoruri

ponderare <- XC%*%coef

#tema!! cor=T - pasii 5, 8

# seminar 5 real ----------------------------------------------------------

xf <- XF
acp <- princomp(xf,cor=T,scores=T)
acp
var <- acp$sdev
var
coef <- acp$loadings
coef
scoruri <- acp$scores
scoruri

# 2 summary
summary(acp)

# 3 - reprezentare grafica
plot(acp,type='l',main="Scree Plot",col='plum4')
abline(h=1,col='chocolate4')

# 4 - print
print(acp)

# 5 - diverse
R <- cor(XF)
R

## E = valorile proprii pentru matricea de corelatie
E <- eigen(R)
E

v <- E$vectors
v

#uneori sunt coeficientii "loadings" cu -
lambda <- E$values
lambda
sqrt(lambda) #abatere standard componente principale
comblin <- scale(XF, center=T, scale=T)%*%coef
comblin

# 6 - matrice factor - da noilor variabile o interpretare concreta
MF=cor(XF,scoruri)
corrplot(MF[,1:4],method="color",outline=T,title="Matrice factor",sig.level = 0.05,shade.col = "white",addCoef.col=T,  tl.col = "BLACK", number.font = 6)

acp$var$cor

# 7 - reprezentare cu 2 dimensiuni
#windows()
biplot(acp)

#tema
#install.packages("factoextra")
#install.packages("FactoMineR")
library(factoextra)
library(FactoMineR)

acp2 <- PCA(X,scale.unit=T,ncp=11)
scoruri <- acp2$ind$coord[,1:2]
## acp3 <- PCA(X,scale.unit=T,ncp=11)
summary(acp2) 
fviz_eig(acp2)
fviz_pca_ind(acp2)
fviz_pca_var(acp2,col.var="contrib", col.circle="goldenrod3", gradient.cols="goldenrod")

ggplot(X, aes(x=X1, y=X2,col=X4)) +
  geom_point(shape = 16,aes(size =X3),alpha = .4) +
  geom_text(label=row.names(X),vjust = 0, hjust=0, size=5)+
  labs(x = "X1", y = "X2")+
  theme_minimal() +
  scale_color_gradient(low = "#0098af", high = "#a0690e")+
  theme(axis.text.y=element_text(size=14),
        axis.title=element_text(size=14,face="bold"),
        legend.position="bottom")

acp2$ind$coord
cos2 <- acp2$var$cos2
sum(cos2[,1])

# seminar 6 ---------------------------------------------------------------


        ##q1 <- quantile(X$X1,probs=c(0.33,0.66,0.99))
        ##q2 <- quantile(X$X2,probs=c(0.33,0.66,0.99))
        ##solutie[X$X1 <= q1[1]] <-  "V1"
        ##solutie[X$X1 > q1[1] & X$X1 <=q1[2]] <-  "V2"
        ##solutie[X$X1 > q1[2] & X$X1 <=q1[3]] <-  "V3"
        ##solutie[X$X1 > q1[3]] <-  "V4"
        ##table(solutie)
        
        ##dimensiune[X$X2 <= q2[1]] <-  "mici"
        ##dimensiune[X$X2 > q2[1] & X$X2 <= q2[2]] <-  "mijlocii"
        ##dimensiune[X$X2 > q2[2]] <-  "mari"
        ##table(dimensiune)

solutie <- rep(0,236)
dimensiune <- rep(0,236)
q1 <- quantile(X$X1,probs=c(0.33,0.66))
q1
q1[1]
q1[2]
solutie[X$X1<=q1[1]]<-"mic_X1"
solutie[X$X1>q1[1] & X$X1<=q1[2]]<-"mediu_X1"
solutie[X$X1>q1[2]]<-"mare_X1"
table(solutie)
X$solutie<-factor(solutie)


q2<-quantile(X$X2,probs=c(0.33,0.66))
q2
q2[1]
q2[2]
dimensiune[X$X2<=q2[1]]<-"mic_X2"
dimensiune[X$X2>q2[1] & X$X2<=q2[2]]<-"mediu_X2"
dimensiune[X$X2>q2[2]]<-"mare_X2"
table(dimensiune)
X$dimensiune<-factor(dimensiune)


## Tabelul de contingenta
contingenta <- table(solutie,dimensiune)
contingenta
chisq.test(contingenta)

## 3. Identificati contributia unei categorii la inertia totala
## 4. Interpretare output grafic
#install.packages("ca")
library(ca)
ac <- ca(contingenta)
fviz_ca(ac)
plot(ac)
summary(ac)
## dpdv al calitatii, toate variabilele explica corect setul de date (qlt = 1000)
## dpdv al inertiei, cea mai mare parte din aceasta este preluata de categoria "mic" pentru variabila X1, dar si pentru variabila X2
## categoria mare (variabilele care au o valoare mai mare de 3.4) pentru variabila X1 este corelata negativ cu prima dimnesiune si pozitiv cu a doua dimensiune
## categoria mare (variabilele care au o valoare mai mare de 66.67) pentru variabila X2 este corelata pozitiv cu ambele dimensiuni
## aceste doua categorii scot in evidenta diferentele dintre variabilele X1 si X2


# seminar 8 ---------------------------------------------------------------

table.rowsum <- apply(contingenta,1,sum)
table.rowsum 
table.colsum <-  apply(contingenta,2,sum)
table.colsum
table.sum <- sum(contingenta)
table.sum

X <- X[,-c(11,12)]

## coeficientul de corelatie partiala 
# x <- x1, y <- x2, z <- x3
## Metoda 1
xz <- lm(X$X1~X$X3)
rxz <- resid(xz)
yz<-lm(X$X2~X$X3)
ryz <- resid(yz)

## Metoda 2
cor(X$X1,X$X2)
cpxy <- cor(rxz,ryz)

## Metoda 3 (inversa matricii de corelatie)
R <- cor(X)
P <- solve(R)

# seminar 9 ---------------------------------------------------------------

#install.packages("psych")
library(psych)
KMO(r=cor(X))
cortest.bartlett(cor(X))
xfinal <- xf[,-4]
## rm(xfinal)
KMO(xfinal)
cortest.bartlett(xfinal)

## se schimba dimensiunile - se trece dintr-un spatiu in altul

##Punctul 2 - factanal()
##foloseste MVM (metoda verosimilitatii maxime)
af1 <- factanal(xfinal,factors=2,scoares="regression",rotation="none")
af1
summary(af1)

##Punctul 3
af1$uniquenesses # unicitati
af1$scores # scoruri
af1$loadings # intensitatile

#Punctul 5
### biblioteca psych
#?fa()

# metoda ml = MAXIMUM LIKELIHOOD (verosimilitate maxima)
##windows()
afML <- fa(cor(xfinal),nfactors=2,n.obs=236, rotate="none",fm="ml")
fa.diagram(afML)#,cut=0.4)
summary(afML)

# metoda pa = PRINCIPAL AXIS FACTORING (componente principale) 
# se bazeaza pe o descompunere iterativa a matricii de corelatie
# la fiecare pas se inlocuieste diagonala cu valorile estimate de factorii de la iteratia anterioara
afPA <- fa(cor(xfinal),nfactors=3,n.obs=236, rotate="none",fm="pa")
fa.diagram(afPA)
summary(afPA)

## trebuie sa vedem care dintre metode este mai buna
## comparam prin scores (???)

summary(afML)
afML
summary(afPA)
afPA

LoadPA <- afPA$loadings
LoadPA
l1 <- LoadPA[,1]
l1
sum(l1^2) # egal cu SS loadings 

LoadML <- afML$loadings
LoadML
l2 <- LoadML[,1]
l2
sum(l2^2) 

# Scoruri pentru fiecare dintre metoda (ML si PA)
fsPA <- factor.scores(xfinal, afPA)  
fsPA
scoruriPA <- fsPA$scores
scoruriPA

fsML <- factor.scores(xfinal, afML)  
fsML
scoruriML <- fsML$scores
scoruriML

## Rotire varimax
af <- fa(cor(xfinal),nfactors=2,n.obs=236, rotate="varimax",fm="ml")
fa.diagram(af)
summary(af)

#af <- fa(cor(xfinal),nfactors=3,n.obs=236, rotate="varimax",fm="pa")
#fa.diagram(af)

#install.packages("nFactors")
library(nFactors)
library(psych)
# Numarul de factori
## ?fa.parallel()
## windows()
paralel <- fa.parallel(xfinal)
?parallel
## se genereaza date cu aceleasi proprietati ca datele reale, pentru a obtine centila 95% pentru fiecare valoare proprie
## valorile proprii obtinute pe datele reale se compara cu acest prag 
## se vor pastra cele ce depasesc acest prag

pca_plot <- fviz_pca_ind(acp2, col.ind = "cos2", gradient.cols = c("#C6E284", "#D8D2FA"),
                         geom = c("point", "text"), col.var = "contrib",
                         title = "Nor de Puncte")  + labs(x = "ML1", y = "ML2") + 
  theme(axis.text = element_blank(), axis.title = element_blank(), axis.line = element_blank(), axis.ticks = element_blank())
pca_plot

paralel$nfact 


# pe test (?) ------------------------------------------------------------

##pt indicatori - partea de analzia descriptiva, partea de histograme, boxplot
##ACP
##Analiza corespondentelor
##Analiza factoriala (testul Bartlett, KMO - interpretari + toate grafice)
##alegere varianta (MVM, minimizare reziduuri, interpretabilitate date)


# seminar 11 --------------------------------------------------------------

## Analiza cluster
## n = numarul obiectelor analizate
## p = numarul caracteristicilor


# seminar 13 --------------------------------------------------------------

## analiza cluster
library(factoextra)
library(FactoMineR)
#install.packages("cluster")
library(cluster)
acp2 <- PCA(X,scale.unit=T,ncp=11)
scoruri <- acp2$ind$coord[,1:2]
distante <-  dist(scoruri)
ierarhie <- hclust(distante ,method="centroid")
solutie <- cutree(ierarhie,k = 2)
s <- silhouette(solutie,distante)
fviz_cluster(list(data = scoruri, cluster = solutie))

##Matricea distantelor
d <- dist(scoruri)
ierarhie <- hclust(d,method="ward.D2")
ierarhie
?hclust()
plot(ierarhie)
ierarhie$height #distantele de agregare
ierarhie$labels #etichetele obiectelor
plot(ierarhie)
rect.hclust(ierarhie, k = 4, border = 2:5)
solutie5 <- cutree(ierarhie, k = 5) # apartenenta obiectelor
table(solutie5)
aggregate(scoruri,list(solutie5),mean)

# functie de extragere solutie
sol <- hcut(scoruri, k = 5, hc_metod = "centroid")
sol

# reprezentare solutie
fviz_dend(sol, show_labels = TRUE, rect = F)
fviz_cluster(list(data = scoruri, cluster = solutie5))
fviz_cluster(sol)

# reprezentare grafi silhouette
s <- silhouette(solutie5, d)
plot(s,main="Grafic Silhoette metoda Centroidului")
# valorile cat mai mari, apropiate de 1 -> cu atat tarile sunt clasate in gruparile respective cat mai bine

km <- kmeans(scoruri, 5)
clase <- km$cluster #apartenenta la clase
table(clase)
fviz_cluster(km, scoruri)
s <- silhouette(clase,d)
plot(s, main="Grafic Silhoette metoda Kmeans")

km$centers #centroizii
km$totss # variabilitate toatala
km$withinss # variabilitate intre clase
km$tot.withinss # variabilitate intra clasa totala
km$betweenss # variabilitate inter clasa

