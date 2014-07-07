# setup, functions --------------------------------------------------------

rm(list=ls())
dev.off()

# separate functions for pairs plot ---------------------------------------

## put histograms on the diagonal
panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}

## put (absolute) correlations on the upper panels,
## with size proportional to the correlations.
panel.cor <- function(x, y, digits = 2, prefix = "", cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Serie1, Aufgabe 1 -------------------------------------------------------

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

d.bank <- read.table(paste(kurs.url, "banknot.dat", sep=""), header=T)

summary(d.bank)


# pairs with different col ------------------------------------------------

pairs(d.bank[-d.bank$CODE], pch=1, col=d.bank$CODE+1)

pairs(d.bank[-d.bank$CODE], lower.panel = panel.hist, upper.panel = panel.cor)
pairs(d.bank[-d.bank$CODE], lower.panel = panel.hist, upper.panel = panel.smooth)

# Serie1, Aufgabe 2 -------------------------------------------------------

d.swiss <- swiss

str(d.swiss)
summary(d.swiss)


# pairs, col=ifelse -------------------------------------------------------

pairs(swiss, panel = panel.smooth, main = "swiss data", col = ifelse(swiss$Catholic > 50, 1, 2))

dev.off()



# star plot in scatterplot ------------------------------------------------

stars(d.swiss, draw.segments=T, key.loc=c(0,17.5))

stars(as.matrix(swiss[,c(2,3,5,6)]), location=as.matrix(swiss[,c(4,1)]), axes=T, draw.segments=T, key.loc=c(50,90), labels=NULL, len=2.5, ylab="Fertility", xlab="Education")
abline(lm(swiss$Fertility~swiss$Education))
lines(lowess(swiss$Fertility~swiss$Education), col="red", lwd=5)


# Serie1, Aufgabe 3 -------------------------------------------------------

rm(list=ls())
dev.off()

kurs.url <- "http://stat.ethz.ch/Teaching/Datasets/WBL/"

d.quak <- read.csv(paste(kurs.url, "quakes.csv", sep=""), header=T, sep=",")

str(d.quak)

# jittering ---------------------------------------------------------------
par(mfrow=c(2,2))

plot(d.quak$mag~d.quak$depth)
plot(jitter(d.quak$mag)~d.quak$depth, pch=1, col="red")
plot(d.quak$stations~jitter(d.quak$mag), pch=16, col="red")

scatter.smooth(d.quak$mag~d.quak$depth)
scatter.smooth(d.quak$stations~jitter(d.quak$mag), pch=16, col="red")
scatter.smooth(log(d.quak$stations)~jitter(d.quak$mag), pch=16, col="red")

dev.off()
pairs(d.quak[,-1], lower.panel = panel.smooth, col=round(d.quak$mag, 0)-2)

# parallel coordinate plot ------------------------------------------------

library(MASS)

?parcoord
par(mfrow=c(1,2))
parcoord(d.quak[,-1], col=as.numeric(d.quak$stations))
parcoord(d.quak[,-c(1,6)], col=as.numeric(round(d.quak$mag,0)))

# costumizing colors in plot according to cut criteria --------------------

?cut
deepVec <- cut(d.quak$depth, breaks=c(0,250,450,700), labels=c("green", "black", "red"))
deepVecString <- as.character(deepVec)

plot(d.quak$lat, d.quak$long, col=deepVecString)

# coplot ------------------------------------------------------------------

?coplot
coplot(d.quak$lat~d.quak$long|d.quak$depth)

coplot(d.quak$lat~d.quak$long|d.quak$mag)

coplot(d.quak$lat~d.quak$long|d.quak$depth*d.quak$mag)

coplot(d.quak$depth~d.quak$mag|d.quak$lat*d.quak$long, panel=function(x,y,col,pch){panel.smooth(x,y,span=0.9)})

?coplot
?panel.smooth

coplot(d.quak$lat~d.quak$long|d.quak$depth*d.quak$mag)

# Serie1, Aufgabe 4 -------------------------------------------------------

rm(list=ls())
dev.off()

?mtcars

cars <- mtcars
str(cars)
?stars

stars(cars[c("mpg", "disp", "hp", "wt", "qsec")], draw.segments=T, key.loc=c(-1,2), flip.labels=F, cex=0.6)

row.names(cars)
stars(cars[c("Cadillac Fleetwood","Lincoln Continental", "Merc 450SE", "Merc 450SLC", "Merc 450SL"),c("mpg", "disp", "hp", "wt", "qsec")], draw.segments=T, key.loc=c(0,2), flip.labels=F, cex=0.6)

stars(cars[c("hp", "wt", "qsec")], location=as.matrix(cars[,c(1,3)]), axes=T, draw.segments=T, labels=NULL, len=45, key.loc=c(-1,0))

stars(as.matrix(swiss[,c(2,3,5,6)]), location=as.matrix(swiss[,c(4,1)]), axes=T, draw.segments=T, key.loc=c(50,90), labels=NULL, len=2.5, ylab="Fertility", xlab="Education")


# pairs, panel=function ... span ------------------------------------------

cor(cars[c("mpg", "disp", "hp", "wt", "qsec")])
pairs(cars[c("mpg", "disp", "hp", "wt", "qsec")], panel=function(x,y,col,pch){panel.smooth(x,y,span=0.3)})

plot(cars$mpg, cars$hp, data=cars)
# save pdf ----------------------------------------------------------------

getwd()
setwd("C:\\Users\\Tobi\\Documents\\a1_ETH_WBL\\semester3\\Multivariate Statistik")

pdf(file = "pairs.pdf")
pairs(cars[c("mpg", "disp", "hp", "wt", "qsec")], panel=panel.smooth)
dev.off()


# Serie2, Aufgabe 1 -------------------------------------------------------

# theoretisch


# setup, functions --------------------------------------------------------
rm(list=ls())
dev.off()


# Serie2, Aufgabe 2 -------------------------------------------------------

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

dfTitanic <- as.data.frame(Titanic)

str(dfTitanic)
plot(dfTitanic)



# library(vcd) mosaic, structable, cotabplot ------------------------------

library(vcd)
?mosaic
?structable

# connection between survival and class

# mosaic, observed vs. expected

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))

pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
mosaic(structable(Freq~Class + Survived, dfTitanic), shade=T, newpage=F, main="observed")
popViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
mosaic(structable(chisq.test(structable(Freq~Class + Survived, dfTitanic))$expected), shade=T, newpage=F, main="expected")
popViewport()

grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))

pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
mosaic(structable(chisq.test(structable(Freq~Sex+Class, dfTitanic))$expected), shade=T, newpage=F)
popViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
mosaic(structable(Freq~Sex+Class, dfTitanic), shade=T, newpage=F)
popViewport()

mosaic(~Age+Survived, dfTitanic, shade=T)

dev.off()

# connection between survival and class?
mosaic(structable(Freq~Survived+Class, dfTitanic), shade=T)


# survival rates between men and women
mosaic(structable(Freq~Survived+Sex, dfTitanic), shade=T)
mosaic(structable(chisq.test(structable(Freq~Survived+Sex, dfTitanic))$expected), shade=T)


grid.newpage()
pushViewport(viewport(layout = grid.layout(1,2)))

pushViewport(viewport(layout.pos.col=1, layout.pos.row=1))
mosaic(structable(Freq~Sex+Survived, dfTitanic), shade=T, newpage=F, main="observed")
popViewport()

pushViewport(viewport(layout.pos.col=2, layout.pos.row=1))
mosaic(structable(chisq.test(structable(Freq~Sex+Survived, dfTitanic))$expected), shade=T, newpage=F, main="expected")
popViewport()


# children
mosaic(structable(Freq~Survived+Age, dfTitanic), shade=T)

# womens first policy in all classes

?cotabplot

cotabplot(structable(Freq~Survived+Sex+Class, dfTitanic), shade=T)
cotabplot(Freq~Survived+Sex+Class, dfTitanic, shade=T)



# Serie2, Aufgabe 2 -------------------------------------------------------

rm(list=ls())
dev.off()

library(vcd)

?OvaryCancer

d.o <- OvaryCancer
str(d.o)

mosaic(structable(Freq~stage+survival, data=d.o), shade=T)

mosaic(structable(Freq~operation+survival, data=d.o), shade=T)

mosaic(structable(Freq~xray+survival, data=d.o), shade=T)

cotabplot(Freq~survival+xray+operation|stage, d.o, shade=T)

# Serie2, Aufgabe 4 -------------------------------------------------------

rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

d.typo <- read.csv(paste(kurs.url, "typo.csv", sep=""), header=T, sep=",", row.names=1)
str(d.typo)
plot(d.typo)
pairs(d.typo)


hull <- chull(d.typo[,2], d.typo[,3])
plot(d.typo[,2], d.typo[,3])
identify(d.typo[,2], d.typo[,3])


polygon(d.typo[,2][hull],d.typo[,3][hull],density=15,angle=10)

summary(d.typo)

n <- 1:length(d.typo)
par(mfrow=c(3,4))
for(i in n){
  hist(d.typo[,i], breaks=100)
}

for(i in n){
  boxplot(d.typo[,i], breaks=100)
}

dev.off()

which.min(d.typo[,2])
which.min(d.typo[,4])
which.min(d.typo[,9])


for (i in 1:ncol(d.typo)){
  print(which.min(d.typo[,i]))
}



# identify outliers with chisq.plot::mvoutlier ----------------------------

??namespace

library(mvoutlier)

#install.packages("mvoutlier", dep=T)

chisq.plot(d.typo)
?chisq.plot


# Serie 2, Aufgabe 5 ------------------------------------------------------
rm(list=ls())
dev.off()
kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

water <- read.csv(paste(kurs.url, "waterInDesert.csv", sep=""), header=T, sep=",", row.names=1)
str(water)

boxplot(water$V3)

library(mvoutlier)


chisq.plot(water)



# Serie1, Aufgabe 1 -------------------------------------------------------

rm(list=ls())
dev.off()

usa <- USArrests

str(usa)
pairs(usa)


data <- USArrests[,-3]

str(data)
row.names(data)

stars(data, key.loc = c(-1,1), draw.segments=T, flip.labels=F, cex=0.6)

dev.off()

summary(data)
boxplot(data[,-2])
boxplot(data)


# principal component analysis --------------------------------------------

# PC1 Principal component no. 1: direction of largest variance

?prcomp
?princomp

pc.prn <- princomp(data, cor=F)
summary(pc.prn, loadings=T)
plot(pc.prn)

# pc.pr <- prcomp(data, scale=T)      # see difference scale=T/F in screeplot
# summary(pc.pr, loadings=T)
# plot(pc.pr)


biplot(pc.prn, cex=0.6, xlim=c(-0.5, 0.5), expand=0.8)


# Aufgabe e.) we're still using cor=F, compute var(PC1) -------------------

str(pc.prn)
pc.prn$call

var(pc.prn$scores[,1])

score <- (1/sqrt(3))*(data[, "Murder"]+data[, "Assault"]+data[, "Rape"])
length(score)
nrow(usa)

var(score)



# cor=T -------------------------------------------------------------------
# cor=T correlations matrix, cor=F covariance matrix

pc.prncor <- princomp(data, cor=T)

pc.prncor$call
pc.prn$call

pc <- pc.prncor
summary(pc, loadings=T)


plot(pc)
biplot(pc)
str(pc)

pc$sdev

head(data)

str(pc$loadings)
pc$loadings[1]*data["Murder"]+pc$loadings[2]*data["Assault"]+pc$loadings[3]*data["Rape"]

pc$loadings[1]*data[1,1]+pc$loadings[2]*data[1,2]+pc$loadings[3]*data[1,3]

pc$scores



# Aufgabe 1.h: are PC1 and PC2 uncorrelated? ------------------------------
cor(pc$scores[,1], pc$scores[,2])

plot(pc$scores[,2]~pc$scores[,1], pch="", ylab="PC", xlab="PC1")
text(pc$scores[,1], pc$scores[,2], abbreviate(row.names(data), 5))



# Serie 3, Aufgabe 2 ------------------------------------------------------

rm(list=ls())
dev.off()
data <- read.table("http://stat.ethz.ch/~maathuis/teaching/fall13/protein.txt", header=T)
row.names(data) <- data$Country
data <- data[,-1]

plot(data)
stars(data, draw.segments=T, key.loc=c(0,12), flip.labels=F, cex=0.6)

pc <- prcomp(data, scale=T)
str(pc)
summary(pc)
plot(pc)

pc <- princomp(data, cor=T)
plot(pc)
summary(pc, loadings=T)

par(mfrow=c(1,2))
biplot(pc)
biplot(pc, scale=0, cex=0.6)


str(pc)


# interpretation of PC1 & PC2 ---------------------------------------------
dev.off()
par(mfrow=c(1,2))
biplot(pc, scale=0, cex=0.6)

plot(pc$scores[,2]~pc$scores[,1], pch="", ylab="PC", xlab="PC1")
text(pc$scores[,1], pc$scores[,2], row.names(data), cex=0.7)

# für Länder aus dem Balkan scheinen cerealien wichtig zu sein. Für Zentraleuropäische Länder hingegen tierische Produkte

str(pc$loadings)
pc$loadings
sort(pc$loadings[,1])



# Serie 3, Aufgabe 3 ------------------------------------------------------

rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

zehn <- read.table(paste(kurs.url, "zehnkampf.dat", sep=""), header=T)

head(zehn)


# using covariance vs. correlationmatrix ----------------------------------
dev.off()
par(mfrow=c(1,2))
biplot(princomp(zehn, cor=F), cex=0.6)
title("cor=F")
biplot(princomp(zehn, cor=T), cex=0.6)
title("cor=T")

dev.off()


# since with cor=F variables are not centered, some variables exhibits such a high variance, taht they tend to suppresse all the other variables


# who is an average athlete

PCAdat <- princomp(zehn, cor=T)
biplot(PCAdat, cex=0.6)
identify(PCAdat$scores[,1], PCAdat$scores[,2])
zehn[c(14,22),]



# setup, functions --------------------------------------------------------

rm(list=ls())
dev.off()

# Serie4, Aufgabe 1 -------------------------------------------------------

# theoretisch, optional


# Serie4, Aufgabe 2 -------------------------------------------------------

# kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"
# zehn <- read.table(paste(kurs.url, "zehnkampf.dat", sep=""), header=T)

load(url("http://stat.ethz.ch/Teaching/Datasets/WBL/cor.4.6.rda"))

data <- cor.4.6
data

?factanal


# factanal (x, ...) unsing covariance/correlation matrix as input, --------

res1 <- factanal(covmat=data, factors=2, rotation="none", n.obs=220)
res1
str(res1)
res1$PVAL


par(mfrow=c(1,2))
plot(res1$loadings, xlim=c(0,1), ylim=c(-0.5, 0.5), pty="s", asp=1)


plot(res1$loadings, pch="", xlim=c(0,1), ylim=c(-0.5, 0.5), pty="s", asp=1)
text(res1$loadings, c("Fr", "En", "Hi", "Ar", "Al", "Ge"))
abline(v=0, h=0)

dev.off()

res2 <- varimax(res1$loadings, normalize=F)
abline(0, res2$rotmat[2,1]/res2$rotmat[1,1], col="blue", lwd=2)
abline(0, res2$rotmat[2,2]/res2$rotmat[1,2], col="blue", lwd=2)


rot <- round(cbind(res1$loadings %*% res2$rotmat, res2$loadings), 3)
colnames(rot) <- c("f1.rot", "f2.rot", "f1.vm", "f2.vm")
rot



# Serie4, Aufgabe 3 -------------------------------------------------------

rm(list=ls())
dev.off()


load(url("http://stat.ethz.ch/Teaching/Datasets/WBL/cor.4.7.rda"))
data <- cor.4.7

data


# apply PC just by eigenvalue decomposition -------------------------------


?eigen
res <- eigen(data)
res
plot(res$values,type="b", lwd=2)

res$vectors
res$vectors[,1:2]


# apply ML FC -------------------------------------------------------------

res.fa1 <- factanal(covmat=data, factors=1, rotation="none", n.obs=123)
res.fa2 <- factanal(covmat=data, factors=2, rotation="none", n.obs=123)
res.fa3 <- factanal(covmat=data, factors=3, rotation="none", n.obs=123)

round(c(res.fa1$PVAL, res.fa2$PVAL, res.fa3$PVAL), 5)

# the last computed p-value is larger than 0.05 -> 2 factors are enough

round(res.fa3$loadings[1:9,], digits=3)


# orthogonal rotation -----------------------------------------------------
dev.off()

round(factanal(covmat=data, factors=3, n.obs=123, rotation="varimax")$loadings[1:9,], digits=3)

plot(round(factanal(covmat=data, factors=3, n.obs=123, rotation="varimax")$loadings[1:9,], digits=3))
abline(v=0, h=0)


# oblique rotation --------------------------------------------------------

round(factanal(covmat=data, factors=3, n.obs=123, rotation="promax")$loadings[1:9,], digits=3)


# Serie4, Aufgabe 4 -------------------------------------------------------

rm(list=ls())
dev.off()


data <- ability.cov
data

cov.d <- data$cov
cov.d
cor.d <- cov2cor(cov.d)
cor.d

boxplot(cor.d, outline=T)
stars(cor.d, key.loc=c(-1,6), draw.segments=T, flip.labels=F, cex=0.6)

# check for large correlations
cor.d
diag.1 <- diag(rep(1,6))
cor.min <- cor.d - diag.1
cor.min

# compute max_{i!=j} |r_{ij}| for each column
h2 <- apply(abs(cor.min), 2, max)
h2


res.fa1 <- factanal(covmat=cov.d, factors=1, rotation="varimax", n.obs=112)
res.fa2 <- factanal(covmat=cov.d, factors=2, rotation="varimax", n.obs=112)
res.fa3 <- factanal(covmat=cov.d, factors=3, rotation="varimax", n.obs=112)

res.fa2$loadings


###### doesn't seem to make sense...
round(c(res.fa1$PVAL, res.fa2$PVAL, res.fa3$PVAL), 5)
# the last computet p-value is larger than 0.05 -> 1 factors are enough
cov.d
plot(res.none$loadings)


#rotation "none" vs. "varimax"
par(mfrow=c(1,2))

res.none <- factanal(covmat=cov.d, factors=2, rotation="none", n.obs=112)
res.vari <- factanal(covmat=cov.d, factors=2, rotation="varimax", n.obs=112)

plot(res.none$loadings, pch="", xlim=c(0,1), ylim=c(-0.5, 1), pty="s", asp=1, main="none")
text(res.none$loadings, rownames(cov.d), cex=0.6)
abline(h=0,v=0)

plot(res.vari$loadings, pch="", xlim=c(0,1), ylim=c(-0.5, 1), pty="s", asp=1, main="varimax")
text(res.vari$loadings, rownames(cov.d), cex=0.6)
abline(h=0,v=0)




# setup, functions --------------------------------------------------------

rm(list=ls())
dev.off()


# Serie5, Aufgabe 1 -------------------------------------------------------

kurs.url = "http://stat.ethz.ch/~stahel/courses/multivariate/datasets/"

# Soil:
# pH5, P5, N5, C5       soil variables for depth 5
# pH10, P10, N10, C10   soil variables for depth 10
# pH15, P15, N15, C15 	soil variables for depth 15
# Baresoil 
# 
# Physical environment
# Slope 
# 
# Use
# Grazing
# Dungdensity 
# 
# Vegetation
# VegetationGroup 
# 
# Plant species abundance
# Rubuidae
# ...
# Soldalpi 


d.vegen <- read.table(paste(kurs.url, "vegenv.dat", sep=""), header=T)

str(d.vegen)

names(d.vegen)

vars <- c("pH10", "P10", "N10", "C10")

pairs(d.vegen[, vars])

summary(d.vegen$Dungdensity)
dungdensVec <- cut(d.vegen$Dungdensity, breaks=c(0,10, max(d.vegen$Dungdensity, na.rm=T)), include.lowest=T, labels=c("blue", "black"))
dungdensVecString <- as.character(dungdensVec)

# coloring pairs plot, ifelse colVec --------------------------------------

par(mfrow=c(1,2))
pairs(d.vegen[, vars], col=dungdensVecString, pch=16)
pairs(d.vegen[, vars], col=ifelse(d.vegen$Dungdensity>10, 1, "orange"), pch=16, panel=panel.smooth)


?coplot
dev.off()
hist(d.vegen$Dungdensity, breaks=100)
coplot(d.vegen$P10~d.vegen$pH10 | d.vegen$Dungdensity+d.vegen$Slope, panel=panel.smooth, col=ifelse(d.vegen$Dungdensity>=2, "black", "orange"), pch=16, data=na.omit(d.vegen))


dev.off()
d.vegenNA <- na.omit(d.vegen)

vars2 <- c("Nardstri", "Caluvulg", "Festrubr")

stars(d.vegenNA[, vars2],
      axes=T,
      location=cbind(log10(d.vegenNA$P10),d.vegenNA$N10),
      draw.segments=T,
      len=0.07)

stars(d.vegen[, vars2],
      axes=T,
      location=cbind(log10(d.vegen$P10),d.vegen$N10),
      draw.segments=T,
      len=0.07,
      cex=0.6,
      key.loc=c(-1.6, 1.2))
abline(lm(d.vegen$N10~log10(d.vegen$P10)))


names(d.vegen)
species <- d.vegen[,c(19:82)]

summary(species)

which(apply(species, 2, max)==0)


species <- species[, -c(15,43)]
species
columns <- which(apply(species, 2, mean)>1)
columns


speciestrsf <- sqrt(species[,columns])


# prcomp(..., scale=T) - princomp(..., cor=T) -----------------------------

pca.res <- prcomp(speciestrsf, scale=T)
pca.res <- princomp(speciestrsf, cor=T)

str(pca.res)
summary(pca.res)
biplot(pca.res, cex=0.5)
?biplot

pairs(pca.res$x[,1:3], pch=d.vegen$VegetationGroup+15, col=d.vegen$VegetationGroup)

summary(pca.res)
plot(pca.res, type="l")
plot(pca.res)

biplot(pca.res, cex=0.6, scale=0)


fa.res <- factanal(speciestrsf, factor=3)
fa.res


# plot factor loadings for all pairs of factors ---------------------------

par(mfrow=c(2,2))
plot(fa.res$loadings[,c(1:2)], pch="", asp=1)
text(fa.res$loadings, colnames(speciestrsf), cex=0.7)
abline(v=0) # draw vertical line at x=0
abline(h=0) # draw horizontal line at y=0

plot(fa.res$loadings[,c(1,3)], pch="", asp=1)
text(fa.res$loadings[,c(1,3)], colnames(speciestrsf), cex=0.7)
abline(v=0) # draw vertical line at x=0
abline(h=0) # draw horizontal line at y=0

plot(fa.res$loadings[,2:3], pch="", asp=1)
text(fa.res$loadings[,2:3], colnames(speciestrsf), cex=0.7)
abline(v=0) # draw vertical line at x=0
abline(h=0) # draw horizontal line at y=0
dev.off()


# varying the numbers of factors, meaning of p-values ---------------------

factanal(speciestrsf, factors=3)$PVAL

# objective 
# 0.00997664 

factanal(speciestrsf, factors=4)$PVAL

# objective 
# 0.1253809 

# die Nullhypothese, dass factors=k faktoren ausreichen, kann ab 4 Faktoren nicht mehr verworfen werden.



# setup -------------------------------------------------------------------

rm(list=ls())
dev.off()


# Serie6, Aufgabe 1 -------------------------------------------------------

t.url <- "http://stat.ethz.ch/Teaching/Datasets/WBL/ch-dist.dat"

chdist <- read.table(t.url, header=T)

str(chdist)
dim(chdist) # distance matrix


# deriving "coordinates" using metric cdmscale(x, ...) --------------------

?cmdscale

koord <- cmdscale(chdist, k=2)
koord
str(koord)


plot(-koord, col="red", asp=1, xlab="coord 1", ylab="coord2")
text(-koord, labels=rownames(koord), pos=1)


?dist
t.dist1 <- c(as.matrix(chdist))
t.dist2 <- c(as.matrix(dist(koord)))

par(pty="s")
plot(t.dist1~t.dist2)
abline(0,1, lty=3)



# deriving "coordinates" using non-metric isoMDS(x,...) in library --------

library(MASS)
?isoMDS

m.chdist <- as.matrix(chdist)
koord2 <- isoMDS(m.chdist, k=2)

str(koord2)
koord2

plot(-koord2$points, xlab="coord1", ylab="coord2")
text(-koord2$points, labels=rownames(koord2$points), pos=3, cex=0.6)


plot(c(as.matrix(chdist))~c(as.matrix(dist(koord2$points))))
abline(0,1)


# comparing the two methodes ----------------------------------------------

par(mfrow=c(1,2))

plot(-koord, col="red", asp=1, xlab="coord 1", ylab="coord2", main="metric MDS")
text(-koord, labels=rownames(koord), pos=3, cex=0.6)

plot(-koord2$points, xlab="coord1", ylab="coord2", main="non-metric MDS")
text(-koord2$points, labels=rownames(koord2$points), pos=3, cex=0.6)



# comparing pca and mds ---------------------------------------------------

res.pca <- prcomp(chdist)
str(res.pca)

dev.off()
# PCA only does the same when wer're dealing with euclidean distan --------
plot(-res.pca$x[,1], res.pca$x[,2], xlab="-pc1", ylab="pc2")
text(-res.pca$x[,1], res.pca$x[,2], labels=rownames(chdist), pos=1, cex=0.5)


# Serie6, Aufgabe 2 -------------------------------------------------------

rm(list=ls())
dev.off()

load(url("http://stat.ethz.ch/Teaching/Datasets/WBL/countries.rda"))

str(mat)
dim(mat)
mat
nrow(mat)

dist.mat <- dist(scale(mat))


cntry.mds <- cmdscale(dist.mat, k=179, eig=T)
ev <- cntry.mds$eig
ev

head(cumsum(abs(ev))/sum(abs(ev)))

head(cumsum(ev^2)/sum(ev^2))

# 0.6468669 and 0.8701704 indicatinmg a decent fit, k=2 should be ok

pol.mds <- cmdscale(dist.mat, k=2, eig=T)
pol.mds

summary(pol.mds$points-cntry.mds$points[,1:2])

pol.mds

x <- pol.mds$points
mycol <- rep(1,180)
mycol[155] <- 2
plot(x[,1], x[,2], pch=20, col=mycol, xlab="coord1", ylab="coord2")

identify(x[,1], x[,2], labels=rownames(x))

plot(x[,1], x[,2], pch="", col=mycol, xlab="coord1", ylab="coord2")
text(x[,1], x[,2], labels=rownames(mat), col=mycol, cex=0.6)

pc.res <- prcomp(scale(mat))
plot(-pc.res$x[,1], -pc.res$x[,2])


# Serie6, Aufgabe 3 -------------------------------------------------------

rm(list=ls())
dev.off()

load(url("http://stat.ethz.ch/Teaching/Datasets/WBL/WWIIleaders.rda"))
WWIIleaders


library(MASS)
set.seed(123)
?isoMDS

WWIImds <- isoMDS(WWIIleaders)
WWIImds$stress


plot(WWIImds$points, xlab="coord1", ylab="coord2")
text(WWIImds$points, labels=attr(WWIIleaders, "Labels"), pos=1, cex=0.6)




# Serie6, Aufgabe 4 -------------------------------------------------------

rm(list=ls())
dev.off()

t.url <- "http://stat.ethz.ch/~stahel/courses/multivariate/datasets/vegenv.dat"
d.vegenv <- read.table(t.url, header=T)

str(d.vegenv)

t.d <- d.vegenv[,19:82]
t.d

t.col <- which(apply(t.d, 2, max)>0)
t.d <- sqrt(t.d[,t.col])

?dist

t.dist <- dist(t.d, method= "manhattan")

library(MASS)
?isoMDS
t.r <- isoMDS(t.dist)

str(t.r)
par(pty="s", mar=c(1,2))
plot(t.r$points, pch=d.vegenv$VegetationGroup)


t.s <- scale(t.d)
t.dist2 <- dist(t.s, method = "manhattan")

t.r2 <- isoMDS(t.dist2)

par(pty="s", mfrow=c(2,2))
dev.off()

plot(t.r$points, pch=d.vegenv$VegetationGroup)
plot(t.r2$points, pch=d.vegenv$VegetationGroup)


pc.res <- prcomp(t.s)
pc.res

biplot(pc.res)
plot(pc.res$x[,1], pc.res$x[,2], pch=d.vegenv$VegetationGroup)

