# setup, functions --------------------------------------------------------

rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/~stahel/courses/multivariate/datasets/"
d.vegen <- read.table(paste(kurs.url, "vegenv.dat", sep=""), header=T)


species <- d.vegen[,c(19:82)]
species <- species[, -c(15,43)]
columns <- which(apply(species, 2, mean)>1)
speciestrsf <- sqrt(species[,columns])


# FA ----------------------------------------------------------------------


fa.res1 <- factanal(speciestrsf, factors=3, rotation="varimax")
fa.res2 <- factanal(covmat = cor(speciestrsf), factors=3, rotation="varimax", n.obs=nrow(speciestrsf))

fa.res1$loadings
fa.res2$loadings


par(mfrow=c(1,2))
plot(fa.res1$loadings)
text(fa.res1$loadings, names(speciestrsf), cex=0.6)

plot(fa.res2$loadings)
text(fa.res2$loadings, names(speciestrsf), cex=0.6)


# rotation vs. unrotated --------------------------------------------------

fa.resnone <- factanal(speciestrsf, factors=3, rotation="none")
fa.resvari <- factanal(speciestrsf, factors=3, rotation="varimax")

fa.resnone$loadings
fa.resvari$loadings


# plot rotated vs. unrotated ----------------------------------------------


load(url("http://stat.ethz.ch/Teaching/Datasets/WBL/cor.4.6.rda"))
data <- cor.4.6

rotnone <- factanal(covmat=data, factors=2, rotation="none", n.obs=220)
rotvari <- factanal(covmat=data, factors=2, rotation="varimax", n.obs=220)

rotnone$loadings
rotvari$loadings
par(mfrow=c(1,2))
dev.off()

?varimax

plot(rotnone$loadings, pch="", xlim=c(0,1), ylim=c(-0.7, 0.7), pty="s", asp=1)
text(rotnone$loadings, c("French", "English", "History", "Aithmetic", "Algebra", "Geometry"), cex=0.8)
abline(v=0, h=0)

rotnone$loadings%*%loadvar$rotmat
loadvar$loadings

loadvar <- varimax(rotnone$loadings, normalize=F)
abline(0, loadvar$rotmat[2,1]/loadvar$rotmat[1,1], col="blue", lwd=2)   # x-axes
abline(0, loadvar$rotmat[2,2]/loadvar$rotmat[1,2], col="green", lwd=2)  # y-axes

plot(rotvari$loadings, pch="", xlim=c(0,1), ylim=c(-0.7, 0.7), pty="s", asp=1)
text(rotvari$loadings, c("French", "English", "History", "Aithmetic", "Algebra", "Geometry"), cex=1.2)
abline(v=0, h=0)



# SS loadings etc. --------------------------------------------------------

fa.res1


# Factor1 Factor2 Factor3
# SS loadings      2.887   2.135   1.150
# Proportion Var   0.222   0.164   0.088
# Cumulative Var   0.222   0.386   0.475


# ss loadings: sum of squared loadings
sum(fa.res1$loadings[,1]^2)

#Proportion Var: ss loading / total varianz, CHECK diag cor... sum of ones... standardized variances...?

sum(fa.res1$loadings[,1]^2)/sum(diag(cor(speciestrsf)))

sum(fa.res1$loadings[,2]^2)/sum(diag(cor(speciestrsf)))

sum(fa.res1$loadings[,3]^2)/sum(diag(cor(speciestrsf)))




