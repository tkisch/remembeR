# Principal component analysis --------------------------------------------

rm(list=ls())
dev.off()


# generate some data:
set.seed(414)
a1 <- rnorm(100)
a2 <- rnorm(100)
x1 <- a1 
x2 <- -sqrt(2/3)*a1 + sqrt(1/3)*a2
data <- data.frame(x1,x2)
data <- scale(data)

# manuelle Berechnung

cov(data)
cor(data)

val.eigen <- eigen(cov(data))$values
vec.eigen <- eigen(cov(data))$vectors

# Eigenvalues are variances of principal components -----------------------
pca.res <- prcomp(data, scale=T, center=T)

pca.res$sdev^2
val.eigen


# Eigenvectors are variable loadings / rotation ---------------------------

pca.res$rotation
vec.eigen


# variance of PCs = eigenvalues of cov-mat --------------------------------

cov(cbind(PC1,PC2))
val.eigen


# derive Principal components ---------------------------------------------

PC1 <- vec.eigen[1,1]*data[,1]+vec.eigen[2,1]*data[,2]
PC2 <- vec.eigen[1,2]*data[,1]+vec.eigen[2,2]*data[,2]

round(as.matrix(data)%*% pca.res$rotation[,1]-PC1,4)
round(as.matrix(data)%*% pca.res$rotation[,2]-PC2,4)


# prcomp ------------------------------------------------------------------

pca.res <- prcomp(data, scale=T, center=T)

# rotate by hand ----------------------------------------------------------

rot <- pca.res$rotation
rot

y1 <- rot[1,1]*data[,1] + rot[2,1]*data[,2]
y2 <- rot[1,2]*data[,1] + rot[2,2]*data[,2]

pca.res$x

vec.eigen
pca.res$rotation

pca.res$x



round(pca.res$x - cbind(y1,y2), 10)
round(pca.res$x - cbind(PC1,PC2), 10)


#  ------------------------------------------------------------------------
# plotting ----------------------------------------------------------------


par(mfrow=c(1,2))
rot <- pca.res$rotation
plot(data, pch="", pty="s", xlim=c(-5,5), ylim=c(-5,5), main="data")
text(data, labels=c(1:100), cex=0.6 )
abline(0, rot[2,1]/rot[1,1], col="red")
abline(0, rot[2,2]/rot[1,2], col="blue")

plot(pca.res$x, pch="", pty="s", xlim=c(-5,5), ylim=c(-5,5), main="scores")
text(pca.res$x, labels=c(1:100), cex=0.6)
abline(0, rot[2,1]/rot[1,1], col="lightgrey")
abline(0, rot[2,2]/rot[1,2], col="lightgrey")
abline(h=0, col="red")
abline(v=0, col="blue")

biplot(pca.res, cex=0.6, scale=0)
abline(0, rot[2,1]/rot[1,1], col="red")
abline(0, rot[2,2]/rot[1,2], col="blue")
abline(h=0, v=0)


