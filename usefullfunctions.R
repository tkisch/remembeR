###########################################################
# check if packages are installed, if yes require, if no install

# pkg <- c("sfsmisc","MASS","nnet","regr0")
# as.matrix(pkg)

chkinst <- function(x){
  require(x) || install.packages(x)
  
}

# apply(as.matrix(pkg), 2, inst)

###########################################################
# regr0 package (W.Stahel ETHZ)
# source("http://stat.ethz.ch/~stahel/regression/regr.R")
# install.packages("regr0", repos="http://R-Forge.R-project.org")

###########################################################
# german umlauts


# defining dataset

d.data <- as.data.frame(cbind(c("äzÄzüzÜZöZÖ"), c("äzÄzüzÜZöZÖ")))
d.data

# defining "Umlauts"

a <- c("ä", "ae")
A <- c("Ä", "Ae")
u <- c("ü", "ue")
U <- c("Ü", "Ue")
o <- c("ö", "oe")
O <- c("Ö", "Oe")

# Matrix of substitutions
m <- cbind(a,A,u,U,o,O, deparse.level=0)

swiss2 <- function(m, data){
  for (i in seq_len(ncol(m))){
    data <- gsub(paste(m[,i], sep=",")[1],paste(m[,i], sep=",")[2], data,
                 ignore.case=F, perl = F, fixed = F, useBytes = F)
  }
  data
}

swiss2(m, d.data$V1) 
