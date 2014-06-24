###########################################################
# check if packages are installed, if yes require, if no install

# pkg <- c("sfsmisc","MASS","nnet","regr0")
# as.matrix(pkg)

inst <- function(x){
  require(x) || install.packages(x)
  
}

# apply(as.matrix(pkg), 2, inst)

###########################################################
# regr0 package (W.Stahel ETHZ)
# source("http://stat.ethz.ch/~stahel/regression/regr.R")
