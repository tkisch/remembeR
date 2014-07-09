extrem <- function(dats, n){
  u.dta <- lapply(dats, unique)
  s.dta <- lapply(u.dta, sort)
  f.sort<- function(data){
    s.dta <- lapply(data, sort)
  }
  d.sort <- f.sort(dats)
  f.ext.T <- function(d.sort, n){
    sort(d.sort, decreasing = T)[1:n]
  }
  t <- sapply(s.dta, f.ext.T, n)
  f.ext.F <- function(d.sort, n){
    sort(d.sort, decreasing = F)[1:n]
  }
  f <- sapply(s.dta, f.ext.F, n)
  matrix(c(t,f), nrow=n, ncol=2)
}
