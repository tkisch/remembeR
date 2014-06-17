
# Introduction to time series ---------------------------------------------
#  ------------------------------------------------------------------------

# class ts - start, frequency ---------------------------------------------

rawdat <- c(88, 76, 112, 109, 91, 98, 139)
rawdat

ts.dat <- ts(rawdat, start=2004, freq=1)
ts.dat

# frequency: "1" for yearly, "4" for quartery, "12" for monthly
# since freq=1, there can't be any seasonal effect, just a trend


par(mfrow=c(1,2))
plot(rawdat)
plot(ts.dat)

dev.off()

start(ts.dat)
end(ts.dat)
frequency(ts.dat)
deltat(ts.dat)
time(ts.dat)

# windows aims at selecting a subset of the time series

plot(ts.dat)
plot(window(ts.dat, start=2006, end=2009))


# frequency vs. deltat ----------------------------------------------------
# frequency returns the number of samples per unit time and deltat the time interval between observations (see ts).

deltat(t.rDay)


?deltat


# Dates and Times in R ----------------------------------------------------

# as.Date()
# library(chron) does not handle time zones
# POSIXct and POSIXlt allow for tome zone control
# Sys.Date()
# Sys.time()
# date()

?as.Date

?format  
?Date
# A character string.
# If not specified, it will try "%Y-%m-%d" then "%Y/%m/%d" on the first non-NA element, and give an error if neither works.

as.Date("27.01.12", format="%d.%m.%y")
# for various formats strptime or p18 script
?strptime


dat <- c(rawdat, rawdat*2, rawdat, rawdat*4)


#  ------------------------------------------------------------------------



# time series
tsd <- ts(dat, start=c(1996,1), freq=12)
plot(tsd)

# multiple times series
dat <- read.table("http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat", header=T)
tsd <- ts(dat, start=1958, fre=12)

ts.plot(tsd)
plot(tsd)

dev.off()

tsd[,1] <- tsd[,1]/tsd[1,1]*100
tsd[,2] <- tsd[,2]/tsd[1,2]*100
tsd[,3] <- tsd[,3]/tsd[1,3]*100

clr <- c("green3", "red3", "blue3")
plot.ts(tsd[,1], ylim=range(tsd), ylab="Index", col=clr[1])
lines(tsd[,2], col=clr[2])
lines(tsd[,3], col=clr[3])


# transformations ---------------------------------------------------------

hist(lynx)
qqnorm(lynx); qqline(lynx)


# stationariy -------------------------------------------------------------

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

d.hst <- read.table(paste(kurs.url, "hstart.dat", sep=""), header=T)

str(d.hst)
plot(d.hst)

ts.hst <- ts(d.hst[,1], start=1966, freq=12)
# ts.hst <- ts(d.hst, start=1966, freq=12)

cov(d.hst)
cor(d.hst)
cov(d.hst[1:45,], d.hst[1:45,])
cor(d.hst[1:45,], d.hst[1:45,])


# differencing ------------------------------------------------------------

SwissTraffic <- ts(c(100, 102.7, 104.2, 104.6, 106.7, 106.9, 107.6, 109.9, 112, 114.3, 117.4, 118.3,120.9, 123.7, 124.1, 124.6, 125.6, 127.9, 127.4, 130.2, 131.3), start=1990, freq=1)
plot(SwissTraffic)
?diff
diff(SwissTraffic, lag=3)


st1 <- SwissTraffic[-21]
length(st1)
st2 <- st1[-1]
length(st2)
st2-st1

par(mfrow=c(2,2))
plot(SwissTraffic)
plot(diff(SwissTraffic))
plot(diff(SwissTraffic, diff=2))
plot(diff(SwissTraffic, diff=10))

plot(diff(SwissTraffic, lag=1, diff=10))
plot(diff(SwissTraffic, lag=10))



data(co2)
dev.off()
par(mfrow=c(1,2))
plot(co2)
plot(diff(co2, lag=12)) # remove monthly seasonal effect, by lag=12
plot(diff(diff(co2, lag=12)))

par(mfrow=c(2,1))
plot(decompose(co2, type="additive"))
plot(decompose(co2, type="multiplicative"))

?decompose

plot(stl(co2, t.window=19, s.window="periodic"))

monthplot(stl(co2, t.window=2, s.window=13))


# parametric modelling ----------------------------------------------------
# p43
dat <- read.table("http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/Maine.dat", header=T)
maine <- ts(dat, start=c(1996,1), freq=12)
plot(maine)


# getting trend estimates - extract time component of data
tr <- as.numeric(time(maine))

# center these x-values... 
tc <- tr-mean(tr)

# defining a factor for month to control for seasonality
mm <- factor(rep(month.name, 11), levels=month.name)[1:128]


# fitting the polynomials

fit04 <- lm(maine~tc+I(tc^2)+I(tc^3)+I(tc^4))  # achtung: mm muss hier noch rein...
cf <- coef(fit04)
t.est.04 <- cf[1]+cf[2]*tc+cf[3]*tc^2+cf[4]*tc^3+cf[5]*tc^4
t.est.04

dev.off()
plot(fitted(fit04))
plot(predict(fit04))



plot(maine)
lines(as.numeric(time(maine)), fitted(fit04), col="red")
lines(as.numeric(time(maine)), predict(fit04), col="green")

lines(tr, predict(fit04))


# ? t04.adj <- t.est.04-mean(t.est.04)+mean(maine)
# lines(tr,t04.adj )

fit05 <- lm(maine~tc+I(tc^2)+I(tc^3)+I(tc^4)+I(tc^5))
lines(tr, predict(fit05), col="red")

fit06 <- lm(maine~tc+I(tc^2)+I(tc^3)+I(tc^4)+I(tc^5)+I(tc^6))
lines(tr, predict(fit06), col="green")


# instead of inspecting by eye which order fits best: residual analysis

re.est <- maine-fitted(fit04)

plot(re.est)
fit <- loess(re.est~tr)
lines(tr, fitted(fit), col="red")
abline(h=0, col="grey")

#  ------------------------------------------------------------------------


# Autoregressive Modells --------------------------------------------------
rm(list=ls())
dev.off()
#  Autocorrelation --------------------------------------------------------

dat <- read.table("http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/wave.dat", header=T)
str(dat)

wave <- ts(dat[,1])

plot(window(wave, 1,60), ylim=c(-800, 800), ylab="Height")
title("Wave Tank Data")


# lag plot ----------------------------------------------------------------

?lag.plot

lag.plot(wave, lags=20, do.lines=F)
lag.plot(wave, lags=1, do.lines=T)


# acf auto-correlation function -------------------------------------------

?acf

par(mfrow=c(1,2))
acf(wave, plot=T)
pacf(wave)


# acf for different non-stationary data -----------------------------------

par(mfrow=c(3,2))

data(EuStockMarkets)
esm <- EuStockMarkets
tmp <- EuStockMarkets[,2]
smi <- ts(tmp, start=start(esm), freq=frequency(esm))

plot(smi, main="SMI: trend-data")
acf(smi, lag.max=100)
#  ------------------------------------------------------------------------
data(nottem)
plot(nottem, main="seasonality")
acf(nottem)
#  ------------------------------------------------------------------------
data(AirPassengers)
plot(AirPassengers, main="trend & seanonality")
acf(log(AirPassengers), lag.max=48)

#  ------------------------------------------------------------------------
dev.off()
data(beavers)
str(beaver1)

dbe <- ts(beaver1$temp)
plot(dbe)

plot(dbe[1:113], dbe[2:114], pch=20)


#  ------------------------------------------------------------------------
#  ------------------------------------------------------------------------


# Stationary time series models -------------------------------------------


# ARMA Models -------------------------------------------------------------

# autoregressive moving average processes
# parametric models

plot(ts(rnorm(200, mean=0, sd=1))) # white noise

lofit <- loess(tsdat~as.numeric(time(tsdat)))
lines(as.numeric(time(tsdat)), fitted(lofit), col="red")
abline(h=0, col="grey")
abline(h=mean(tsdat), col="green")
hist(tsdat)

set.seed(20)
tsdat <- ts(rnorm(200, mean=0, sd=1))

acf(tsdat)
pacf(tsdat)


# AR, MA, ARMA
# AR autoregressive, MA moving average, ARMA autoregressive moving average

# AR(p) processes need to be stationary -> global mean = 0, resp. shifted processes...
# conditions for an AR process to be stationary

# abs(alphas) < 1
# or abs(polyroot() > 1: verify if a AR(3) with alpha1=0.4, alpha2=-0.2, alpha3=0.3 is stationary

abs(polyroot(c(1,0.4, -0.2, 0.3)))

# example

set.seed(24)
E <- rnorm(200,0,1) # gaussian white noise
x <- numeric()
x[1] <- E[1]

for(i in 2:200) {
  x[i] <- 0.8*x[i-1] + E[i]
}

plot(ts(x), main="AR(1) Process")
lines(time(x), fitted(loess(x~time(x))), col="red")


acf(ts(x))$acf
acf(ts(x))$acf[[2]] # = alpha 1, autocorrelation at lag 1

# arima.sim

?arima.sim # to simulate arma data

plot(ts(arima.sim(n = 200, list(ar = c(-0.4, -0.2, 0.3)))))

?ARMAacf  # to determine autocorrelation from ar modell coefficients

par(mfrow=c(2,1))

autocorr <- ARMAacf(ar = c(0.4, -0.2, 0.3), lag.max=20)
plot(0:20, autocorr, type="h")

pautocorr <- ARMAacf(ar = c(0.4, -0.2, 0.3), pacf=T,  lag.max=20)
plot(pautocorr, type="h")  # ab 4 = 0 for al k > p, pacf = 0



# fitting an AR(p) model to data ------------------------------------------

# three main steps

# model and order need to be determined
# estimation of model parameters
# check quality of the fit by residual analysis


# 1. acf  = exponentially decreasing?
# 2. pacf = cut-off value?

plot(lynx)
par(mfrow=c(1,2))
acf(lynx)
pacf(lynx)


# all R routines fit shifted modells by default

log(lynx)-mean(log(lynx))


# Serie1, Aufgabe 1 -------------------------------------------------------

rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/rain.txt"
d.rain <- ts(read.table(kurs.url, header=T))

str(d.rain)

t.rain <- ts(d.rain, start=1965, frequency=4)
str(t.rain)
time(t.rain)
plot(t.rain)


# Serie1, Aufgabe 2 -------------------------------------------------------
# as.Date, format, factor -------------------------------------------------

rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/rainDay.txt"
d.rainDay <- read.table(kurs.url, header=T)

str(d.rainDay)


d.rainDay$DATE <- as.Date(d.rainDay$DATE, format="%d.%m.%Y")
str(d.rainDay)

t.rDay <- ts(d.rainDay$rain, start=2000, freq=365)

str(d.rainDay)

t.weekday <-factor(weekdays(d.rainDay$DATE, abbr=T), levels=c("Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"))
levels(t.weekday)


t.month <- factor(months(d.rainDay$DATE, abbr=T), levels=c("Jan", "Feb", "Mrz", "Apr", "Mai", "Jun", "Jul", "Aug", "Sep", "Okt", "Nov", "Dez"))
levels(t.month)

t.quart <- factor(quarters(d.rainDay$DATE, abbr=F))
levels(t.quart)

d.new <- data.frame(date=d.rainDay$DATE, rain=t.rDay, weekday=t.weekday, month=t.month, quarter=t.quart)

str(d.new)
str(t.rDay)

plot(d.new$rain)
plot(d.new)

# Serie 1, Aufgabe 2.d ----------------------------------------------------
# logst::regr0, boxplots --------------------------------------------------

library(regr0)
install.packages("regr0", repos="http://R-Forge.R-project.org")

logst(d.rainDay$rain)
par(mfrow=c(2,2))

boxplot(logst(d.rainDay$rain)~d.new$month)
boxplot(logst(d.rainDay$rain)~d.new$weekday)
boxplot(logst(d.rainDay$rain)~d.new$quart)


# plotting parts of a time series window() --------------------------------

?window
dev.off()
plot(window(d.new$rain, start=2006, end=2007))


# freq vs. deltat ---------------------------------------------------------

?ts

# frequency: the number of observations per unit of time.
# 
# deltat: the fraction of the sampling period between successive observations; e.g., 1/12 for monthly data. Only one of frequency or deltat should be provided.



# Serie1, Aufgabe 1 -------------------------------------------------------

# stationariy is a major prerequisite for being able to learn from time series data
# achiving this by decomposition
# goal is to get sth. of the form:
# X = m + s + R , or
# X = m*s*R
# so we decomposed into trend (m), seasonal effect(s) and a remainder (R)
# then R should be stationary

rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

d.hst <- read.table(paste(kurs.url, "hstart.dat", sep=""))

str(d.hst)
plot(d.hst)

ts.hst <- ts(d.hst[,1], start=1966, freq=12)
# ts.hst <- ts(d.hst, start=1966, freq=12)

plot(ts.hst)


# parametric modelling ----------------------------------------------------

Time <- 1:length(ts.hst)
Time
Months <- factor(rep(month.name, length(ts.hst)/12), levels=month.name)
Months

plot(ts.hst)

length(ts.hst)
length(Months)
length(Time)

par(mfrow=c(2,2))

H.lm1 <- lm(ts.hst ~ Time)
H.fit1 <- ts(fitted(H.lm1), start=1966, freq=12)
plot(ts.hst)
lines(H.fit1)

H.lm2 <- lm(ts.hst ~ Months + Time)
H.fit2 <- ts(fitted(H.lm2), start=1966, freq=12)
lines(H.fit2)

H.lm7 <- lm(ts.hst ~ Months + Time + I(Time^2) + I(Time^3) + I(Time^4) + I(Time^5) + I(Time^6) + I(Time^7))
H.fit7 <- ts(fitted(H.lm7), start=1966, freq=12)

plot(ts.hst)
lines(H.fit)
lines(H.fit7, lty=3, col=2)

# Serie2, Aufgabe 2 -------------------------------------------------------
# stl decomposition (seasonal-trend decomposition with LOESS --------------
dev.off()
?stl

plot(ts.hst)
H.stl <- stl(ts.hst, s.window="periodic")
plot(H.stl)

str(H.stl)
H.stl$win

plot(residuals(H.lm2))
plot(H.stl$time.series[,"remainder"])

plot(ts.hst)
lines(ts.hst-H.stl$time.series[,"remainder"], col=2, lty=3)

summary(H.stl)

H.stl15 <- stl(ts.hst, s.window=15)
plot(H.stl15)

plot(H.stl)

H.stl <- stl(ts.hst, s.window="periodic")
str(H.stl)
remainder <- H.stl$time.series[,3]
plot(ts.hst)

H.np <- ts.hst-remainder
lines(H.np, col=2, lty=2)

plot(ts(resid(H.lm7), start=1966, freq=12), lty=3, col=2, ylab="residuals")
lines(remainder)
abline(h=0)

legend(1966, -22, legend=c("parametric model", "stl"), col=c(2,1), lty=c(3,1))

# Serie1, Aufgabe 1.d-e(resp f)
rm(list=ls())
dev.off()
hstart <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/hstart.dat")
hstart <- ts(hstart[,1], 1966, freq=12)
time(hstart)

H.stl <- stl(hstart, s.window="periodic")
H.stl.var <- stl(hstart, s.window=15)

plot(H.stl)

head(H.stl$time.series)
str(H.stl)
par(mfrow=c(3,1))
plot(H.stl$time.series[,1])
plot(H.stl$time.series[,2])
plot(H.stl$time.series[,3])

seasonal <- H.stl$time.series[,1]
seasonal.var <- H.stl.var$time.series[,1]

par(mfrow=c(1,2))
monthplot(seasonal)
monthplot(seasonal.var)


# special filters ---------------------------------------------------------
# Aufgabe e, filters
dev.off()
plot(hstart,lty=3)

H.filt <- filter(hstart, c(1,rep(2,11),1)/24)

c(1,rep(2,11),1)/24
length(c(1,rep(2,11),1))

trend <- H.stl$time.series[,2]
lines(trend, col=3)
lines(H.filt, lty=2, col=2)


# Serie2, Aufgabe 2 -------------------------------------------------------

seasonal <- H.stl$time.series[,1]
plot(seasonal)

hstart.ds <- hstart-seasonal

dev.off()
plot(hstart)
lines(hstart.ds, lty=2, col=10)

plot(H.stl)
plot(seasonal)

plot(H.stl$time.series[,3])

plot(hstart, ylim=c(0,300))
lines(hstart-H.stl$time.series[,1], lty=2, col=10)
lines(hstart-H.stl$time.series[,2], lty=2, col=12)
lines(hstart-H.stl$time.series[,1]-H.stl$time.series[,2], lty=2, col=14)
lines(hstart-H.stl$time.series[,3], lty=2, col=16)


# filters -----------------------------------------------------------------

hstart.fil1 <- filter(hstart.ds, rep(1,5)/5)
hstart.fil2 <- filter(hstart.fil1, rep(1,5)/5)
hstart.fil3 <- filter(hstart.ds, c(1:4,4:1)/25)
hstart.fil4 <- filter(hstart.ds, rep(1,9)/9)

str(hstart.fil1)
?filter

plot(hstart)
plot(hstart.fil1)
lines(hstart.fil2, col=12)
lines(hstart.fil3, col=10)
lines(hstart.fil4, col=14)




# Serie2, Aufgabe 3 -------------------------------------------------------

rm(list=ls())
kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"
d.sim <- read.table(paste(kurs.url, "zeitsim.dat", sep=""), header=T)

d.sim
plot(ts(d.sim))

plot(stl(ts.hst, s.window="periodic"))
plot(stl(ts.hst, s.window=2))


# Serie1, Aufgabe 1 -------------------------------------------------------

rm(list=ls())
dev.off()

# Serie3, Aufgabe 1 -------------------------------------------------------

d.co2 <- co2
str(d.co2)
length(d.co2)

# plot time series
plot(d.co2)

# what kind of non-stationarity

# trend & seasonality

# what order? check number of extrema and add 1

# one??

plot(co2)


# p44


tr <- as.numeric(time(d.co2))
tc <- tr-mean(tr)
mm <- factor(rep(month.name, length(d.co2)/12), levels=month.name)

length(tr)
length(tc)
length(mm)
tr
tc
mm

fit1 <- lm(d.co2~tc+mm)
summary(fit1)
fit2 <- lm(d.co2~tc+I(tc^2)+mm)
fit4 <- lm(d.co2~tc+I(tc^2)+I(tc^3)+I(tc^4)+mm)

dev.off()
par(mfrow=c(2,1))

cf1 <- coef(fit1)
cf1
cf2 <- coef(fit2)
cf2
cf4 <- coef(fit4)

plot(d.co2, type="l")
abline(fit1)
lines(t.est.1)
lines(t1.adj)

t.est.1 <- cf1[2]*tc+cf1[1]
t1.adj <- t.est.1-mean(t.est.1) + mean(co2)

t.est.2 <- cf2[2]*tc+ cf2[3]*tc^2+cf2[1]
t2.adj <- t.est.2-mean(t.est.2) + mean(co2)

t.est.4 <- cf4[2]*tc+ cf4[3]*tc^2+cf4[4]*tc^3+cf4[5]*tc^4+cf4[1]
t4.adj <- t.est.4-mean(t.est.4) + mean(co2)


plot(co2)
lines(tr, t1.adj, col="red")
lines(tr, t2.adj, col="green")
lines(tr, t4.adj, col="blue")



# adding a LOESS smoother

res1 <- co2-fitted(fit1)
lofit1 <- loess(res1~tr)


res2 <- co2-fitted(fit2)
lofit2 <- loess(res2~tr)

res4 <- co2-fitted(fit4)
lofit4 <- loess(res4~tr)

dev.off()
par(mfrow=c(2,2))
plot(res1)
abline(h=0, col="grey")
lines(tr, fitted(lofit1), col="red")

plot(res2)
abline(h=0, col="grey")
lines(tr, fitted(lofit2), col="red")

plot(res4)
abline(h=0, col="grey")
lines(tr, fitted(lofit4), col="red")


# Serie3, Aufgabe 2 -------------------------------------------------------
rm(list=ls())
dev.off()

kurs.url = "http://stat.ethz.ch/Teaching/Datasets/WBL/"

d.kreatin <- read.table(paste(kurs.url, "kreatin.dat", sep=""), header=T)

str(d.kreatin)

plot(d.kreatin$gehalt)

t.gehalt <- ts(d.kreatin$gehalt, start=1, frequency=1)
plot(t.gehalt)


# lagged scatterplot ------------------------------------------------------
# to visually test autocorrelation
?lag.plot

lag.plot(t.gehalt, lag=1)
lag.plot(t.gehalt, lag=3)

# Ljung-Box Test for autocorrelations

Box.test(t.gehalt, lag=5, type="Ljung-Box")

# plug-in approach; using estimated autocovariances as the basis... p48

par(mfrow=c(2,2))
plot(t.gehalt)
acf(t.gehalt, type="correlation", plot=T)
pacf(t.gehalt, plot=T)

# Serie3, Aufgabe 3 -------------------------------------------------------

# theoretisch

# welche Correlogramme passen zu welchen ts plots


# Serie3, Aufgabe 4 -------------------------------------------------------

rm(list=ls())
dev.off()


dat <- read.table("http://staff.elena.aut.ac.nz/Paul-Cowpertwait/ts/cbe.dat", sep="", header=T)

str(dat)

tsd <- ts(dat, start=1958, freq=12)

plot(tsd)
plot(tsd[,3])

tselec <- tsd[,3]
plot(tselec)

dev.off()
dectsd <- decompose(tsd[,3], type="additive")
acf(tselec)

str(dectsd)
plot(dectsd)
plot(decompose(tsd[,3], type="multiplicative"))

dev.off()


acf(dectsd$random, na.action=na.pass, type="correlation", plot=T)


# decomposition using stl (Seasonal Decomposition of Time Series b --------

?stl
str(tselec)

logtselec <- log(tselec)

plot(logtselec)

plot(stl(logtselec, s.window="periodic"))

?monthplot
fit <- stl(log(co2), s.window = 3, t.window = 20)
monthplot(fit, choice = "seasonal", cex.axis = 0.8)



# figuring out which s.window is appropriate
# generate 12 plot, when it's smooth enough take it


dev.off()
par(mfrow=c(4,3))
sw <- c(2:4, 6:8, 12:14, 30:32)
length(sw)

for (i in sw){
  monthplot(stl(log(tselec), s.window=i))
}

dev.off()
par(mfrow=c(3,1))
deltat(tselec)
start(tselec)-end(tselec)

monthplot(stl(log(tselec), s.window="periodic"))
monthplot(stl(log(tselec), s.window=30))
monthplot(stl(log(tselec), s.window=12))

# differencing

eleclag12 <- diff(tselec, lag=12)
par(mfrow=c(2,1))
plot(eleclag12)
acf(eleclag12, ylim=c(-1,1))


# Serie4, Aufgabe 1 -------------------------------------------------------

rm(list=ls())
dev.off()

d.ts <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/ts_S3_A2.dat", header=T)

str(d.ts)

ts1 <- ts(d.ts$ts1)
ts2 <- ts(d.ts$ts2)

par(mfrow=c(2,3))
plot(ts1);acf(ts1);pacf(ts1)
plot(ts2);acf(ts2);pacf(ts2)

# check whether they could have been generated by an AR process...
# are they stationary...
# what is their mean...
# shifted AR processes...

mean(ts1)
mean(ts2)

# since we knwo the coefficients
# abs(alpha_i) < 1 memory fades out

# complex roots must exceed 1
# abs(polyroot(c(1, alpha_i...)))

t.ar1 <- c(-0.5, 0.2)
abs(polyroot(c(1, t.ar1)))

# ....

# b.) check acf and pacf to see if they could have been generated by an AR process...
# is the acf exponentially decreasing?
# recognizable cut-off at some lag p smaller than 5-10 in pacf

dev.off()
par(mfrow=c(2,1))
acf(ts1)
pacf(ts1)

acf(ts2)
pacf(ts2)

# acf lag1 and pacf lag1 need to be the same
acf(ts1)$acf[[2]]
pacf(ts1)$acf[[1]]


# which model order should we take...?
# from which lag on are pacf not statistically distinguishable different from zero?

str(pacf(ts1))
plot(pacf(ts1)$acf, type="l")
abline(h=0)


# Serie 4, Aufgabe 2 ------------------------------------------------------

rm(list=ls())
dev.off()

?arima.sim
?ARMAacf
plot(arima.sim(n=100, list(ar=c(0,0,0))))   # gaussian white noise

# AR(3) process, given alphas

set.seed(3)
ar.coef <- c(0.5, -0.4, 0.6)

# plot(ts(arima.sim(n = 100, list(ar = c(0.5, -0.4, 0.6)))))
ts.sim <- arima.sim(n=100, list(ar=ar.coef))

plot(ts.sim)

abs(polyroot(c(1, -ar.coef)))

acf(ts.sim)
pacf(ts.sim)

# compute theoretical (p)acf from an arma process

par(mfrow=c(2,1))

?ARMAacf

autocorr <- ARMAacf(ar.coef, lag.max=20)
pautocorr <- ARMAacf(ar.coef, pacf=T, lag.max=20)

par(mfrow=c(2,1))
acf(ts.sim)
plot(ARMAacf(ar.coef, lag.max=20), type="h", main="theoretische ACF ARMAacf")


pacf(ts.sim)
plot(pautocorr, type="h")


# Serie 4, Aufgabe 3

rm(list=ls())
dev.off()

yields <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/yields.dat", header=F)

str(yields)

ty <- ts(yields)

plot(ty)
abline(h=mean(ty))


# investigate the dependencie structur of the series
# acf, pacf, lagplots
par(mfrow=c(2,3))
plot(ty)
abline(h=mean(ty))
acf(ty)
pacf(ty)

lag.plot(ty, lag=5, do.lines=F, layout=c(4,5))

r.yw <- ar(ty, method="yw", order.max=1)

r.yw
var(r.yw$resid, na.rm=T)


# Yule-Walker Equation ----------------------------------------------------

dev.off()

# pacf(ty) proposes AR(p) Modell with p=1: AR(1)-Modell



# setup, functions --------------------------------------------------------

rm(list=ls())
dev.off()

# Serie5, Aufgabe 1 -------------------------------------------------------


yields <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/yields.dat", header=F)

str(yields)

t.yields <- ts(yields[,1])

time(t.yields)

plot(t.yields)


par(mfrow=c(2,1))
acf(t.yields)
pacf(t.yields)

# acf: exponentially decreasing
# pacf: after cut-off p=1 no signifance

acf(t.yields, plot=F)[1]

# Yule-Walker Method ------------------------------------------------------

?ar
r.yw

r.yw <- ar(t.yields, method="yw", order.max=1)
r.yw <- ar.yw(t.yields, aic=F, order.max=1)
r.yw
str(r.yw)
r.yw$ar

# Burg Method -------------------------------------------------------------

r.burg <- ar(t.yields, method="burg", order.max=1)
mean(t.yields)

fit.aic <- ar.burg(t.yields)
plot(0:fit.aic$order.max, fit.aic$aic, type="l")
r.burg$ar

fit.aic$aic

# check residuals ---------------------------------------------------------

r.burg <- ar(t.yields, method="burg", order.max=2)
str(r.burg)

plot(r.burg$resid)

par(mfrow=c(1,2))
acf(r.burg$resid, na.action=na.pass)
pacf(r.burg$resid, na.action=na.pass)

qqnorm(as.numeric(r.burg$resid))
qqline(as.numeric(r.burg$resid))


# MLE Method --------------------------------------------------------------
# ARIMA allows to compute confidence intervals ----------------------------



r.mle <- ar(t.yields, method="mle", order.max=1)
r.mle <- arima(t.yields, order=c(1,0,0), include.mean=T)
confint(r.mle)

# comparison --------------------------------------------------------------

r.mle$ar

r.yw$ar
r.burg$ar
r.mle$coef[1]

# Serie5, Aufgabe 2 -------------------------------------------------------

rm(list=ls())
dev.off()

d.force <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/kraft.dat", header=F)
str(d.force)

t.force <- ts(d.force[,1])
plot(t.force)

time(t.force)

t.forceA <- window(t.force, end=280)
length(time(t.force))-length(time(t.forceA))

par(mfrow=c(2,1))
plot(t.force, xlim=c(0,330))
plot(t.forceA, xlim=c(0,330))

par(mfrow=c(1,2))
acf(t.forceA, xlim=c(0,25), ylim=c(-1,1))
pacf(t.forceA, xlim=c(0,25), ylim=c(-1,1))
abline(v=17)


# get order either by pacf or by plotting aic -----------------------------
dev.off()

aic.burg <- ar(t.forceA, method="burg")
str(aic.burg)
aic.yw <- ar(t.forceA, method="yw")
aic.mle <- ar(t.forceA, method="mle")


plot(0:aic.burg$order.max, aic.burg$aic, type="l")
lines(0:aic.yw$order.max, aic.yw$aic, col="red")
lines(0:aic.mle$order.max, aic.mle$aic, col="green")

str(aic.burg)
plot(aic.burg$aic, type="l")
abline(h=min(aic.burg$aic))
min <- aic.burg$aic
min

# fitting order p=9
dev.off()
p <- 9
ar.force <- arima(t.forceA, order=c(p, 0,0), method="ML")
ar.force
str(ar.force)
plot(ar.force$residuals)
par(mfrow=c(2,1))
acf(ar.force$residuals)
pacf(ar.force$residuals)

qqnorm(ar.force$residuals)
qqline(ar.force$residuals)

# compare to order p = 6

ar.force6 <- arima(t.forceA, order=c(6, 0,0), method="ML")
ar.force6
plot(ar.force6$residuals)
acf(ar.force6$residuals)
pacf(ar.force6$residuals)


# tukey-anscome plot
dev.off()
plot(t.forceA-ar.force6$residuals, ar.force6$residuals)
abline(h=0)



# d. predict, extrapolate, forecast ---------------------------------------

force.pred <- predict(ar.force, n.ahead=40)
plot(t.force)
lines(force.pred$pred, lty=2)
lines(force.pred$pred + 1.96*force.pred$se, lty=3)
lines(force.pred$pred - 1.96*force.pred$se, lty=3)

plot(window(t.force, start=250))
lines(force.pred$pred, col="red")
lines(force.pred$pred + 1.96*force.pred$se, lty=3)
lines(force.pred$pred - 1.96*force.pred$se, lty=3)
