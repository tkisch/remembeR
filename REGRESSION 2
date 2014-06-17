rm(list=ls())
dev.off()


# packages ----------------------------------------------------------------

library(sfsmisc)
library(MASS)
library(nnet)
library(regr0)

# Serie 7: Aufgabe 1----------------------------------------------------
# Kontingenztafeln

?chisq.test
?fisher.test
?mcnemar.test
?dhyper

d.umwelt<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/umwelt.dat", header=T)

# R ordnet Faktorstufen alphabetisch... für Darstellung nicht sinnvoll

levels(d.umwelt$Schule)         
d.umwelt$Schule <- factor(d.umwelt$Schule, levels = c("ungelernt", "Lehre", "ohne.Abi", "Abitur", "Studium"))

levels(d.umwelt$Beeintr)
d.umwelt$Beeintr <- factor(d.umwelt$Beeintr, levels = c("nicht", "etwas", "ziemlich", "sehr"))


# Kontingenztafel ---------------------------------------------------------

tab.umwelt <- table(d.umwelt$Schule, d.umwelt$Beeintr)
prop.tab.umwelt <- prop.table(tab.umwelt, 1)  # relativ zu Zeilensumme = 1


# Plots -------------------------------------------------------------------

par(mfrow=c(1,2))
barplot(t(prop.table(tab.umwelt, 1)), beside=T, legend=levels(d.umwelt$Beeintr))
barplot(prop.table(tab.umwelt, 1), beside=T, legend=levels(d.umwelt$Schule))


# Chisq-Wert manuel berechnen ---------------------------------------------

str(prop.tab.umwelt)
# prop.tab.umwelt[1,1]+prop.tab.umwelt[2,1]+prop.tab.umwelt[3,1]+prop.tab.umwelt[4,1]+prop.tab.umwelt[5,1]

sum(tab.umwelt)

n <- sum(tab.umwelt)
t.s <- apply(tab.umwelt, 1, sum)
t.b <- apply(tab.umwelt, 2, sum)

sqrt(tab.umwelt)


t(t.b)
umw.e <- t.s %*% t(t.b)/n
umw.e

barplot(as.table(umw.e), beside=T)
barplot(prop.table(as.table(umw.e),2), beside=T)


# chisq-Test --------------------------------------------------------------

dchisq(q, df, ncp = 0, lower.tail = TRUE, log.p = FALSE)

par(mfrow=c(2,2))
x <- c(1:20)
plot(dchisq(x, 3, ncp = 0, log = FALSE), type="l")
plot(pchisq(x, 3, ncp = 0, log = FALSE), type="l")

plot(dchisq(x, 10, ncp = 0, log = FALSE), type="l")
plot(pchisq(x, 10, ncp = 0, log = FALSE), type="l")

assocplot(as.matrix(tab.umwelt), col = c("black", "red"), space = 0.3, main = NULL, xlab = NULL, ylab = NULL)
assocplot(tab.umwelt, col = c("black", "red"), space = 0.3, main = NULL, xlab = NULL, ylab = NULL)

chisq.test(tab.umwelt)

# Serie 7, Aufgabe 2 Fisher's Exact Test ----------------------------------
rm(list=ls())
dev.off()
c.table <- cbind(c(21, 3), c(14,10))

dimnames(c.table) <-  list(c("promote", "hold file"), c("male", "female"))
c.table

chisq.test(c.table)
fisher.test(c.table)
mcnemar.test(c.table)


# Serie 8: Aufgabe 1----------------------------------------------------

# Logistische Regression --------------------------------------------------

d.h<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/heart.dat", header=T)

# Gruppierte Daten

plot(d.h$N/d.h$m~d.h$age)
plot(d.h$N/d.h$m~d.h$age, cex=0.5*d.h$m, pch=16, type="p", col=3)

# logistische Regression mit Gruppierten Daten

# Zielvariable als Matrix eigeben:

# 1. Spalte: Anzahl Erfolge:      N
# 2. Spalte: Anzahl Misserfolge:  m-N

# N: Anzahl Personen mit Symptomen
# m: Anzahl Personen total

# cbind(d.h$N, d.h$m-d.h$N)

r.glm <- glm(cbind(N, m-N)~age, family=binomial(link="logit"), data=d.h)
r.glm.regr <- regr(cbind(N,m-N)~age, data=d.h, family=binomial(link="logit"))

summary(r.glm)                # Achtung hier wird Wald-Test durchgefürt
anova(r.glm, test="Chisq")    # für likelihood Ratio Test, oder
drop1(r.glm, test="Chisq")

summary(r.glm.regr)           # regr macht per default likelihood ratio test

coef(r.glm)
coef(r.glm.regr)


# Residuen-Analyse --------------------------------------------------------
# Achtung: welche Residuen?

#           1. Response-Residuals vs. estimated P
#               Pearson(weighted) residuals vs. P
#           2. working residuals vs. values of eta

library(sfsmisc)

dev.off()
?TA.plot

TA.plot(r.glm)
TA.plot(r.glm.regr)

plot(r.glm.regr)


# ist Annahme einer lineare Beziehung sinnvoll? Oder müssen allenfalls quadratische Terme eingeführt werden?

par(mfrow=c(1,2))
termplot(r.glm, partial.resid=T, rug=T)
termplot(r.glm.regr, partial=TRUE, rug=TRUE)
termplot(r.glm.regr, partial=TRUE, rug=TRUE, smooth=panel.smooth)



# Plot Regressions-Kurve --------------------------------------------------
# Prediction
# Predict Wahrscheinlichkeiten, oder für Wahrscheinlichkeiten x_i berrechne

?predict
dev.off()
par(mfrow=c(2,2))


r.pred <- predict(r.glm.regr, newdata=data.frame(age=0:100), type="response")

plot(d.h$N/d.h$m~d.h$age, cex=0.3*d.h$m,type="p", col=3)
lines(r.pred)



# Jetzt aber Pi 10,20,90%

# log(pi/(1-pi))=beta0 + beta1x
# x1=(log(pi/(1-pi))-beta0)/beta1

summary(r.glm.regr)

# Beispiel pi=10%, yields age= 27
(log(0.1/(1-0.1))-coef(r.glm.regr)[1])/coef(r.glm.regr)[2]

# check: predict 
predict(r.glm.regr, data.frame(age=0:100), type="response")[27:28]

r.pred <- predict(r.glm.regr, newdata=data.frame(age=0:100), type="response")
r.pred[c(27,28)]

predict(r.glm.regr)

predict(r.glm.regr, newdata=data.frame(age=27), type="response")


# Serie 8, Aufgabe 2 ------------------------------------------------------

d.floh<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/floh.dat", header=T)
str(d.floh)

plot(d.floh$x1~d.floh$x2, pch=19, cex=1.5, col=d.floh$art+1)




# Achtung: wenn family=binomial nicht angegeben, wird Normalvert.  --------

# Modell logodds(pi)=logit(pi)=beta0 + beta1x1+beta2x2+beta3unt

floh.glm.full <- glm(art~x1+x2+unt, family= binomial(link="logit"), data=d.floh)
summary(floh.glm.full)


# Modellselektion: drop1, oder anova --------------------------------------

# sollte unt ins Modell aufgenommen werden?
drop1(floh.glm.full, test="Chisq")

# oder über anova
floh.glm.red <- glm(art~x1+x2, family= binomial(link="logit"), data=d.floh)
anova(floh.glm.full, floh.glm.red, test="Chisq")

# das selbe mit regr: regr berechnet automatisch likelihood ratio Test
floh.glm.full.regr <- regr(art~x1+x2+unt, family= binomial(link="logit"), data=d.floh)
summary(floh.glm.full.regr)

# macht Einbezug als linearer Zusammenhang sind?

library(sfsmisc)
par(mfrow=c(1,2))
termplot(floh.glm.red, partial.resid=T)

# welcher Art Käfer mit x1= 197, und x2= 303 zuzuordnen? Wenn pi > 0.5 Art = 1

predict(floh.glm.red, newdata=data.frame(x1=197, x2=303), type="response")

rm(list=ls())
dev.off()
# Serie 9: Aufgabe 1----------------------------------------------------


# Poisson-Verteilung ------------------------------------------------------

x <- seq(0, 50)

plot(x, dpois(x, 7.5), type="l", ylim=c(0, 0.2))
lines(x, dpois(x, 15), type="l", col="green")
lines(x, dpois(x, 20), type="l", col="red")
lines(x, dpois(x, 5), type="l", col="black")


par(mfrow=c(1,2))
plot(x, dpois(x, 7.5), type="l", ylim=c(0, 0.2))
abline(v=7.5)
plot(x, ppois(x, 7.5), type="l", ylim=c(0, 1))
abline(h=0.5, v=7.5)


# . -----------------------------------------------------------------------


d.spec<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/species.dat", header=T)

str(d.spec)

par(mfrow=c(2,4))
plot(Diversity~., data=d.spec, col=as.numeric(d.spec$Aspect), pch=19)

dev.off()

# Welches Modell scheint sinnvoll?
# Anzahl: Poisson Regression
#     - Zielvariable ist Poisson-Verteilt
#     - Linkfunktion: log

g1 <- glm(Diversity~., family = poisson(link = "log"), data=d.spec)
summary(g1)

g2 <- regr(Diversity~., family = poisson(link="log"), data=d.spec)
summary(g2)

coef(g1)
coef(g2)

# glm führt für einzelne Koeffizienten einen Wald-Test durch
# besser regr. regr führt likelihood ratio test durch

# Vergleiche dazu
s2 <- summary(g2)

s2$testcoef["p.value"]
drop1(g1, test="Chisq")$"Pr(>Chi)"

# Residuen
library(sfsmisc)
TA.plot(g1)
plot(g2)

# linearer Zusammenhang
par(mfrow=c(3,3))
termplot(g1, partial.resid=T, rug=T, smooth=panel.smooth)

# Variablenreduktion - Modellselektion - step backward


g1.step <- step(g2, direction="backward", trace=F)
g2.step <- step(g2, trace=F)

summary(g1.step)

summary(g2.step)
regr(g2.step$formula, data=d.spec, family="poisson")


s <- summary(g2.step)
str(s)

g2.step$anova
g2.step$formula


# Serie 9, Aufgabe 2 ------------------------------------------------------
rm(list=ls())
dev.off()
d.aids<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/aids.dat", header=T)

str(d.aids)

plot(number~., data=d.aids, type="l")

regr.glm1 <- regr(number~., family= "poisson"(link="log"), data=d.aids)

plot(regr.glm, sequence=F)

library(sfsmisc)
par(mfrow=c(1,2))
TA.plot(regr.glm)
termplot(regr.glm, partial=T, smooth=panel.smooth)

rm(list=ls())
dev.off()
# Serie 10, Aufgabe 1 -----------------------------------------------------
# ordered logit -----------------------------------------------------------

d.ment<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/mental.dat", header=T)
str(d.ment)
library(MASS)
library(regr0)

?plot.design
par(mfrow=c(1,2))
plot.design(d.ment[,1:2])
plot.design(d.ment[,-2])

summary(d.ment$Y)

# zuerst Zielvariable als geordneten Faktor angeben
d.ment$Y <- ordered(d.ment$Y, levels=c("Well", "Mild", "Moderate", "Impaired"))
summary(d.ment$Y)

library(MASS)
?polr

r.mental <- polr(Y~SES + LE, data=d.ment)
summary(r.mental)

drop1(r.mental, test="Chisq")

r.mental2 <- polr(Y~LE, data=d.ment)
anova(r.mental, r.mental2)



# "ftp://stat.ethz.ch/WBL/Source-WBL-4/03.RCodes/polr.plots.R" ------------


## NDK 8 ; Beat Jaggi
## rg2a, Serie 3, Aufgabe 1d)

# Koeffizienten aus dem polr-Objekt lesen
a <- r.mental$zeta # Schwellenwerte
b <- coef(r.mental)

# Berechnung der Wahrscheinlichkeiten
pSES0 <- array(dim=c(4,10))
pSES0[1,] <- rep(1,10)
for(k in 2:4)
  pSES0[k,] <- exp(0*b[1]+(0:9)*b[2]-a[k-1])/(1+exp(0*b[1]+(0:9)*b[2]-a[k-1]))


pSES1 <- array(dim=c(4,10))
pSES1[1,] <- rep(1,10)
for(k in 2:4)
  pSES1[k,] <- exp(1*b[1]+(0:9)*b[2]-a[k-1])/(1+exp(1*b[1]+(0:9)*b[2]-a[k-1]))

# für die einzelnen Klassen die Differenzen aus den kum. Wkeiten rechnen
dpSES0 <- rbind(-diff(pSES0),pSES0[4,])
dpSES1 <- rbind(-diff(pSES1),pSES1[4,])


# Wahrscheinlichkeiten für kumulierte Klassen:
par(mfrow=c(2,2))
for(i in 1:4) {
  matplot(0:9,cbind(pSES0[i,],pSES1[i,]),xlab='LE',pch="01",
          type="b",ylim=c(0,1),main=paste('P[Y>=',i-1,']'),ylab="")
  if(i==1) legend(6,0.9,c("SES = 1","SES = 0"),col=c("red","black"),
                  pch=c("1","0"))#,text.col=c("red","black"))
}


# Wahrscheinlichkeiten für einzelne Klassen:
par(mfrow=c(2,2))
for(i in 1:4) {
  matplot(0:9,cbind(dpSES0[i,],dpSES1[i,]),xlab='LE',pch="01",
          type="b",ylim=c(0,1),main=paste('P[Y =',i-1,']'),ylab="")
  if(i==1) legend(6,0.95,c("SES = 1","SES = 0"),col=c("red","black"),
                  pch=c("1","0"))#,text.col=c("red","black"))
}




# Serie 10, Aufgabe 2 -----------------------------------------------------

# Multinomiales Logit -----------------------------------------------------
# library(nnet)

rm(list=ls())
dev.off()
source("ftp://stat.ethz.ch/NDK/Source-WBL-4/03.RCodes/umwelt.R")
str(t.d)

# r.mnl.s171 <- multinom(Hauptv~Alter + Schule + Beeintr + Geschlecht, data=t.d)
# summary(r.mnl.s171)

# Full: full <- multinom(Hauptv~., data=t.d)
# drop1 funktioniert nicht per default: zuerst global:
# trace <- TRUE
# drop1(full, test="Chisq")
# drop1(full)

# library(MASS)
# stepAIC(full)

str(d.umwelt)
str(t.d)
levels(t.d$Schule)

t.rm <- multinom(Hauptv~Schule + Beeintr, data=t.d)
summary(t.rm)

# Modellselektion

?drop1

t.rm <- multinom(Hauptv ~ Alter + Geschlecht + Schule + Wohnlage + Ortsgroesse + Partei + Beeintr, data=t.d)
trace <- TRUE
drop1(t.rm)
t.rm1 <- update(t.rm,~.-Partei)
drop1(t.rm1)
t.rm2 <- update(t.rm1,~.-Wohnlage)
drop1(t.rm2)
t.rm3 <- update(t.rm2,~.-Alter)
drop1(t.rm3)

## oder mit der Funktion stepAIC aus dem Package MASS
require(MASS)
stepAIC(t.rm)




rm(list=ls())
dev.off()


# Serie 11, Aufgabe 1 -----------------------------------------------------

d.kev<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/kevlar49.dat", header=T)

str(d.kev)

levels(d.kev$Spool)
d.kev$Spool <- as.factor(d.kev$Spool)

?subset
d.kev <- subset(d.kev, d.kev$Stress >= 24)

source("http://stat.ethz.ch/~stahel/regression/regr.R")
library(regr0)
r1 <- regr(log(Failure)~Stress + Spool, data=d.kev)

summary(r1)
termplot(r1, partial=T, rug=T)

library(sfsmisc)
TA.plot(r1)
plot(r1)



# Überlebenszeiten - survival ---------------------------------------------

library(survival)
?Surv
str(d.kev)

s.surv <- survreg(Surv(Failure, rep(1, nrow(d.kev)))~Stress + Spool, data=d.kev)
summary(s.surv)
# Survival mit regr -------------------------------------------------------

# zuerst attachen

attach(d.kev)
nrow(d.kev)

s.regr <- regr(Surv(Failure, rep(1, 87))~Stress + Spool, family="weibull", data=d.kev)

summary(s.regr)
plot(s.regr)


# Aufgabe 2 ---------------------------------------------------------------

rm(list=ls())
dev.off()

d.kaes<- read.csv("http://stat.ethz.ch/Teaching/Datasets/WBL/Kaese.csv", header=T, sep="\t")

str(d.kaes)

d.kaes$Temp <- as.factor(d.kaes$Temp)

plot.design(d.kaes)


# Tobit-Regression --------------------------------------------------------

# Datensatz muss attach sein, sonst funktionierts nicht mit regr...


t.regr <- regr(Tobit(Anzahl, limit=1, log=TRUE)~., data=d.kaes)


summary(t.regr)

plot(t.regr)

coef(t.regr)
