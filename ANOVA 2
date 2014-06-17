# Serie 7, Aufgabe 1 ------------------------------------------------------

# Serie 7, Aufgabe 2 ------------------------------------------------------

d.legi<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/legierung.dat", header=T)
str(d.legi)

# für plot.design() -------------------------------------------------------
# immer zuerst Faktoren generieren ----------------------------------------

d.legi$day <- as.factor(d.legi$day)
d.legi$alloy <- as.factor(d.legi$alloy)
d.legi$temp <- as.factor(d.legi$temp)

# plot.design - zuerst Faktoren generieren --------------------------------

plot.design(d.legi)

# interaction.plot --------------------------------------------------------

par(mfrow=c(2,2))
interaction.plot(d.legi$day, d.legi$alloy, d.legi$breaking)
interaction.plot(d.legi$temp, d.legi$alloy, d.legi$breaking)
interaction.plot(d.legi$day, d.legi$temp, d.legi$breaking)

# Modelle rechnen ---------------------------------------------------------

summary(d.legi)

# day = block
# temp = main unit
# alloy = sub unit

m.rcb <- aov(breaking ~ day + temp*alloy + day:temp, data=d.legi)

m.spalt <- aov(breaking ~ day + temp*alloy + Error(day:temp), data=d.legi)


summary(m.rcb)
summary(m.spalt)

anova(m.rcb)
#anova(m.spalt)

s.rcb <- summary(m.rcb)
str(s.rcb)
s.rcb[[1]]$"Mean Sq"

# Funktioniert so besser!
a.rcb <- anova(m.rcb)
str(a.rcb)
a.rcb$"Mean Sq"[2]

# Korrektur um richtigen MSe, ACHTUNG: die richtigen Auswählen

a.rcb$"Mean Sq"[2]/a.rcb$"Mean Sq"[5]

# Aufgabe f, package (nlme) -----------------------------------------------

library(nlme)

r.lme <- lme(breaking ~ temp * alloy, data= d.legi, random = ~1 | day/temp)

anova(r.lme)
summary(r.lme)

m.spalt2 <- aov(breaking ~ temp*alloy + Error(day + day:temp), data=d.legi)

summary(m.spalt2)

str(d.legi)

View(d.legi)

# Variable erzeugen, um Main Units zu identifizieren, damit Block Faktor als fix betrachtet werden kann
d.legi$munit <- rep(1:12, each=3)

r.lme2 <- lme(breaking ~ day + temp * alloy, data= d.legi, random = ~1 | munit)
anova(r.lme2)

m.spalt <- aov(breaking ~ day + temp*alloy + Error(day:temp), data=d.legi)
summary(m.spalt)


# Serie 8, Aufgabe 1 ------------------------------------------------------
dev.off()
rm(list=ls())

d.k<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/klaerschlammIntens.dat", header=T)
str(d.k)

plot(d.k, pch=19, col=as.numeric(d.k$STADT))
pairs(d.k, pch=19, col=as.numeric(d.k$STADT))


# Plot mit abline pro Stadt erstellen -------------------------------------

r.full <- lm(ZINK~INT, data=d.k)

r.A <- lm(formula(r.full), data=subset(d.k, d.k$STADT=="A"))
r.B <- lm(formula(r.full), data=subset(d.k, d.k$STADT=="B"))
r.C <- lm(formula(r.full), data=subset(d.k, d.k$STADT=="C"))


plot(ZINK~INT, data=d.k, pch=19, col=as.numeric(d.k$STADT), xlim=c(0,1.7), ylim=c(-2,85))
abline(r.A, col=1)
abline(r.B, col=2)
abline(r.C, col=3)
abline(r.full)


# Modelle ANCOVA berechnen ------------------------------------------------

# 2 Modelle, "normal" und "zentriert" -------------------------------------

#um gleiche Resultate wie im Skript zu erhalten, Kontraste anpassen
str(d.k)
contrasts(d.k$STADT)

options("contrasts")
options(contrasts=c("contr.SAS","contr.poly"))

# für zentriertes Modell x-Var anpassen (Mittelwert abziehen)

d.k$INT.Z <- d.k$INT-mean(d.k$INT)

str(d.k)
View(d.k)


cov1 <- aov(ZINK~STADT+INT + STADT*INT, data=d.k)
anova(cov1)

dummy.coef(cov1)


cov1z <- aov(ZINK~STADT+INT.Z + STADT*INT.Z, data=d.k)
summary(cov1z)
anova(cov1z)

dummy.coef(cov1z)

# Aufgabe c: Modellvergleiche, SS Type

# üblicherweise werden Interaktionseffekte erst nach den Haupteffekten ins Modell genommen
# wie möchten aber nun ein volles Modell mit einem Modellvergleiche, 
# in dem nur Haupteffekte weggelassen werden
# ---

# für die Berechnung der SS ist Reihenfolge allenfalls relevant (je nach Typ)

d.k$s1 <- ifelse(d.k$STADT=="A", 1, 0)
d.k$s2 <- ifelse(d.k$STADT=="B", 1, 0)

# nun Variablen für Interaktionen erzeugen
# unzentriert
d.k$s1INT <- d.k$s1*d.k$INT
d.k$s2INT <- d.k$s2*d.k$INT
# zentriert
d.k$s1INT.Z <- d.k$s1*d.k$INT.Z
d.k$s2INT.Z <- d.k$s2*d.k$INT.Z


rm(list=ls())
dev.off()
# Serie 9, Aufgabe 1 ------------------------------------------------------

d.filtrat<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/filtrat.dat", header=T)

str(d.filtrat)

# Variablen als Faktoren definieren, ACHTUNG: hier nicht erwünscht... Probleme bei Pareto Chart... ??

# gegeben ist nur die Filtrationsmenge (Y), und welche Versuchsbedingung eingesetzt wurde...

d.filtrat$A <- as.factor(d.filtrat$A)
d.filtrat$B <- as.factor(d.filtrat$B)
d.filtrat$C <- as.factor(d.filtrat$C)  
d.filtrat$D <- as.factor(d.filtrat$D)   


# full Modell

r.full <- lm(rate~A*B*C*D, data=d.filtrat)
summary(r.full)
anova(r.full)


# Pareto Chart erstellen --------------------------------------------------

NROW(r.full$coefficients)
# 16 Koeffizienten, den Intercept interessiert uns aber nicht

t.effekte <- 2*r.full$coefficients[2:16]
t.x <- sort(abs(t.effekte)) # absolute Effekte, sortiert

par(las=1) # Beschriftungsart der y-Achse
palette(heat.colors(15))

barplot(t.x, names.arg=names(t.x), space=0.8, horiz=T, main="Pareto chart", xlab="abs(Effekt)", col=1:15)
palette("default")
par(las=0)



# Half Normal Plot erstellen ----------------------------------------------

?ppoints
t.p
t.y
t.p <- 0.5 + ppoints(length(t.x))/2
t.y <- qnorm(t.p) # theoretische Quantile
plot(t.x, t.y, xlab="absoluter Effekt", ylab="theoretische Quantile", main="Half-normal plot")
text(t.x, t.y, labels=names(t.x), pos=4, cex=0.7)
abline(a=0,b=median(t.y)/median(t.x)) # Gerade im Halfnormalplot


# plot design erfordert Faktoren

d.filtrat$A <- as.factor(d.filtrat$A)
d.filtrat$B <- as.factor(d.filtrat$B)
d.filtrat$C <- as.factor(d.filtrat$C)  
d.filtrat$D <- as.factor(d.filtrat$D)

# Plot Design
plot.design(formula(r.full), data=d.filtrat)
plot.design(d.filtrat)

# Interaction Plot
dev.off()
par(mfrow=c(3,2))
interaction.plot(d.filtrat$A,d.filtrat$B,d.filtrat$rate)
interaction.plot(d.filtrat$A,d.filtrat$C,d.filtrat$rate)
interaction.plot(d.filtrat$A,d.filtrat$D,d.filtrat$rate)
interaction.plot(d.filtrat$B,d.filtrat$C,d.filtrat$rate)
interaction.plot(d.filtrat$B,d.filtrat$D,d.filtrat$rate)
interaction.plot(d.filtrat$C,d.filtrat$D,d.filtrat$rate)


rm(list=ls())



# Aufgabe 1.e, Modelle mit wichtigsten Faktoren ---------------------------

# aov -> Faktoren bilden
str(d.filtrat)

m1 <- aov(rate~A + D + C + A:C + A:D, data= d.filtrat)

anova(m1)
summary(m1)

model.tables(m1, type="means")
dummy.coef(m1,  type="means")

par(mfrow=c(2,2))
plot(m1, which=c(1:4))


rm(list=ls())
dev.off()
# Serie 10, Aufgabe 1 ------------------------------------------------------

d.ebene <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/webene.dat", header=T)
d.flaeche <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/wflaeche.dat", header=T)

d.e <- d.ebene
d.f <- d.flaeche

str(d.e)
str(d.f)

plot(d.e)
plot(d.f)

# Wirkungsfläche: 1. Ordnung, ohne Interaktion

ebene <- lm(y~x1+x2, data=d.e)

# oder S.123
# ebene <- lm(y~x1*x2, data=d.e)

anova(ebene)
summary(ebene)


# Vergleich: Residuenvarianz und Varianz der Zentrumspunkte
sumeb <- summary(ebene)
str(sumeb)

sumeb$sigma^2
var(d.e$y[c(5:9)])

# Residuenanalyse
par(mfrow=c(2,2))
plot(ebene)


# Test, ob Ebene gut ist, oder Krümmung vorliegt --------------------------

# +/- Vergleich Zentrumspunkte mit Treatments...
str(d.e) # Zentrumspunkte 5:9

# Mittelwertdifferenz berechnen
diff <- mean(d.e$y[c(5:9)]) - mean(d.e$y[c(1:4)])

# Intervall berechnen

up <- diff + qt(0.975, 5-1) * sqrt(var(d.e$y[c(5:9)])*(1/5 + 1/4))
lo <- diff - qt(0.975, 5-1) * sqrt(var(d.e$y[c(5:9)])*(1/5 + 1/4))

lo;up

# Null liegt nicht in diesem Intervall... Unterschied ist signifikant


# Serie 10, Aufgabe 1.b ---------------------------------------------------

# Wirkungsfläche 2. Ordnung

str(d.f)

poly <- lm(y~x1 + I(x1^2) + x2 + I(x2^2) + x1:x2, data=d.f)

sum.ebene <- summary(ebene)
sum.poly <- summary(poly)

str(sum.ebene)

(sum.ebene$sigma)^2; var(d.e$y[c(5:9)])
(sum.poly$sigma)^2; var(d.e$y[c(5:9)])

par(mfrow=c(2,2))
plot(poly, which=c(1:2))
plot(ebene, which=c(1:2))


# Wirkungsfläche und Konturlinien zeichnen

t.x1 <- seq(-2, 2, by=0.1)
t.x2 <- t.x1

f.yWert <- function(x1, x2){
  predict(poly, newdata=data.frame(x1, x2))
}


t.y <- outer(t.x1, t.x2, f.yWert)
dev.off()
par(bg="white")
persp(t.x1, t.x2, t.y, theta=40, phi=10, expand=0.5, col="lightblue", xlab="x1", ylab="x2", zlab="Y", ticktype="detailed")

# t.x1, t.x2, t.y

persp(t.x1, t.x2, t.y)
contour(t.x1, t.x2, t.y)
filled.contour(t.x1, t.x2, t.y)


# Maximum berechnen -------------------------------------------------------

poly$coef

# Wirkungsfläche 2. Ordnung nach x1, x2 ableiten und Null setzen
# Matrizen erstellen

a <- matrix(c(2*poly$coef[3], poly$coef[6], poly$coef[6], 2*poly$coef[5]), byrow=T, nrow=2)
b <- matrix(-poly$coef[c(2,4)], byrow=T, nrow=2)

# mit solve lösen
solve(a, b)


# Serie 10, Aufgabe 2 -----------------------------------------------------

rm(list=ls())
dev.off()
d.pet <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/peter182.dat", header=T, sep=";")

View(d.pet)

# Berechnen eines Modell mit den fünf HALBIERTEN Haupteffekten und den 10 HALBIERTEN Interaktionseffekten 1. Ordnung

r.full <- lm(Y~(A+B+C+D+E)^2, data=d.pet)

summary(r.full)
plot(r.full, which=1)

# Effekte in halbiertem 2^k Versuch s^k-1 sind ebenfalls halbiert.. ?!!! vermengt mit jeweiligen Interaktionen...

2*(r.full$coef[-1])

cof <- 2*(r.full$coef[-1])

# Pareto chart
s.cof <- sort(abs(cof))  # Achtung: hier bereits verdoppelt! vgl. gleich oben
par(las=1)
barplot(s.cof, horiz=T)

# Half Normal Plot

?ppoints
t.p <- 0.5+ppoints(length(s.cof))/2
t.y <- qnorm(t.p)

plot(s.cof, t.y)  # s.cof, sorted absolut effects from above
text(s.cof, t.y, labels=names(s.cof), pos=4, cex=0.5)
abline(a=0, b=median(t.y)/median(s.cof))


# Serie 10, Aufgabe 2.b ---------------------------------------------------

r.red <- lm(Y~(A+B+C+D+E)^2-(A:C+A:D+A:E+B:C+B:D+B:E), data=d.pet)
summary(r.red)
dev.off()
par(mfrow=c(2,2))
plot(r.red, which=c(1:3))

# Koeffizienten Vergleichen

nn <- paste(variable.names(r.red))          # Names der Variablen in reduziertem Modell

dummy.coef(r.red)[nn]
dummy.coef(r.full)[nn]


aov.red <- aov(formula(r.red), data=d.pet)

anova(aov.red)

dummy.coef(aov.red)
dev.off()



# Serie 11, Aufgabe 1 -----------------------------------------------------
rm(list=ls())
dev.off()
# d.k<- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/klaerschlammIntens.dat", header=T)

# BLOCK 1 - 5
# Faktoren
#     B: (-1, 1)
#     C: (-1, 0, 1)

dd <- expand.grid(BLOCK = c(1,2,3,4,5), B = c(-1, 1), C = c(-1, 0, 1))
?expand.grid
View(dd)

dd$y <- rnorm(30, 10,1)

# Achtung zuerst als Faktoren definieren!!!

dd$BLOCK <- as.factor(dd$BLOCK)
dd$B <- as.factor(dd$B)
dd$C <- as.factor(dd$C)


model <- aov(y~BLOCK + B + C + B:C, data=dd)

summary(model)


# Aufgabe 2: : Sample Size "Vergleich Mittelwerten" -----------------------


?power.t.test

power.t.test(n = NULL, delta = 0.5, sd = 2, sig.level = 0.01, power = 0.8, type = c("two.sample"), alternative = c("one.sided"))

# Mit Formel Seite 122-127

z.alpha <- qnorm((1-0.01))
z.beta <- qnorm((0.8))

ceiling(2*(z.alpha + z.beta)^2*(2/0.5)^2)

2*(((z.alpha + z.beta)^2)*(2/0.5)^2)


# Aufgabe 3: Sample Size "Vergleich von Anteilen" -------------------------

# p1 = 0.05, p2 = 0.15, alpha = 0.05, beta = 0.1

?power.prop.test

power.prop.test(n = NULL, p1 = 0.05, p2 = 0.15, sig.level = 0.05, power = 0.9, alternativ = c("one.sided"))

# Manuell

# p1 <- 0.05; p2  <- 0.15

anteile <- function(p1, p2){
  q1 <- 1-p1; q2 <- 1-p2
  
  p.hat <- (p1 + p2)/2
  q.hat <- 1-p.hat
  
  (((qnorm(1-0.05)*sqrt(2*(p.hat*q.hat)) +
       qnorm(1-0.1)*sqrt(p1*q1 + p2*q2))^2)/(abs(p1-p2)^2) + 
     (2/abs(p1-p2)))
  
}

anteile(0.35, 0.45)


# Serie 11, Aufgabe 4 -----------------------------------------------------

# from series 1, 3

# plotting distribution curves etc.
