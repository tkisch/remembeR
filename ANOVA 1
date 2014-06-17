# R-Code-ANOVA-1 ----------------------------------------------------------

# Modellschreibweisen

model <- aov(y~-1)                    # Nullmodel
model <- aov(y~1)                     # nur Intercept
model <- aov(y~x1)                    # Intercept und x1
model <- aov(y~x1 + x2)               # Intercept und x1 und x2
model <- aov(y~x1 + x2 + x1:x2)       # Intercept und x1 und x2 und Interaktion x1 und x2
model <- aov(y~x1 + x2%in%x1)         # Intercept und x1 und in x2 geschachtelter Faktor x1, keine Interaktion möglich
model <- aov(y~Error(x1 + x2%in%x1))  # Zufällige Effekte


# read and screen data, defined as factors

d.stream <- read.table("http://stat.ethz.ch/Teaching/Datasets/WBL/stream.dat", header=T)

str(d.stream)
head(d.stream)

# plot

plot(DIVERSITY~as.numeric(ZNGROUP), data = d.stream)

plot(YIELD~jitter(as.numeric(d.hafer$GROUP), factor= 0.2), data=d.hafer, type="n") # type="n" -> keine marker
text(jitter(as.numeric(d.hafer$GROUP), factor= 0.2), d.hafer$YIELD, labels=d.hafer$REP)

# define factors

d.stream$ZNGROUP <- as.factor(d.stream$ZNGROUP)
d.dung$DUENGER <- ordered(d.dung$DUENGER)

# Modell rechnen

aov1b <- aov(DIVERSITY~ZNGROUP, data=d.stream)

# Tukey-Anscombe Plot und QQ-PLot

plot(aov1b, which=c(1:2))

# summary (x,...) = anova (x, ...)
summary(aov1b)
anova(aov1b)

summary.lm(aov1b)
dummy.coef(aov1b)

model.matrix(aov1b)

# interaction.plot(x.factor, trace.factor, response, fun = mean,...)

interaction.plot(d.stream$ZNGROUP, d.stream$STREAM, d.stream$DIVERSITY)

# Nebenbedingungen definieren

# function C() definiert auf welche Art die Faktoren als Kontrast definiert werden sollen
# mit d.hafer$GROUP <- as.factor(d.hafer$GROUP) werden automatisch "contr.treatment" definiert

d.hafer$GROUPct <- C(as.factor(d.hafer$GROUP), contr=treatment)
d.hafer$GROUPcs <- C(as.factor(d.hafer$GROUP), contr=sum)
d.hafer$GROUPcS <- C(as.factor(d.hafer$GROUP), contr=SAS)

# Modellvergleiche und Modellreduktion

a.FULL <- aov(YIELD~GROUP, data=d.hafer)
a.NURINTERCEPT <- aov(YIELD~1, data=d.hafer) 
a.NULL <- aov(YIELD~-1, data=d.hafer)

anova(a.FULL)
anova(a.NULL, a.NURINTERCEPT, a.full)

# subset data

a1 <- aov(KMP4L~as.factor(AUTO), data=subset(d.auto, d.auto$STADT=="Los Angeles"))
a2 <- aov(KMP4L~as.factor(AUTO), data=subset(d.auto, d.auto$STADT=="Portland"))
a3 <- aov(KMP4L~as.factor(AUTO), data=subset(d.auto, d.auto$STADT=="San Francisco"))

# various other Types of Sum of Squares

library(car)

Anova(a12, type="III")
Anova(a22, type="III")

# pairwise comparison

pairwise.t.test(d.blut$BLUTZUCK, d.blut$DIAET, p.adjust.method="none") # Fisher
pairwise.t.test(d.blut$BLUTZUCK, d.blut$DIAET, p.adjust.method="bonf") # Bonferroni

# Plot Intervals 
plot(TukeyHSD(va1, order=F))

# Kontraste definieren

lent.contr <- cbind(c(-6,1,1,1,1,1,1),
                    c(0,-1,-1,-1,1,1,1),
                    c(0,2,-1,-1,2,-1,-1),
                    c(0,0,-1,1,0,-1,1),
                    c(0,-2,1,1,2,-1,-1),
                    c(0,0,1,-1,0,-1,1))

contrasts(d.lent$TR) <- lent.contr
d.lent$TR <- C(d.lent$TR, lent.contr)

# Modell rechnen und Output bestimmen

c.lent <- aov(Y~TR + BLOCK, data = d.lent)

summary(c.lent, split= list(TR=list(L1=1, L2=2, L3=3, L4=4, L5=5, L6=6)))

# geordnete Faktoren - othogonale polynomiale Kontraste

d.dung$DUENGER <- ordered(d.dung$DUENGER)

# Modell rechnen und Output bestimmen

o.aov <- aov(log10(ERTRAG)~DUENGER + SORTE, data=d.dung)

summary(o.aov, split=list(DUENGER = list(linear = 1, Quadratisch = 2, Kubisch = 3)))

# Zufällige Effekte
# Zufällige Effekte mit Error... und angeben, dass sie geschachtelt sind...

a1 <- aov(MOISTURE~Error(BATCH + PROBE%in%BATCH, data=d.paint))

library(nlme)

x1/x2 Kurzschreibweise für y~x1 + x2%in%x1.
mi1 <- lme(MOISTURE~1, data=d.paint, random = ~1 | BATCH/PROBE)

intervals(mi1, which="var-cov")
plot(mi1)

# aggregieren

d.gummi.ag <- aggregate(CURE~SERIE+TEMP+LAB, data=d.gummi, FUN = mean)
