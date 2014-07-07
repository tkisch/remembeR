
# boxplots with large outliers, ruining the plot --------------------------

y = c(.01,.02,.03,.04,.05,.06,.07,.08,.09,.5,-.6)
x = c(1,1,1,1,1,1,1,1,1,1,1)
qplot( factor( x ) , y , geom="boxplot" )

dev.off()
# solutions one -----------------------------------------------------------

# create a dummy data frame with outliers
df = data.frame(y = c(-100, rnorm(100), 100))

# create boxplot that includes outliers
p0 = ggplot(df, aes(y = y)) + geom_boxplot(aes(x = factor(1)))


# compute lower and upper whiskers
ylim1 = boxplot.stats(df$y)$stats[c(1, 5)]

# scale y limits based on ylim1
p1 = p0 + coord_cartesian(ylim = ylim1*1.05)
p1
p0

# various ggplots side-by-side

require(gridExtra)
grid.arrange(p0, p1, ncol=2)


# solution 2 --------------------------------------------------------------

n <- 1e4L
dfr <- data.frame(
  y = exp(rlnorm(n)),  #really right-skewed variable
  f = gl(2, n / 2)
)

p <- ggplot(dfr, aes(f, y)) + 
  geom_boxplot()
p   # big outlier causes quartiles to look too slim

p2 <- ggplot(dfr, aes(f, y)) + 
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(limits = quantile(dfr$y, c(0.1, 0.9)))
p2  # no outliers plotted, range shifted


# solution 3 --------------------------------------------------------------

rm(list=ls())
dev.off()
# precompute values
x <- c(1:100, 1000)
?boxplot.stats
boxplot.stats(x)$stats
stats <- boxplot.stats(x)$stats
df <- data.frame(x="label1", ymin=stats[1], lower=stats[2], middle=stats[3], upper=stats[4], ymax=stats[5])

# create plot
p <- ggplot(df, aes(x=x, lower=lower, upper=upper, middle=middle, ymin=ymin, ymax=ymax)) + 
  geom_boxplot(stat="identity")
p

