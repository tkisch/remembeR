---
title: "SP Finanzen - Vorabauswertungen Fundraising"
output:
  html_document:
    number_sections: yes
    theme: spacelab
    toc: yes
---


# Einleitung

Im Rahmen des Strategieprojekt _Finanzen_ werden in einem ersten Schritt unter anderem die Daten aus dem Fundraising untersucht. In diesem Dokument sind erste deskriptive Auswertungen anhand der Daten aus _Lobos_ gemacht.

## Summary Statistics

### raw r

```{r}
summary(cars)
```

### pander

```{r sumtable, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'}
require(pander)
set.caption("Summary Statistic on Fundraising")
pander(summary(cars), style = 'rmarkdown')
```

check also panderOptions()


### xtable

```{r xtable1, comment=NA, results='asis'}
library(xtable)
data(iris)
print(xtable(summary(iris)), type = "html", include.rownames = F)
```
?xtable
You can also embed plots, for example:


## plots n'diagrams

```{r chname, echo=FALSE}
plot(cars)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

# choropleth map: without 'echo = FALSE'

```{r geoplot_setup, cache=TRUE}
library(plyr)    # for join(...)
library(rgdal)   # for readOGR(...)
library(ggplot2)

setwd("C:/Users/Tobi/Documents/a1_workfiles/Wagerenhof/geodata/webexample")
votes <- read.csv("vote.csv", sep=";")

votes$Yes.Pct <- votes$Yes.Pct*sample(1:1000,1)
map   <- readOGR(dsn=".",layer="VEC200_Commune")
map   <- map[map$COUNTRY=="CH",]   # extract just Swiss Gemeinde
data <- data.frame(id=rownames(map@data), 
                   GEMNAME=map@data$GEMNAME,
                   BFSNR=map@data$BFSNR)
# convert id to char from factor
data$id <- as.character(data$id)
# merge vote data based on Gemeinden (different columns names in the two df...)
data    <- merge(data,votes,by.x="BFSNR",by.y="Gemeinde.Nr.", all.x=T)

map.df <- fortify(map)
map.df <- join(map.df,data,by="id")
```

Mit
```
, cache=TRUE
```
kann code "gespeichert" werden.  
Das heist aber das ich
```
library(ggplot2)
```
unten (beim plot) wieder laden muss!


hier kommt der plot

```{r gplot, out.width = '\\maxwidth'}
library(ggplot2)


ggplot(map.df, aes(long,lat, group=group))+
  geom_polygon(aes(fill=Yes.Pct))+
  coord_fixed()+
  scale_fill_gradient(low = "#460BF8", high = "#F8F40B", 
                      space = "Lab", na.value = "grey80",
                      guide = "colourbar")+
  labs(title="Spendenvolumen auf Gemeindeebene", x="", y="")+
  theme(axis.text=element_blank(),axis.ticks=element_blank())
```

Hats funtioniert? plot kommt, anpassung der grösse unklar  
```
fig.width=12, fig.height=10
```
funktioniert nicht. Falsch: funktioniert. Mit:
```
, out.width = '\\maxwidth'
```




