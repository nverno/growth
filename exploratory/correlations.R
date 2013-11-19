## Moosilauke
## Visualize plot level statistics and how they are correlated to asp/elevation
library(plyr)
library(lattice)
library(ggplot2)
library(grid)
library(spatstat)
source("~/work/functions/functions-plotting.R")
long <- read.csv("~/work/data/data/growth/moose-long.csv")

## change times
long$time <- apply(long, 1, function(x) {
    if (as.numeric(x[["time"]]) == 86) { 1986 }
    else if (as.numeric(x[["time"]]) == 87) { 1987 }
    else if (as.numeric(x[["time"]]) == 98) { 1998 }
    else if (as.numeric(x[["time"]]) == 10) { 2010 }
    else { NA }
})

## complete cases
comp <- long[complete.cases(long[,c("htgrowth","dbhgrowth","elev")]),]

###############################################################################
## get some plot level stats
## - plot density
## - average distance between trees

################
## plot density
dens <- ddply(long,  .(pplot, time), .fun = function(x) {
    data.frame(id = x$id, plotden = nrow(subset(x, stat = "ALIVE" & dbh > 5)))
})

dens <- dens[dens$pplot > 3,]
dens$time <- dens$yrs
## ggplot(dens, aes(yrs, plotden, group = pplot, color = yrs)) + geom_line(arrow = arrow())
## ggsave(file = "~/work/growth/visuals/plotden-byyear.pdf")

######################
## neighbor counts
ncounts <- ddply(long, .(pplot, time), .fun = function(x) {
    x <- droplevels(x)
    x <- x[!is.na(x$bqudy) & x$stat == "ALIVE" & !is.na(x$bqudy) & !is.na(x$dbh),]
    if (nrow(x) > 0) {
        numnebs <- apply(x, 1, function(y) {
            dbh <- as.numeric(y[["dbh"]])
            bqudy <- as.numeric(y[["bqudy"]])
            bqudx <- as.numeric(y[["bqudx"]])
            num <- nrow(x[!is.na(sqrt(abs(x$bqudy - bqudy)^2 + abs(x$bqudx - bqudx)^2) < 3)
                          & sqrt(abs(x$bqudy - bqudy)^2 + abs(x$bqudx - bqudx)^2) < 3
                          & x$dbh >= dbh,])
        })
        data.frame(id = x$id, numnebs = numnebs)
    }
    else data.frame(id = NA, numnebs = NA)
})

tst <- merge(long, ncounts, by = c("pplot","id","time"), all.x = TRUE)
tst <- merge(tst, dens,by = c("pplot","id","time"), all.x = TRUE)

###############################################################################
##
## paired
##

## comp$elevcl = factor(x,levels(x)[c(4,5,1:3)])
comp <- tst[complete.cases(tst[,c("dbhgrowth","htgrowth","numnebs")]),]
pairs(~ht + htgrowth + dbh + dbhgrowth + asp + elev + plotden + numnebs,
      data = comp, lower.panel = panel.smooth,
      upper.panel = panel.cor)
dev.copy2pdf(file = "~/work/growth/visuals/pairs-dbh-ht-growths-elev-asp-nebs-plotden.pdf")
dev.off()






