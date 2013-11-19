## Moosilauke
## Visualize growth/species by environmental factors:
## - elevation
## - aspect
library(plyr)
library(lattice)
long <- read.csv("~/work/data/data/growth/moose-long.csv")

## See what our variables look like
hist(long$dbh)
hist(long$ht)
hist(long$dbhgrowth)
hist(long$bvgrowth)
hist(long$htgrowth)

## Singular value decomposition (principal component analysis)
## nums <- sapply(long, is.numeric)
## mat <- as.matrix(long[,nums])
comp <- long[complete.cases(long[,c("htgrowth","dbhgrowth","elev")]),
             c("htgrowth","dbhgrowth","elev")]
mat <- as.matrix(scale(comp))

## look for pattern in rows/columns
par(mar = rep(0.2, 4))
image(1:3, 1:nrow(mat), t(mat)[,nrow(mat):1])

###########################################################################
##
## Clusters and heatmaps
##

## quantile classes for dbh/ht/bv growth
long$dbhcl <- as.numeric(cut(long$dbhgrowth, breaks = quantile(long$dbhgrowth,
                                             probs = seq(0,1,0.1), na.rm = TRUE)))
long$htcl <- as.numeric(cut(long$htgrowth, breaks = quantile(long$htgrowth,
                                           probs = seq(0,1,0.15), na.rm = TRUE)))
long$bvcl <- as.numeric(cut(long$bvgrowth, breaks = quantile(long$bvgrowth,
                                           probs = seq(0,1,0.15), na.rm = TRUE)))
## numeric asp/elev classes
long$aspcl <- as.numeric(long$aspcl)
long$elevcl <- as.numeric(long$elevcl)
long$soilcl <- as.numeric(long$soilcl)

## by correlation
comp <- long[complete.cases(long[,c("aspcl","elevcl","soilcl","htcl","dbhcl")]),
             c("aspcl","elevcl","soilcl","htcl","dbhcl")]
mat <- as.matrix(comp)
symnum( cU <- cor(mat) )
hU <- heatmap(cU, Rowv = FALSE, symm = TRUE,
              distfun = function(c) as.dist(1 - c), keep.dendro = TRUE)
dev.copy2pdf(file = "~/work/growth/visuals/heatmap-correlations.pdf")
dev.off()

## htgrowth and dbhgrowth
comp <- long[complete.cases(long[,c("htgrowth","dbhgrowth","htcl","dbhcl")]),
             c("htgrowth","dbhgrowth","htcl","dbhcl")]
mat <- as.matrix(comp)
heatmap(mat, labCol = c("ht grwth", "dbh grwth", "htcl","dbhcl"),
        main = "Clustering height and dbh growth", cexCol = 1.5, labRow = FALSE,
        scale = "column")
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap.pdf")
dev.off()

##########################################################################
## ht and dbh growth by elevation
comp <- long[complete.cases(long[,c("htcl","dbhcl","elevcl")]),
             c("htcl","dbhcl","elevcl")]
mat <- as.matrix(scale(comp))
heatmap(mat, labCol = c("htcl","dbhcl","elevcl"),
        main = "Clustering height and dbh growth by elevation",
        cexCol = 1.5, labRow = FALSE)
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap-elev.pdf")
dev.off()

## plotmeans
comp <- long[complete.cases(long[,c("htgrowth","dbhgrowth","elev")]),
             c("htgrowth","dbhgrowth","elev")]
byelev <- ddply(comp, .(elev), .fun = function(x) {
    data.frame(dbhgr = mean(x$dbhgrowth, na.rm=TRUE),
               htgr = mean(x$htgrowth, na.rm = TRUE))
})
rownames(byelev) = byelev$elev
byelev <- byelev[,2:3]
mat <- as.matrix(byelev)
rc <- rainbow(nrow(byelev), start = 0, end = .3)
heatmap(mat, labCol = c("ht","dbh"),
        main = "Clustering mean height and dbh growth by elevect",
        cexCol = 1.5, scale = "column", RowSideColors = rc)
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap-elevmeans.pdf")
dev.off()

##########################################################################3
## ht and dbh growth by aspect
comp <- long[complete.cases(long[,c("htcl","dbhcl","aspcl")]),
             c("htcl","dbhcl","aspcl")]
mat <- as.matrix(comp)
heatmap(mat, labCol = c("htcl","dbhcl","aspcl"),
        main = "Clustering height and dbh growth by aspect",
        cexCol = 1.5, labRow = FALSE, scale = "column")
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap-aspect.pdf")
dev.off()

## plotmeans
comp <- long[complete.cases(long[,c("htgrowth","dbhgrowth","asp")]),
             c("htgrowth","dbhgrowth","asp")]
byasp <- ddply(comp, .(asp), .fun = function(x) {
    data.frame(dbhgr = mean(x$dbhgrowth, na.rm=TRUE),
               htgr = mean(x$htgrowth, na.rm = TRUE))
})
rownames(byasp) = byasp$asp
byasp <- byasp[,2:3]
mat <- as.matrix(byasp)
rc <- rainbow(nrow(byasp), start = 0, end = .3)
heatmap(mat, labCol = c("ht","dbh"),
        main = "Clustering mean height and dbh growth by aspect",
        cexCol = 1.5, scale = "column", RowSideColors = rc)
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap-aspmeans.pdf")
dev.off()

##########################################################################
## ht and dbh growth by plot
comp <- long[complete.cases(long[,c("htcl","dbhcl","pplot")]),
             c("htcl","dbhcl","pplot")]
mat <- as.matrix(comp)
heatmap(mat, labCol = c("htcl","dbhcl","pplot"),
        main = "Clustering height and dbh growth by plot",
        cexCol = 1.5, labRow = FALSE, scale = "column")
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap-plot.pdf")
dev.off()

## plotmeans
comp <- long[complete.cases(long[,c("htgrowth","dbhgrowth","pplot")]),
             c("htgrowth","dbhgrowth","pplot")]
byplot <- ddply(comp, .(pplot), .fun = function(x) {
    data.frame(dbhgr = mean(x$dbhgrowth, na.rm=TRUE),
               htgr = mean(x$htgrowth, na.rm = TRUE))
})
rownames(byplot) = byplot$pplot
byplot <- byplot[,2:3]
mat <- as.matrix(byplot)
rc <- rainbow(nrow(byplot), start = 0, end = .3)
heatmap(mat, labCol = c("ht","dbh"),
        main = "Clustering height and dbh growth by plot",
        cexCol = 1.5, scale = "column", RowSideColors = rc)
dev.copy2pdf(file="~/work/growth/visuals/ht_dbh-growth-heatmap-plotmeans.pdf")
dev.off()

###########################################################################
##
## Correlations between ht and dbh
##
##############################################################################
##
## Ht vs DBH (NOT growth)
##

## General trends by gradients between ht and dbh (not growth)
comp <- long[complete.cases(long[,c("ht","dbh","aspcl","elevcl")]),
             c("ht","dbh","aspcl","elevcl")]
ggplot(comp, aes(dbh, ht)) + geom_point() + geom_smooth() + facet_wrap(~aspcl)
ggsave(filename = "~/work/growth/visuals/ht_dbh-aspcl.pdf")
ggplot(comp, aes(dbh, ht)) + geom_point() + geom_smooth() + facet_wrap(~elevcl)
ggsave(filename = "~/work/growth/visuals/ht_dbh-elevcl.pdf")

## htgrowth correlated with aspect
comp <- long[complete.cases(long[,c("ht","htgrowth","dbh","dbhgrowth","elev","asp")]),
             c("ht","htgrowth","dbh","dbhgrowth","asp","elev")]

## comp$elevcl = factor(x,levels(x)[c(4,5,1:3)])
pairs(~ht + htgrowth + dbh + dbhgrowth + asp + elev,
      data = comp, lower.panel = panel.smooth,
      upper.panel = panel.cor)
dev.copy2pdf(file = "~/work/growth/visuals/pairs-dbh-ht-growths-elev-asp.pdf")
dev.off()


comp$asp <- as.factor(comp$asp)

## dummy columns for aspect
A <- model.matrix(~ ht + dbh + asp - 1, comp)
pairs(~ht + dbh * 2:ncol(A), data = comp, lower.panel = panel.smooth,
      upper.panel = panel.cor)

tst <- reshape(comp, timevar = "asp",varying = c("dbh","ht"), direction = "wide")

rgb.palette <- colorRampPalette(c("blue", "yellow"), space = "rgb")
levelplot(cor, main="stage 12-14 array correlation matrix", xlab="",
          ylab="", col.regions=rgb.palette(120), cuts=100, at=seq(0,1,0.01))


###############################################################################
##
## levelplots
##
## ht growth by asp * elev
levelplot(htgrowth ~ elev*asp, data = long,
  xlab = "elevation", ylab = "aspect",
  main = "Height growth by aspect and elevation",
  col.regions = terrain.colors(100)
)
dev.copy2pdf(file = "~/work/growth/visuals/levelplot-htgrowth-asp-elev.pdf")
dev.off()

## dbh growth by asp * elev
levelplot(dbhgrowth ~ elev*asp, data = long,
  xlab = "elevation", ylab = "aspect",
  main = "DBH growth by aspect and elevation",
  col.regions = terrain.colors(100)
)
dev.copy2pdf(file = "~/work/growth/visuals/levelplot-dbhgrowth-asp-elev.pdf")
dev.off()


hh <- hclust(dist(mat))
ordered <- mat[hh$order,]

par(mfrow = c(1,3))
image(t(ordered)[,nrow(ordered):1])
plot(rowMeans(ordered,40:1,xlab = "Row", ylab = "Row Mean", pch = 19))
plot(colMeans(ordered,40:1,xlab = "Column", ylab = "Column Mean", pch = 19))


thing <- svd(complete.cases(tst))
plot(thing$d)

## svd
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[,nrow(dataMatrix):1])

