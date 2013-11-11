## Transform rGR calculated by upper quantile regression by SI and SDP classes
##  to measure of absolute growth.
## Need to multiply rGR by a*priorBV^b where a, b were fit during upper quantile
##  regression by SI and SDP

## load size effect parameters (fit during rgr fitting), data
rgrparams <- read.csv("~/Allometry/data/rgr-parameters-sisdp.csv")
bclong <- read.csv("~/Allometry/data/long-bc-derived.csv")
bclong <- subset(bclong, stat == "ALIVE" & bvgrowth>=0) ## not sure how to treat trees

##################################################################################
## Get size effect parameters for each each combination of sdpclass and si
sizepars <- ddply(rgrparams, .(sdpclass, si), .fun = function(x) {
    x <- droplevels(x)
    data.frame(
    sdpclass = mean(x$sdpclass), si = mean(x$si), top.a = mean(x$top.a),
    top.b = mean(x$top.b))
})

## Calculate size effect
bclong$sdpclass <- as.numeric(bclong$sdpclass)
sizeeff <- ddply(bclong, .(sdpclass, si), .fun = function(x) {
    x <- droplevels(x)
    par.a <- sizepars[sizepars$sdpclass==mean(x$sdpclass)
                      & sizepars$si==mean(x$si),]$top.a
    par.b <- sizepars[sizepars$sdpclass==mean(x$sdpclass)
                      & sizepars$si==mean(x$si),]$top.b
    data.frame(sdpclass = x$sdpclass, si = x$si, id = x$id, install = x$install,
    plot = x$plot, sizeeff = par.a * x$priorbv ^ par.b, time = x$time)
})

sizeeff <- sizeeff[order(sizeeff$install, sizeeff$plot, sizeeff$time, sizeeff$id),]
bclong <- bclong[order(bclong$install, bclong$plot, bclong$time, bclong$id),]
bclong$sizeeff <- sizeeff$sizeeff

##################################################################################
## Get predicted rGR, requires making neighbor matrices
## define arguments
rgrcol = "rgrsisdp"
sr = 6
spec = "FD"
ind.var = "priorbv"
dep.var = "rgrsisdp"
currentmodel = "simplest"

## Make neighbor matrices
fit.MLE.models(bclong, sr=sr, spec=spec, ind.var = ind.var, dep.var = dep.var,
               realdist = TRUE)

## Retrieve parameters of most recent fit
pars <- read.csv("parameters.csv")
ps <- get.params(sr,spec,ind.var,dep.var,currentmodel = currentmodel)
ps

## Calculate predicted rGR values
targets$pred.rGR <- simplest(ps)

## Convert predicted rGR to predicted growth:
##  size effect * predicted rGR = predicted growth
targets$pgrowth <- targets$sizeeff * targets$pred.rGR

##################################################################################
## Graph the results
pdf('predicted-growth-rgrsisdp-simplest.pdf')

par(mfrow = c(1,2))
plot(targets$priorbv, targets$bvgrowth, main = "Predicted/Observed BV Growth vs.
Prior BV\n Size effect fit with SI and SDP,\n NCI fit with Simplest Model",xlab = "Prior Bole Volume", ylab = "Bole Volume Growth")
points(targets$priorbv, targets$pgrowth, col="red")
legend("topleft",legend = c("Observed","Predicted"), col = c("black","red"),
       pch = 1)

## residuals, obs - pred
targets$res <-  targets$bvgrowth - targets$pgrowth
plot(targets$bvgrowth, targets$res, main = "Residuals vs Observed Growth",
     xlab = "Observed Bole Volume Growth",
     ylab = "Obs. BV Growth - Pred. BV Growth")
abline(h=0)

dev.off()

