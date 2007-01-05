library(diveMove)

(sealX <- readTDR(system.file(file.path("data", "dives.csv"),
                             package="diveMove"),
                  concurrentCols=4:6, speed=TRUE))
(dcalib <- calibrateDepth(sealX, dry.thr=3610, offset=3))
(dcalib <- calibrateDepth(sealX, offset=3, ascent.crit=0.5,
                          descent.crit=0.5, wiggle=0.75))

## (dcalib <- calibrateDepth(sealX, offset=3, descent.crit.q=0.25,
##                           ascent.crit.q=0.75))

###_+ Check all calibrateDepth() procedure

###_ : Check phase detection
detp <- detPhase(getTime(sealX), getDepth(sealX), dry.thr=70,
                 wet.thr=3610, getDtime(sealX))
###_ : Check zoc
zd <- zoc(getTime(sealX), getDepth(sealX), offset=3)
if (!is.null(zd)) sealX@depth <- zd
###_ : Check dive detection
detd <- detDive(getDepth(sealX), detp[[2]], 4, getDtime(sealX))
###_ : Check labelling of dive phases
phaselabs <- labDivePhase(sealX, detd[, 1], descent.crit.q=0.1,
                          ascent.crit.q=0.5, wiggle.tol=0.85)

## (vcalib <- calibrateSpeed(dcalib, calType="ascent"))
## (vcalib <- calibrateSpeed(dcalib))

vcalib <- calibrateSpeed(dcalib, z=0, cex.pts=0.2)

## ## ##_+ Phase Detection ------------------------------------------------------
## "cutDive" <- function(x, crit.quantile, tolerance)
## {
##     ## Value: Create a factor that breaks a dive into descent,
##     ## descent/bottom, bottom, bottom/ascent, ascent, and/or
##     ## descent/ascent given a proportion of maximum depth for bottom time.
##     ## Return a character matrix with orig ID and corresponding label.
##     ## --------------------------------------------------------------------
##     ## Arguments: x=a 2-col matrix with index in original TDR object and
##     ## non NA depths.  A single dive's data.
##     ## --------------------------------------------------------------------
##     ## Author: Sebastian Luque
##     ## --------------------------------------------------------------------
##     depths <- x[, 2]
##     times <- x[, 3]
##     "det.fun" <- function(x, y, index, ascent=FALSE) {
##         if (ascent) {
##             diffx <- diff(x[index:length(x)])
##             rate <- -diff(y[index:length(y)]) / diffx
##             crit.rate <- quantile(rate[rate > 0], crit.quantile)
##             beyond <- which(rate > crit.rate)
##             crit.id <- ifelse(length(beyond) < 1, length(rate),
##                               index + (beyond[1] - 1))
##             crit.id:length(x)
##         } else {
##             diffx <- diff(x[1:index])
##             rate <- diff(y[1:index]) / diffx
##             crit.rate <- quantile(rate[rate > 0], crit.quantile)
##             beyond <- which(rate < crit.rate)
##             crit.id <- ifelse(length(beyond) < 1, length(rate), beyond[1])
##             1:(crit.id + 1)
##         }
##     }

##     ## Descent detection
##     desc.maxdd.id <- which.max(depths)
##     descind <- det.fun(times, depths, desc.maxdd.id)

##     ## Ascent detection
##     depths.rev <- rev(depths)
##     asc.maxdd.id <- (nrow(x) - which.max(depths.rev)) + 1
##     ascind <- det.fun(times, depths, asc.maxdd.id, ascent=TRUE)

##     ## Bottom detection
##     bottind <- c(descind[length(descind)],
##                  setdiff(seq(nrow(x)), union(descind, ascind)),
##                  ascind[1])

##     ## descent is everything in descind that's not in union of bottind and ascind
##     d <- setdiff(descind, union(bottind, ascind))
##     ## descent/bottom is what's common to descind and bottind
##     db <- intersect(descind, bottind)
##     ## bottom is everything in bottind that's not in union of descind and ascind
##     b <- setdiff(bottind, union(descind, ascind))
##     ## bottom/ascent is what's common to ascind and bottind
##     ba <- intersect(ascind, bottind)
##     ## ascent is everything in ascind that's not in union of descind and bottind
##     a <- setdiff(ascind, union(descind, bottind))
##     ## descent/ascent is what's common to descind and ascind
##     da <- intersect(descind, ascind)

##     labs <- character(nrow(x))
##     labs[d] <- "D"
##     labs[db] <- "DB"
##     labs[b] <- "B"
##     labs[ba] <- "BA"
##     labs[a] <- "A"
##     labs[da] <- "DA"
##     ## If there are repetitions, keep the last one to avoid missing ascent labels
##     rowids <- unique(c(x[d, 1], x[db, 1], x[b, 1], x[ba, 1], x[a, 1], x[da, 1]))
##     cbind(rowids, labs)
## }

## debug(diveMove:::.cutDive)
## Extract dive 3 which is problematic
## diveX <- extractDive(dcalib, 127)
## depths <- getDepth(diveX)
## times <- getTime(diveX)
## pCol <- diveMove:::.cutDive(matrix(c(rep(1, length(times)), depths, as.numeric(times)),
##                         ncol=3), ascent.crit=0.1, descent.crit=0.1, wiggle=0.75)
## plotDive(times, depths, phaseCol=factor(pCol[, 2]))

## ## A loess method
## desc.smooth <- loess.smooth(times[1:maxd.id], -depths[1:maxd.id], span=0.5)
## plot(times[1:maxd.id], -depths[1:maxd.id])
## lines(desc.smooth)
