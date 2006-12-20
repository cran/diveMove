library(diveMove)

(sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                             package="diveMove"),
                  concurrentCols=4:6, speed=TRUE))
(dcalib <- calibrateDepth(sealX, landerr=3610, offset=3))
(dcalib <- calibrateDepth(sealX, offset=3))

## (vcalib <- calibrateSpeed(dcalib, calType="ascent"))
## (vcalib <- calibrateSpeed(dcalib))

vcalib <- calibrateSpeed(dcalib)

## Plot
## plotcalib <- function(x, diveNo=seq(max(getDAct(x, "dive.id"))),
##                       labels="phase.id", concurVars=NULL, surface=FALSE, ...) {
##     diveids <- getDAct(x, "dive.id")
##     tdr <- getTDR(x)
##     if (surface) {
##         dives <- diveids %in% diveNo
##         postdiveids <- getDAct(x, "postdive.id")
##         postdives <- postdiveids %in% diveNo
##         ok <- which(dives | postdives)
##     } else ok <- diveMove:::.diveIndices(diveids, diveNo)
##     newtdr <- tdr[ok]
##     switch(labels,
##            phase.id={labs <- as.factor(getGAct(x, "phase.id")[ok])},
##            dive.phase={labs <- getDPhaseLab(x)[ok]})
##     labs <- factor(as.character(labs))
##     if (!is.null(concurVars)) {
##         if (!is.character(concurVars))
##             stop("concurVars must be of class character")
##         ccd <- getCCData(tdr, concurVars)[ok, ]
##         plot(newtdr, concurVars=ccd, phaseCol=labs, ...)
##     } else plot(newtdr, phaseCol=labs, ...)
## }
