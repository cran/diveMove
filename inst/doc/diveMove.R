## ----global-opts, include=FALSE-----------------------------------------------
knitr::opts_chunk$set(strip.white=TRUE, message=FALSE, warnings=FALSE,
                      size="small", out.width="100%")
library(pander)

## ----setup, results="hide"----------------------------------------------------
library(diveMove)

## ----dives-con----------------------------------------------------------------
fp <- file.path("data", "dives.csv")
sfp <- system.file(fp, package="diveMove")

## ----readin-csv---------------------------------------------------------------
srcfn <- basename(sfp)
tdrXcsv <- read.csv(sfp, sep=";")

## ----create-tdr---------------------------------------------------------------
ddtt.str <- paste(tdrXcsv$date, tdrXcsv$time)
ddtt <- strptime(ddtt.str,
                 format="%d/%m/%Y %H:%M:%S")
time.posixct <- as.POSIXct(ddtt, tz="GMT")
tdrX <- createTDR(time=time.posixct,
                  depth=tdrXcsv$depth,
                  concurrentData=tdrXcsv[, -c(1:3)],
                  dtime=5, file=srcfn)
## Or a TDRspeed object, since we know we have
## speed measurements:
tdrX <- createTDR(time=time.posixct,
                  depth=tdrXcsv$depth,
                  concurrentData=tdrXcsv[, -c(1:3)],
                  dtime=5, file=srcfn,
                  speed=TRUE)

## ----readin-tdr, eval=FALSE, results="hide"-----------------------------------
#  fp <- file.path("data", "dives.csv")
#  sfp <- system.file(fp, package="diveMove")
#  tdrX <- readTDR(sfp, speed=TRUE, sep=";",
#                  na.strings="", as.is=TRUE)
#  plotTDR(tdrX)

## ----plot-tdr, echo=FALSE, eval.after="fig.cap", fig.cap=capt, fig.asp=1.3----
fp <- file.path("data", "dives.csv")
sfp <- system.file(fp, package="diveMove")
tdrX <- readTDR(sfp, speed=TRUE, sep=";",
                na.strings="", as.is=TRUE)
capt <- paste("The `plotTDR()` method for *TDR* objects produces an",
              "interactive (disabled here to minimize vignette size)",
              "plot of the data, allowing for zooming and panning.")
knitr::include_graphics("figs/plot_tdr.png")

## ----eval=FALSE---------------------------------------------------------------
#  dcalib <- calibrateDepth(tdrX, zoc.method="visual")

## ----eval=FALSE---------------------------------------------------------------
#  dcalib <- calibrateDepth(tdrX,
#                           zoc.method="offset",
#                           offset=3)

## ----eval=FALSE---------------------------------------------------------------
#  dcalib <- calibrateDepth(tdrX,
#                           zoc.method="filter",
#                           k=c(3, 5760),
#                           probs=c(0.5, 0.02),
#                           na.rm=TRUE)

## ----zoc----------------------------------------------------------------------
dcalib <- calibrateDepth(tdrX, dive.thr=3,
                         zoc.method="offset",
                         offset=3,
                         descent.crit.q=0.01,
                         ascent.crit.q=0,
                         knot.factor=20)

## ----plot-tdrcalibrate, eval=FALSE--------------------------------------------
#  plotTDR(dcalib, concurVars=c("speed", "light"), surface=TRUE)

## ---- echo=FALSE, eval.after="fig.cap", fig.cap=capt, out.height="180%"-------
capt <- paste("The `plotTDR()` method for *TDRcalibrate* objects produces",
              "an interactive (disabled here to minimize vignette size)",
              "plot displaying information on the major activities",
              "identified throughout the record",
              "(wet/dry periods in this case).")
knitr::include_graphics("figs/plot_tdrcalib_01.png", dpi=NA)

## ----plot-dive-activity, eval=FALSE-------------------------------------------
#  plotTDR(dcalib, diveNo=2:8, what="phases", depth.lim=c(0, 80))

## ---- echo=FALSE, eval.after="fig.cap", fig.cap=capt--------------------------
capt <- paste("The `plotTDR()` method for *TDRcalibrate* objects can also",
              "display information on the different activities identified",
              "during each dive (descent=D, descent/bottom=DB,",
              "bottom/ascent=BA, ascent=A, X=surface).")
knitr::include_graphics("figs/plot_tdrcalib_02.png")

## ----extract-dive, eval=FALSE-------------------------------------------------
#  extractDive(dcalib, diveNo=2:8)

## ----tdr-extract--------------------------------------------------------------
getTDR(dcalib)

## ----grossact1, eval=FALSE----------------------------------------------------
#  getGAct(dcalib)

## ----grossact2, eval=FALSE----------------------------------------------------
#  getGAct(dcalib, "phase.id")

## ----diveact-1, eval=FALSE----------------------------------------------------
#  getDAct(dcalib)

## ----dphaselab1, eval=FALSE---------------------------------------------------
#  getDPhaseLab(dcalib)
#  getDPhaseLab(dcalib, 20)

## ----dphaselab2---------------------------------------------------------------
dphases <- getDPhaseLab(dcalib, c(100:300))

## ---- echo=FALSE--------------------------------------------------------------
capt <- paste("Details of the process of identification of dive phases",
              "shown by `plotDiveModel`, which has methods for objects",
              "of class *TDRcalibrate* and *diveModel*.")

## ----diveModel, fig.cap=capt, fig.width=5, fig.height=5, out.width=NULL-------
plotDiveModel(dcalib, diveNo=260)

## ----extractdive--------------------------------------------------------------
sealX <- extractDive(dcalib, diveNo=c(100:300))
sealX

## ----plot-phases, eval=FALSE--------------------------------------------------
#  plotTDR(sealX, phaseCol=dphases)

## ----dive-summaries, results="asis"-------------------------------------------
tdrXSumm1 <- head(diveStats(dcalib), 2)
cap <- "Per-dive summaries can be obtained with function `diveStats()`."
pander(tdrXSumm1, digits=2, caption=cap)

## ----time-budget, results="asis"----------------------------------------------
tbudget <- head(timeBudget(dcalib, ignoreZ=TRUE), 5)
cap <- "Time budget summary can be calculated with function `timeBudget()`."
pander(tbudget, digits=2, caption=cap)

## ---- echo=FALSE--------------------------------------------------------------
capt <- paste("The relationship between measured speed and rate of depth",
              "change can be used to calibrate speed readings.",
              "The line defining the calibration for speed measurements",
              "passes through the bottom edge of a chosen contour,",
              "extracted from a bivariate kernel density grid.")

## ----calibrate-speed, fig.cap=capt, fig.width=5, fig.height=5, out.width=NULL----
vcalib <- calibrateSpeed(dcalib, tau=0.1,
                         contour.level=0.1,
                         z=1, bad=c(0, 0),
                         cex.pts=0.2)

