## METHODS

## FOR TDR AND TDRvel -----------------------------------------------------
setMethod("show", signature=signature(object="TDR"),
          definition=function(object) {
              cat("Time-Depth Recorder data -- Class",
                  class(object), "object\n")
              cat("  Source File             :", object@file, "\n")
              cat("  Sampling Interval (s)   :", object@dtime * 86400, "\n")
              cat("  Number of Samples       :", length(object@time), "\n")
              cat("  Sampling Begins         :",
                  paste(object@time[1]), "\n")
              cat("  Sampling Ends           :",
                  paste(object@time[length(object@time)]), "\n")
              cat("  Total Duration (d)      :",
                  diff(range(object@time, na.rm=TRUE)), "\n")
          })


if (!isGeneric("plot")) {
    setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}
setMethod("plot", signature(x="TDR", y="missing"),
          function(x, ...) {
              plotDive(getTime(x), getDepth(x), ...)
          })
setMethod("plot", signature(x="TDRvel", y="missing"),
          function(x, ...) {
              plotDive(getTime(x), getDepth(x), getVeloc(x), ...)
          })

if (!isGeneric("getFileName")) {               # File name accessor
    setGeneric("getFileName", function(x) standardGeneric("getFileName"))
}
setMethod("getFileName", signature(x="TDR"), function(x) x@file)

if (!isGeneric("getTime")) {               # time accessor
    setGeneric("getTime", function(x) standardGeneric("getTime"))
}
setMethod("getTime", signature(x="TDR"), function(x) x@time)

if (!isGeneric("getDepth")) {               # Depth accessor
    setGeneric("getDepth", function(x) standardGeneric("getDepth"))
}
setMethod("getDepth", signature(x="TDR"), function(x) x@depth)

if (!isGeneric("getVeloc")) {               # velocity accessor
    setGeneric("getVeloc", function(x) standardGeneric("getVeloc"))
}
setMethod("getVeloc", signature(x="TDRvel"),
          function(x) x@velocity)

if (!isGeneric("getDtime")) {               # interval accessor
    setGeneric("getDtime", function(x) standardGeneric("getDtime"))
}
setMethod("getDtime", signature(x="TDR"), function(x) x@dtime)


## Conversions
setAs("TDR", "data.frame", function(from) {
    if (!is(from, "TDRvel")) {
        data.frame(time=from@time, depth=from@depth)
    } else {
        data.frame(time=from@time, depth=from@depth, velocity=from@velocity)
    }
})
setMethod("as.data.frame", signature("TDR"),
          function(x, row.names=NULL, optional=FALSE) {
              as(x, "data.frame")
          })


## FOR TDRcalibrate -------------------------------------------------------
setMethod("show", signature=signature(object="TDRcalibrate"),
          definition=function(object) {
              dd <- length(unique(object@gross.activity$
                                  phase.id[object@gross.activity$trip.act == "L"]))
              ww <- length(unique(object@gross.activity$
                                  phase.id[object@gross.activity$trip.act == "W" |
                                           object@gross.activity$trip.act == "Z"]))
              cat("Depth calibration -- Class", class(object), "object\n")
              cat("  Source file                       :",
                  object@tdr@file, "\n")
              cat("  Number of dry phases              :", dd, "\n")
              cat("  Number of aquatic phases          :", ww, "\n")
              cat("  Number of dives detected          :",
                  max(object@dive.activity$dive.id, na.rm=TRUE), "\n")
              cat("  Dry threshold used (s)            :",
                  object@land.threshold, "\n")
              cat("  Aquatic theshold used (s)         :",
                  object@sea.threshold, "\n")
              cat("  Dive threshold used (s)           :",
                  object@dive.threshold, "\n")
              if (length(object@vel.calib.coefs) != 0) {
                  cat("  Velocity calibration coefficients : a =",
                      format(object@vel.calib.coefs[1], digits=2), "; b =",
                      format(object@vel.calib.coefs[2], digits=2), "\n")
              } else cat("\n")
          })

if (!isGeneric("getTDR")) {               # zoc'ed TDR accessor
    setGeneric("getTDR", function(x) standardGeneric("getTDR"))
}
setMethod("getTDR", signature(x="TDRcalibrate"), function(x) x@tdr)

if (!isGeneric("getGAct")) {               # gross activity accessor
    setGeneric("getGAct", function(x, y) standardGeneric("getGAct"))
}
## access the entire list
setMethod("getGAct", signature(x="TDRcalibrate", y="missing"),
          function(x) x@gross.activity)
## access only a named element
setMethod("getGAct", signature(x="TDRcalibrate", y="character"),
          function(x, y) x@gross.activity[[y]])

if (!isGeneric("getDAct")) {               # dive activity accessor
    setGeneric("getDAct", function(x, y) standardGeneric("getDAct"))
}
## access entire data frame
setMethod("getDAct", signature(x="TDRcalibrate", y="missing"),
          function(x) x@dive.activity)
## access only a certain column
setMethod("getDAct", signature(x="TDRcalibrate", y="character"),
          function(x, y) x@dive.activity[, y])

if (!isGeneric("getDPhaseLab")) {       # dive phase label accessor
    setGeneric("getDPhaseLab",
               function(x, diveNo) standardGeneric("getDPhaseLab"))
}
## access the entire factor
setMethod("getDPhaseLab", signature(x="TDRcalibrate", diveNo="missing"),
          function(x) x@dive.phases)
## access only those from certain dives
setMethod("getDPhaseLab", signature(x="TDRcalibrate", diveNo="numeric"),
          function(x, diveNo) {
              ctdr <- getTDR(x)
              phases <- x@dive.phases
              ok <- which(getDAct(x, "dive.id") %in% diveNo)
              okl <- setdiff(ok - 1, ok)
              okr <- setdiff(ok + 1, ok)
              okpts <- sort(c(okl, ok, okr))        # add the surface points
              phases[okpts]
          })

if (!isGeneric("getVelCoef")) {         # vel calibration coefs accessor
    setGeneric("getVelCoef", function(x) standardGeneric("getVelCoef"))
}
setMethod("getVelCoef", signature(x="TDRcalibrate"),
          function(x) x@vel.calib.coefs)


## Generators and subsetters ----------------------------------------------
"createTDR" <- function(time, depth, vel, dtime, file)
{
    ## Purpose: Create an object of TDR or TDRvel class.  Useful to recreate
    ## 	      objects once depth has been zoc'ed and velocity calibrated
    ## 	      for further analyses.
    ## --------------------------------------------------------------------
    ## Arguments: see class definitions
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if(missing(vel)) {
        new("TDR", time=time, depth=depth, dtime=dtime, file=file)
    } else {
        new("TDRvel", time=time, depth=depth, velocity=vel,
            dtime=dtime, file=file)
    }
}


if (!isGeneric("extractDive")) {               # extract a dive
    setGeneric("extractDive",
               function(obj, diveNo, id) standardGeneric("extractDive"))
}
setMethod("extractDive", signature(obj="TDR", diveNo="numeric",
                                   id="numeric"), # for TDR object
          function(obj, diveNo, id) {
              if (length(id) != length(getTime(obj))) {
                  stop ("id and obj must have the equal number of rows")
              }
              ok <- which(id %in% diveNo)
              okl <- setdiff(ok - 1, ok)
              okr <- setdiff(ok + 1, ok)
              okpts <- sort(c(okl, ok, okr))        # add the surface points
              if (is(obj, "TDRvel")) {
                  new("TDRvel", time=getTime(obj)[okpts],
                      depth=getDepth(obj)[okpts],
                      velocity=getVeloc(obj)[okpts], dtime=getDtime(obj),
                      file=obj@file)
              } else {
                  new("TDR", time=getTime(obj)[okpts],
                      depth=getDepth(obj)[okpts],
                      dtime=getDtime(obj), file=obj@file)
              }
          })

setMethod("extractDive",                # for TDRcalibrate
          signature(obj="TDRcalibrate", diveNo="numeric", id="missing"),
          function(obj, diveNo) {
              ctdr <- getTDR(obj)
              ok <- which(getDAct(obj, "dive.id") %in% diveNo)
              okl <- setdiff(ok - 1, ok)
              okr <- setdiff(ok + 1, ok)
              okpts <- sort(c(okl, ok, okr))        # add the surface points
              if (is(ctdr, "TDRvel")) {
                  new("TDRvel", time=getTime(ctdr)[okpts],
                      depth=getDepth(ctdr)[okpts],
                      velocity=getVeloc(ctdr)[okpts],dtime=getDtime(ctdr),
                      file=ctdr@file)
              } else {
                  new("TDR", time=getTime(ctdr)[okpts],
                      depth=getDepth(ctdr)[okpts],
                      dtime=getDtime(ctdr), file=ctdr@file)
              }
          })


if (!isGeneric("attendance")) {
    setGeneric("attendance",
               function(obj, ignoreZ) standardGeneric("attendance"))
}
setMethod("attendance",            # a table of general attendance pattern
          signature(obj="TDRcalibrate", ignoreZ="logical"),
          function(obj, ignoreZ) {
              act <- getGAct(obj, "trip.act")
              tt <- getTime(getTDR(obj))
              interval <- getDtime(getTDR(obj))
              if (ignoreZ) {              # ignore the short baths
                  act[act == "Z"] <- "L"
                  attlist <- getAct(tt, act, interval)
                  actlabel <- act[match(names(attlist[[3]]), attlist[[1]])]
                  tripno <- seq(along=actlabel)
              } else {                    # count the short baths
                  attlist <- getGAct(obj)
                  actlabel <- act[match(names(attlist[[3]]), attlist[[1]])]
                  tripno <- seq(along=actlabel)
              }
              data.frame(phaseno=tripno, activity=actlabel,
                         beg=chron(attlist[[3]]),
                         end=chron(attlist[[4]]), row.names=NULL)
          })
