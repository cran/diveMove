## METHODS

## FOR TDR AND TDRspeed ---------------------------------------------------
setMethod("show", signature=signature(object="TDR"),
          definition=function(object) {
              trange <- range(object@time)
              cat("Time-Depth Recorder data -- Class",
                  class(object), "object\n")
              cat("  Source File             :", object@file, "\n")
              cat("  Sampling Interval (s)   :", object@dtime, "\n")
              cat("  Number of Samples       :", length(object@time), "\n")
              cat("  Sampling Begins         :",
                  paste(object@time[1]), "\n")
              cat("  Sampling Ends           :",
                  paste(object@time[length(object@time)]), "\n")
              cat("  Total Duration (d)      :",
                  difftime(trange[2], trange[1], units="days"), "\n")
          })


if (!isGeneric("plot")) {
    setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}
setMethod("plot", signature(x="TDR", y="missing"),
          function(x, ...) {
              plotDive(getTime(x), getDepth(x), ...)
          })
setMethod("plot", signature(x="TDRspeed", y="missing"),
          function(x, ...) {
              plotDive(getTime(x), getDepth(x), getSpeed(x), ...)
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

if (!isGeneric("getSpeed")) {               # speed accessor
    setGeneric("getSpeed", function(x) standardGeneric("getSpeed"))
}
setMethod("getSpeed", signature(x="TDRspeed"),
          function(x) x@speed)

if (!isGeneric("getDtime")) {               # interval accessor
    setGeneric("getDtime", function(x) standardGeneric("getDtime"))
}
setMethod("getDtime", signature(x="TDR"), function(x) x@dtime)


## Conversions
setAs("TDR", "data.frame", function(from) {
    file.src <- from@file
    dtime <- from@dtime
    if (!is(from, "TDRspeed")) {
        val <- data.frame(time=from@time, depth=from@depth)
    } else {
        val <- data.frame(time=from@time, depth=from@depth, speed=from@speed)
    }
    attr(val, "file") <- file.src
    attr(val, "dtime") <- dtime
})
setMethod("as.data.frame", signature("TDR"),
          function(x, row.names=NULL, optional=FALSE) {
              as(x, "data.frame")
          })


## FOR TDRcalibrate -------------------------------------------------------
setMethod("show", signature=signature(object="TDRcalibrate"),
          definition=function(object) {
              dry <- object@gross.activity$trip.act == "L"
              dd <- length(unique(object@gross.activity$ phase.id[dry]))
              wet <- object@gross.activity$trip.act == "W"
              wetz <- object@gross.activity$trip.act == "Z"
              ww <- length(unique(object@gross.activity$ phase.id[wet | wetz]))
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
              if (length(object@speed.calib.coefs) != 0) {
                  cat("  Speed calibration coefficients    : a =",
                      format(object@speed.calib.coefs[1], digits=2), "; b =",
                      format(object@speed.calib.coefs[2], digits=2), "\n")
              } else cat("\n")
          })

setMethod("plot", signature(x="TDRcalibrate", y="missing"),
          function(x, diveNo=seq(unique(getDAct(x, "dive.id"))),
                   labels="phase.id", surface=FALSE, ...) {
              diveids <- getDAct(x, "dive.id")
              tdr <- getTDR(x)
              if (surface) {
                  dives <- diveids %in% diveNo
                  postdiveids <- getDAct(x, "postdive.id")
                  postdives <- postdiveids %in% diveNo
                  ok <- seq(length(diveids))[dives | postdives]
              } else ok <- .diveIndices(diveids, diveNo)
              dtime <- getTime(tdr)[ok]
              ddepth <- getDepth(tdr)[ok]
              switch(labels,
                     phase.id={labs <- as.factor(getGAct(x, "phase.id")[ok])},
                     dive.phase={labs <- getDPhaseLab(x)[ok]})
              if (is(tdr, "TDRspeed")) {
                  plotDive(dtime, ddepth, getSpeed(tdr)[ok], phaseCol=labs, ...)
              } else {
                  plotDive(dtime, ddepth, phaseCol=labs, ...)
              }
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
              okpts <- .diveIndices(getDAct(x, "dive.id"), diveNo)
              phases[okpts]
          })

if (!isGeneric("getSpeedCoef")) {         # speed calibration coefs accessor
    setGeneric("getSpeedCoef", function(x) standardGeneric("getSpeedCoef"))
}
setMethod("getSpeedCoef", signature(x="TDRcalibrate"),
          function(x) x@speed.calib.coefs)


## Generators and subsetters ----------------------------------------------
"createTDR" <- function(time, depth, speed, dtime, file)
{
    ## Value: An object of TDR or TDRspeed class.  Useful to recreate
    ## objects once depth has been zoc'ed and speed calibrated for further
    ## analyses.
    ## --------------------------------------------------------------------
    ## Arguments: see class definitions
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if(missing(speed)) {
        new("TDR", time=time, depth=depth, dtime=dtime, file=file)
    } else {
        new("TDRspeed", time=time, depth=depth, speed=speed,
            dtime=dtime, file=file)
    }
}


if (!isGeneric("extractDive")) {        # extract a dive
    setGeneric("extractDive",
               function(obj, diveNo, id) standardGeneric("extractDive"))
}
setMethod("extractDive", signature(obj="TDR", diveNo="numeric",
                                   id="numeric"), # for TDR object
          function(obj, diveNo, id) {
              if (length(id) != length(getTime(obj))) {
                  stop ("id and obj must have equal number of rows")
              }
              okpts <- .diveIndices(id, diveNo)
              if (is(obj, "TDRspeed")) {
                  new("TDRspeed", time=getTime(obj)[okpts],
                      depth=getDepth(obj)[okpts],
                      speed=getSpeed(obj)[okpts], dtime=getDtime(obj),
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
              okpts <- .diveIndices(getDAct(obj, "dive.id"), diveNo)
              if (is(ctdr, "TDRspeed")) {
                  new("TDRspeed", time=getTime(ctdr)[okpts],
                      depth=getDepth(ctdr)[okpts],
                      speed=getSpeed(ctdr)[okpts],dtime=getDtime(ctdr),
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
              if (ignoreZ) {            # ignore the short baths
                  act[act == "Z"] <- "L"
                  attlist <- getAct(tt, act, interval)
                  actlabel <- rle(as.vector(act))$values
                  tripno <- seq(along=actlabel)
              } else {                  # count the short baths
                  attlist <- getGAct(obj)
                  actlabel <- rle(as.vector(act))$values
                  tripno <- seq(along=actlabel)
              }
              data.frame(phaseno=tripno, activity=actlabel,
                         beg=attlist[[3]], end=attlist[[4]],
                         row.names=NULL)
          })
