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
              drange <- range(object@depth, na.rm=TRUE)
              cat("  Measured depth range (m): [",
                  drange[1], ",", drange[2], "]\n")
              if (length(names(object@concurrentData)) > 0) {
                  cat("  Other variables         :",
                      names(object@concurrentData), "\n")
              }
          })


if (!isGeneric("plot")) {
    setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
}
setMethod("plot", signature(x="TDR", y="missing"),
          function(x, ...) {
              plotDive(getTime(x), getDepth(x), ...)
          })
setMethod("plot", signature(x="TDRspeed", y="missing"),
          function(x, concurVars=NULL, concurVarTitles, ...) {
              if (missing(concurVarTitles) && !is.null(concurVars)) {
                  concurVarTitles <- colnames(concurVars)
              } else if (missing(concurVarTitles) && is.null(concurVars)) {
                  concurVarTitles <- "speed (m/s)"
              }
              plotDive(getTime(x), getDepth(x),
                       concurVars=cbind(getSpeed(x), concurVars),
                       concurVarTitles=concurVarTitles, ...)
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
".speedCol" <- function(x)
{
    ## Value: column number where speed is located in x
    ## --------------------------------------------------------------------
    ## Arguments: x=data frame
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    dataNames <- names(x)
    colN <- dataNames %in% .speedNames
    if (length(which(colN)) != 1)
        stop("the column number for speed could not be determined")
    which(colN)
}
setMethod("getSpeed", signature(x="TDRspeed"), function(x) {
    ccData <- x@concurrentData
    speedCol <- .speedCol(ccData)
    ccData[, speedCol]
})

if (!isGeneric("getDtime")) {               # interval accessor
    setGeneric("getDtime", function(x) standardGeneric("getDtime"))
}
setMethod("getDtime", signature(x="TDR"), function(x) x@dtime)

if (!isGeneric("getCCData")) {               # concurrent data accessor
    setGeneric("getCCData", function(x, y) standardGeneric("getCCData"))
}
## Get entire data frame
setMethod("getCCData", signature(x="TDR", y="missing"), function(x) {
    if (nrow(x@concurrentData) > 0) {
        x@concurrentData
    } else stop("No concurrent data are available")
})
## Get named component(s) of the data frame
setMethod("getCCData", signature(x="TDR", y="character"), function(x, y) {
    if (nrow(x@concurrentData) < 1) stop("No concurrent data are available")
    ccd <- getCCData(x)
    ccdnames <- names(ccd)
    ok <- ccdnames %in% y
    bady <- !y %in% ccdnames
    if (length(which(ok)) < 1) {
        stop(paste("y must contain at least one of the names of",
                   "the concurrent data frame"))
    } else if (any(bady)) {
        warning("components: ", y[bady], " could not be found and were ignored")
    }
    as.data.frame(ccd[, ok])
})


## Conversions
setAs("TDR", "data.frame", function(from) {
    file.src <- from@file
    dtime <- from@dtime
    val <- data.frame(time=from@time, depth=from@depth, getCCData(from))
    attr(val, "file") <- file.src
    attr(val, "dtime") <- dtime
    val
})
setMethod("as.data.frame", signature("TDR"),
          function(x, row.names=NULL, optional=FALSE) {
              as(x, "data.frame")
          })

setAs("TDR", "TDRspeed", function(from) {
    new("TDRspeed", file=from@file, time=from@time, depth=from@depth,
        dtime=from@dtime, concurrentData=from@concurrentData)
})
if (!isGeneric("as.TDRspeed")) {               # zoc'ed TDR accessor
    setGeneric("as.TDRspeed", function(x) standardGeneric("as.TDRspeed"))
}
setMethod("as.TDRspeed", signature("TDR"), function(x) as(x, "TDRspeed"))


## Replacements
if (!isGeneric("depth<-")) {               # depth
    setGeneric("depth<-", function(x, value) standardGeneric("depth<-"))
}
setReplaceMethod("depth", signature(x="TDR", value="numeric"),
                 function(x, value) {
                     orig <- getDepth(x)
                     if (length(orig) != length(value))
                         stop(paste("replacement must have length:",
                                    length(orig),
                                    "(i.e. same length as original)"))
                     x@depth <- value
                     x
                 })

if (!isGeneric("speed<-")) {               # speed
    setGeneric("speed<-", function(x, value) standardGeneric("speed<-"))
}
setReplaceMethod("speed", signature(x="TDRspeed", value="numeric"),
                 function(x, value) {
                     ccData <- x@concurrentData
                     speedCol <- .speedCol(ccData)
                     if (length(ccData[, speedCol]) != length(value))
                         stop(paste("replacement must have length:",
                                    length(ccData[, speedCol]),
                                    "(i.e. same length as original)"))
                     x@concurrentData[, speedCol] <- value
                     x
                 })

if (!isGeneric("ccData<-")) {               # speed
    setGeneric("ccData<-", function(x, value) standardGeneric("ccData<-"))
}
setReplaceMethod("ccData", signature(x="TDR", value="data.frame"),
                 function(x, value) {
                     ccDataN <- nrow(getCCData(x))
                     valueN <- nrow(value)
                     if (ccDataN != valueN)
                         stop(paste("replacement must have:", ccDataN,
                                    "rows (i.e. same as original)"))
                     x@concurrentData <- value
                     x
                 })


## Subsetting
setMethod("[", signature("TDR"), function(x, i, j, ..., drop) {
    if (!missing(j) || !missing(...) || !missing(drop))
        stop("subsetting TDR objects can only be done on a single index")
    new(class(x), file=getFileName(x), dtime=getDtime(x), time=getTime(x)[i],
        depth=getDepth(x)[i], concurrentData=getCCData(x)[i, ])
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
          function(x, diveNo=seq(max(getDAct(x, "dive.id"))),
                   labels="phase.id", concurVars=NULL, surface=FALSE, ...) {
              diveids <- getDAct(x, "dive.id")
              tdr <- getTDR(x)
              if (surface) {
                  dives <- diveids %in% diveNo
                  postdiveids <- getDAct(x, "postdive.id")
                  postdives <- postdiveids %in% diveNo
                  ok <- which(dives | postdives)
              } else ok <- .diveIndices(diveids, diveNo)
              newtdr <- tdr[ok]
              switch(labels,
                     phase.id={labs <- as.factor(getGAct(x, "phase.id")[ok])},
                     dive.phase={labs <- getDPhaseLab(x)[ok]})
              labs <- factor(as.character(labs))
              if (!is.null(concurVars)) {
                  if (!is.character(concurVars))
                      stop("concurVars must be of class character")
                  ccd <- getCCData(tdr, concurVars)[ok, ]
                  plot(newtdr, concurVars=ccd, phaseCol=labs, ...)
              } else plot(newtdr, phaseCol=labs, ...)
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
"createTDR" <- function(time, depth, concurrentData, speed=FALSE, dtime, file)
{
    ## Value: An object of TDR or TDRspeed class.  Useful to recreate
    ## objects once depth has been zoc'ed and speed calibrated for further
    ## analyses.
    ## --------------------------------------------------------------------
    ## Arguments: see class definitions
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if(speed) {
        new("TDRspeed", time=time, depth=depth, concurrentData=concurrentData,
            dtime=dtime, file=file)
    } else {
        new("TDR", time=time, depth=depth, concurrentData=concurrentData,
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
                      concurrentData=getCCData(obj)[okpts, ],
                      dtime=getDtime(obj), file=obj@file)
              } else {
                  new("TDR", time=getTime(obj)[okpts],
                      depth=getDepth(obj)[okpts],
                      concurrentData=getCCData(obj)[okpts, ],
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
                      concurrentData=getCCData(ctdr)[okpts, ],
                      dtime=getDtime(ctdr), file=ctdr@file)
              } else {
                  new("TDR", time=getTime(ctdr)[okpts],
                      depth=getDepth(ctdr)[okpts],
                      concurrentData=getCCData(ctdr)[okpts, ],
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
