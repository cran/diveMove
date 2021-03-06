
".rleActivity" <- function(time, act, interval)
{
    ## Value: list with factor breaking activity phases, duration of each,
    ## and beginning and end times of each
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct date and time; act=factor representing
    ## activity for each row interval=sampling interval (s) in POSIXct
    ## units
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    runs <- rle(as.vector(act))
    cuttimbr <- factor(rep(seq(along=runs$lengths), runs$lengths))
    timsplit <- split(time, cuttimbr)
    begtim <- .POSIXct(sapply(timsplit, "[", 1),  "GMT")
    names(begtim) <- NULL
    endtim <- .POSIXct(sapply(timsplit, function(x) x[length(x)]), "GMT")
    names(endtim) <- NULL
    duration <- difftime(endtim, begtim, units="secs") + interval
    list(time.br=cuttimbr,
         time.peract=duration,
         beg.time=begtim,
         end.time=endtim)
}

##' Detect phases of activity from depth readings
##'
##' Functions to identify sections of a \acronym{TDR} record displaying one
##' of three possible activities: dry, wet, and trivial wet.
##'
##' See \code{\link{calibrateDepth}}.
##'
##' @aliases .detPhase
##' @param time \code{POSIXct} object with date and time for all depths.
##' @param depth numeric vector with depth readings.
##' @param dry.thr,wet.cond,wet.thr As passed from
##'     \code{\link{calibrateDepth}}.
##' @param interval As passed from \code{\link{calibrateDepth}}; sampling
##'     interval in seconds.
##' @return A list with components:
##'
##' \item{phase.id}{Numeric vector identifying each activity phase,
##' starting from 1 for every input record.}
##'
##' \item{activity}{Factor with levels \dQuote{L} indicating dry,
##' \dQuote{W} indicating wet, \dQuote{U} for underwater (above dive
##' criterion), \dQuote{D} for diving, \dQuote{Z} for trivial wet animal
##' activities.  Only \dQuote{L}, \dQuote{W}, and \dQuote{Z} are actually
##' represented.}
##'
##' \item{begin}{A \code{\link{POSIXct}} object as long as the number of
##' unique activity phases identified, indicating the start times for each
##' activity phase.}
##'
##' \item{end}{A \code{\link{POSIXct}} object as long as the number of
##' unique activity phases identified, indicating the end times for each
##' activity phase.}
##' @author Sebastian P. Luque \email{spluque@@gmail.com} and Andy Liaw.
##' @seealso \code{\link{.detDive}}, \code{\link{calibrateDepth}}
##' @keywords internal
##' @rdname detPhase-internal
##' @examples
##' data(divesTDR)
##' depths <- getDepth(divesTDR)
##' times <- getTime(divesTDR)
##'
##' detp <- diveMove:::.detPhase(times, depths, dry.thr=70, wet.thr=3610,
##'                              interval=getDtime(divesTDR))
##' ## Plot detected phases
##' plotTDR(times, depths)
##' rect(xleft=detp$begin, xright=detp$end, ybottom=0, ytop=-4,
##'      col=seq_along(detp$begin))
".detPhase" <- function(time, depth, dry.thr, wet.cond, wet.thr,
                        interval)
{
    ## Factor with default "land" values to code activity levels: L=land,
    ## W=wet (at-sea), U=underwater (below dive threshold), D=diving, Z=wet
    ## (leisure)
    trange <- range(time)
    trange.diff <- difftime(trange[2], trange[1], units="secs")
    if (wet.thr >= trange.diff)
        warning("wet.thr is larger than duration of time series")
    if (dry.thr >= trange.diff)
        warning("dry.thr is larger than duration of time series")
    act <- factor(rep("L", length(time)), levels=c("L", "W", "U", "D", "Z"))
    ## W when animal is wet; i.e. when depth is being recorded
    if (missing(wet.cond)) {
        act[!is.na(depth)] <- "W"
    } else {                            # or when wet.cond=TRUE
        if ((!is.logical(wet.cond)) || (length(wet.cond) != length(depth)))
            stop("'wet.cond' must be a logical vector as long as 'depth'")
        act[wet.cond] <- "W"
    }
    ## First run calculates times in each activity phase from the raw data
    rawacts <- .rleActivity(time, act, interval)
    ## On-land activity < 'dry.thr' should be considered still at-sea
    land <- levels(rawacts[[1]])[rawacts[[2]] < dry.thr]
    act[rawacts[[1]] %in% land & act == "L"] <- "W"
    ## Second run; at-sea phases < wet.thr should be leisure
    leiacts <- .rleActivity(time, act, interval)
    leisure <- levels(leiacts[[1]])[leiacts[[2]] < wet.thr]
    act[leiacts[[1]] %in% leisure & act == "W"] <- "Z"
    ## Last run of NAs should be forced back to "L"
    maxnoNA <- max(which(!is.na(depth)))
    if (maxnoNA < length(act)) act[(maxnoNA + 1):length(act)] <- "L"
    ## Final run to determine times with all corrected activities
    finacts <- .rleActivity(time, act, interval)
    nphase <- length(levels(finacts[[1]]))
    if(act[1] == "L" & act[length(act)] == "L") {
        message("Record is complete\n", nphase, " phases detected")
    } else {
        if(act[1] != "L" & act[length(act)] != "L") {
            message("Record is truncated at the beginning and at the end\n",
                    nphase, " phases detected")
        } else {
            if(act[1] != "L") {
                message("Record is truncated at the beginning\n", nphase,
                        " phases detected")
            } else {
                message("Record is truncated at the end\n", nphase,
                        " phases detected")
            }
        }
    }
    indphases <- as.numeric(finacts[[1]])
    names(finacts[[3]]) <- seq(length(finacts[[3]]))
    names(finacts[[4]]) <- seq(length(finacts[[4]]))
    list(phase.id=indphases,            # index of per-row activities
         activity=act,                  # activities themselves
         begin=finacts[[3]],            # start of activity phase
         end=finacts[[4]])              # end of activity phase
}
