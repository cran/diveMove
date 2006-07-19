"diveStats" <- function(x)
{
    ## Purpose: Per-dive statistics
    ## --------------------------------------------------------------------
    ## Arguments: x=object of class TDRcalibrate
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDRcalibrate")) stop("x must be a TDRcalibrate object")
    zvtdr <- getTDR(x)                     # fully calibrated object
    interval <- getDtime(zvtdr)              # sampling interval
    diveid <- getDAct(x, "dive.id")     # dive IDs
    postdiveid <- getDAct(x, "postdive.id")        # postdive IDs
    ok <- which(diveid > 0 & diveid %in% postdiveid) # diving subscripts
    dtimes <- getTime(zvtdr)[ok]                        # diving times
    ddepths <- getDepth(zvtdr)[ok]                      # diving depths
    dids <- diveid[ok]                    # dive IDs
    dphases <- getDPhaseLab(x)[ok]        # dive phase labels
                                        # postdive subscripts:
    okpd <- which(postdiveid %in% unique(dids))
    pdtimes <- getTime(zvtdr)[okpd]          # required postdive times
    pddepths <- getDepth(zvtdr)[okpd]        # required postdive depths
    pdids <- postdiveid[okpd]             # required postdive IDs

    postdive.dur <- tapply(pdtimes, pdids,
                           function(k) k[length(k)] - k[1]) * 86400

    if (!is(zvtdr, "TDRvel")) {
        td <- data.frame(dphases, dtimes, ddepths)
        perdive <- do.call(rbind, by(td, dids, getDive, interval=interval))
        res <- data.frame(perdive, postdive.dur)
        for (i in 1:3) res[, i] <- chron(res[, i])
    } else {
        dvels <- getVeloc(zvtdr)[ok]        # diving velocities
        td <- data.frame(dphases, dtimes, ddepths, dvels)
        perdive <- do.call(rbind, by(td, dids, getDive, interval=interval,
                                     vel=TRUE))
        ## for postdive total distance and mean velocity
        ptd <- matrix(c(pdtimes, getVeloc(zvtdr)[okpd]), ncol=2)
        pdv <- do.call(rbind, by(ptd, pdids, .getVelStats))
        res <- data.frame(perdive, postdive.dur, postdive.tdist=pdv[, 1],
                          postdive.mean.vel=pdv[, 2])
        for (i in 1:3) res[, i] <- chron(res[, i])
    }

    res
}
