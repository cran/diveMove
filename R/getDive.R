"getDive" <- function(x, interval, vel=FALSE)
{
    ## Purpose: Get time/depth stats for each dive segment
    ## --------------------------------------------------------------------
    ## Arguments: x=a matrix with data for a single dive
    ## interval=sampling interval
    ## vel=logical; should we calculate velocity stats?
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## define descent, bottom, and ascent submatrices
    desc <- x[grep("D", as.character(x[, 1])), 2:ncol(x)]
    bott <- x[grep("B", as.character(x[, 1])), 2:ncol(x)]
    asc <- x[grep("A", as.character(x[, 1])), 2:ncol(x)]

    ## DESCENT
    begdesc <- desc[1, 1]
    enddesc <- desc[nrow(desc), 1]
    desctim <- enddesc - begdesc + interval / 2
    descdist <- max(desc[, 2], na.rm=TRUE)
    ## BOTTOM
    if (nrow(bott) > 0) {
        botttim <- bott[nrow(bott), 1] - bott[1, 1]
        bottdist <- sum(abs(diff(bott[!is.na(bott[, 2]), 2])))
    }
    ## ASCENT
    begasc <- asc[1, 1]
    asctim <- asc[nrow(asc), 1] - begasc + interval/2
    ascdist <- max(asc[, 2], na.rm=TRUE)
    ## DIVE DURATION
    divetim <- ifelse(exists("botttim"),
                      desctim + botttim + asctim,
                      desctim + asctim)
    ## MAXIMUM DIVE DEPTH
    maxdep <- max(x[, 3], na.rm=TRUE)

    if (!vel) {
        cbind(begdesc=begdesc, enddesc=enddesc, begasc=begasc,
              desctim=desctim * 86400,
              botttim=ifelse(exists("botttim"), botttim * 86400, NA),
              asctim=asctim * 86400, descdist=descdist,
              bottdist=ifelse(exists("bottdist"), bottdist, NA),
              ascdist=ascdist, divetim=divetim * 86400, maxdep=maxdep)
    } else {
        descv <- .getVelStats(desc[, -2], vdist=descdist)
        bottv <- .getVelStats(bott[, -2])
        ascv <- .getVelStats(asc[, -2], vdist=ascdist)
        cbind(begdesc=begdesc, enddesc=enddesc, begasc=begasc,
              desctim=desctim * 86400,
              botttim=if (exists("botttim")) botttim * 86400 else NA,
              asctim=asctim * 86400, descdist=descdist,
              bottdist=if (exists("bottdist")) bottdist else NA,
              ascdist=ascdist, desc.tdist=descv[, 1],
              desc.mean.vel=descv[, 2], desc.angle=descv[, 3],
              bott.tdist=bottv[, 1], bott.mean.vel=bottv[, 2],
              asc.tdist=ascv[, 1], asc.mean.vel=ascv[, 2],
              asc.angle=ascv[, 3], divetim=divetim * 86400,
              maxdep=maxdep)
    }
}

## TEST ZONE ---------------------------------------------------------
