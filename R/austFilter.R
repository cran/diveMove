"grpSpeedFilter" <- function(x, velthres, window=5)
{
    ## Purpose: Do stage one on matrix x (assuming it's a single unit)
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: chron, lon, lat
    ## velthres=velocity threshold, window=size of window to test
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    if (nrow(x) < window) stop ("there are fewer rows than window size")
    tpos <- window %/% 2                  # test subscript - 1
    testfun <- function(k) {
        mid <- tpos + 1                     # subscript of pt to test
        ref <- c(-seq(tpos), seq(tpos))     # subscripts of pts to test against
        vels <- numeric(length(ref))
        for (n in ref) vels[which(ref == n)] <- distSpeed(k[mid, ], k[n, ])[3]
        all(vels > velthres, na.rm=TRUE)
    }
    failed <- logical(nrow(x))
    ## define all test rows and subscript for forward movement of window
    testrows <- seq(1 + tpos, nrow(x) - tpos); i <- 1
    for (j in testrows) {
        if(testfun(x[c(i:(i + tpos - 1), j:(j + tpos)), ])) {
            failed[j] <- TRUE
        } else {
            i <- i + 1
        }
    }
    failed
}


"rmsDistFilter" <- function(x, velthres, window=5, distthres)
{
    ## Purpose:  Apply McConnell et al's filter and Austin et al's last stage
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: chron, lon, lat
    ## velthres=velocity threshold, window=size of window to test
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    if (nrow(x) < window) stop ("fewer rows than window size")
    tpos <- window %/% 2                  # test subscript - 1
    testrows <- seq(1 + tpos, nrow(x) - tpos) # subscripts of pts to test
    testmtx <- x[testrows, 1:3]            # we test all these points

    ## Mcconnell et al filter
    ref <- c(-seq(tpos), seq(tpos))       # reference points for test
    ## define matrices where we'll store velocity and distance
    ## one column per test (e.g. if window=5 -- 4 cols)
    vels <- matrix(nrow=length(testrows), ncol=length(ref))
    dists <- vels
    for (i in ref) {                      # do this for all tests needed
        travel <- distSpeed(x[testrows + i, 1:3], testmtx)
        curcol <- which(ref == i)           # subscript of current col
        vels[, curcol] <- travel[, 3]
        dists[, curcol] <- travel[, 1]
    }
    ## root mean square value
    rms <- apply(vels, 1, function(k) {   # do this for every row of velocities
        sqrt(sum(k ^ 2, na.rm=TRUE) / length(k)) # divide by the no cols
    })                                           # we get vector
    failv <- rms > velthres
    rmsFail <- logical(nrow(x))
    rmsFail[testrows] <- failv

    ## Distance filter
    distt <- apply(dists, 1, function(k) sum(k, na.rm=TRUE) / length(k))
    faild <- distt > distthres
    distFail <- logical(nrow(x))
    distFail[testrows] <- faild

    cbind(rmsFail, distFail)
}


"austFilter" <- function(time, lon, lat, id=gl(1, 1, length(time)),
                         velthres, distthres, window=5)
{
    ## Purpose: Run the filters in Austin et al. (2003)
    ## --------------------------------------------------------------------
    ## Arguments: lat and lon=latitude and longitude vectors in degrees;
    ## time=chron object with times for each point; id=factor identifying
    ## sections of the data to be treated separately;
    ## velthres=speed threshold (m/s); distthres=distance threshold (km)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## FIRST STAGE ********************************************************
    ## We split the data frame according to ID
    locs <- matrix(c(time, lon, lat), ncol=3)

    ## Do first stage over each seal's data, returns vector as long as locs
    first <- unlist(by(locs, id, grpSpeedFilter, velthres, window),
                    use.names=FALSE)

    ## SECOND AND THIRD STAGES ********************************************
    good <- which(!first)                 # native subscripts that passed

    last <- do.call(rbind, by(locs[good, ], id[good], rmsDistFilter,
                              velthres, window, distthres))
    filter23 <- logical(length(first))

    filter123 <- cbind(firstFail=first,
                       secondFail=filter23,
                       thirdFail=filter23)
    filter123[good, 2:3] <- last
    filter123
}

## TEST ZONE --------------------------------------------------------------
