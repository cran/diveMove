"grpSpeedFilter" <- function(x, speedthres, window=5)
{
    ## Value: Do stage one on matrix x (assuming it's a single unit),
    ## return a logical; whether each observation in x failed the test
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: POSIXct, lon, lat; speedthres=speed
    ## threshold (m/s), window=size of window to test
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    if (nrow(x) < window) stop ("there are fewer rows than window size")
    tpos <- window %/% 2                # test subscript - 1
    testfun <- function(k) {
        mid <- tpos + 1                 # subscript of pt to test
        ref <- c(-seq(tpos), seq(tpos)) # subscripts of pts to test against
        speeds <- numeric(length(ref))
        for (n in ref) speeds[which(ref == n)] <- distSpeed(k[mid, ], k[n, ])[3]
        all(speeds > speedthres, na.rm=TRUE)
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


"rmsDistFilter" <- function(x, speedthres, window=5, distthres)
{
    ## Value: Apply McConnell et al's filter and Austin et al's last
    ## stage, return 2-col matrix of logicals; whether each observation
    ## failed each test.  These 2 filters are independent of each other.
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with cols: POSIXct, lon, lat; speedthres=speed
    ## threshold (m/s), window=size of window to test; distthres=distance
    ## threshold (km)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!window %% 2) stop ("window size must be an odd number")
    if (nrow(x) < window) stop ("fewer rows than window size")
    tpos <- window %/% 2                      # test subscript - 1
    testrows <- seq(1 + tpos, nrow(x) - tpos) # subscripts of pts to test
    testmtx <- x[testrows, 1:3]               # we test all these points

    ## Mcconnell et al filter
    ref <- c(-seq(tpos), seq(tpos))     # reference points for test
    ## define matrices where we'll store speed and distance
    ## one column per test (e.g. if window=5 -- 4 cols)
    speeds <- matrix(nrow=length(testrows), ncol=length(ref))
    dists <- speeds
    for (i in ref) {                    # do this for all tests needed
        travel <- distSpeed(x[testrows + i, 1:3], testmtx)
        curcol <- which(ref == i)       # subscript of current col
        speeds[, curcol] <- travel[, 3]
        dists[, curcol] <- travel[, 1]
    }
    ## root mean square value
    rms <- apply(speeds, 1, function(k) { # do this for every row of speeds
        sqrt(sum(k ^ 2, na.rm=TRUE) / length(k)) # divide by the no cols
    })                                           # we get vector
    failv <- rms > speedthres
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
                         speedthres, distthres, window=5)
{
    ## Value: A matrix with logicals indicating whether each reading
    ## failed each filter.  This runs the filters in Austin et al. (2003).
    ## Results are presented from each filter, independently of the others
    ## for flexibility.
    ## --------------------------------------------------------------------
    ## Arguments: lat and lon=latitude and longitude vectors in degrees;
    ## time=POSIXct object with times for each point; id=factor
    ## identifying sections of the data to be treated separately;
    ## speedthres=speed threshold (m/s); distthres=distance threshold (km);
    ## window=size of window to test
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## FIRST STAGE
    ## ********************************************************
    locs <- data.frame(time, lon, lat)

    ## Do first stage over each seal's data, returns vector as long as locs
    first <- unlist(by(locs, id, grpSpeedFilter, speedthres, window),
                    use.names=FALSE)

    ## SECOND AND THIRD STAGES ********************************************
    good <- which(!first)               # native subscripts that passed
    last <- do.call(rbind, by(locs[good, ], id[good], rmsDistFilter,
                              speedthres, window, distthres))
    filter23 <- logical(length(first))
    filter123 <- cbind(firstFail=first,
                       secondFail=filter23,
                       thirdFail=filter23)
    filter123[good, 2:3] <- last
    filter123
}

## TEST ZONE --------------------------------------------------------------
