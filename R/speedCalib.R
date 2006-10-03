## GETTING THE DATA #######################################################

".descAsc" <- function(x, phase, type=c("all", "strict"),
                       interval, z=0)
{
    ## Value: 2-element list with rate of depth change (m/s) and speed
    ## (original units) for descent and ascent.
    ## --------------------------------------------------------------------
    ## Arguments: x=4-col matrix with a single dive (id, time, depth,
    ## speed) phase=factor labelling each row for its phase in dive
    ## interval=sampling interval in seconds; z=minimum depth differences
    ## to use.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    type <- match.arg(type)
    ## define descent, bottom, and ascent submatrices
    switch(type,
           strict={
               desc <- x[grep("^D$", as.character(phase)), ]
               asc <- x[grep("^A$", as.character(phase)), ]},
           all={
               desc <- x[grep("D", as.character(phase)), ]
               asc <- x[grep("A", as.character(phase)), ]})

    phases <- list(descent=desc, ascent=asc)

    bkdive <- lapply(phases, function(k) {
        if (nrow(k) > 2) {
            difftim <- (k[nrow(k), 2] - k[1, 2]) + interval
            diffdep <- max(k[, 3], na.rm=TRUE)
            if (diffdep > z) {
                dratedep <- diffdep / difftim
                mspeed <- mean(k[-1, 4], na.rm=TRUE)
                cbind(dive.id=unique(k[, 1]), dratedep, mspeed)
            }
        }
    })

    ## ## variation using all sequential differences
    ## bkdive <- lapply(phases, function(k) {
    ##   if (nrow(k) > 2) {                  # move on only if we have > 2 rows
    ##     difftim <- diff(k[, 2])           # time differences
    ##     diffdep <- diff(k[, 3])           # depth differences
    ##     dratedep <- diffdep / (difftim * 86400) # rates of depth change
    ##     mspeed <- k[-1, 4]                  # mean speeds for the above
    ##     cbind(dive.id=unique(k[, 1]), dratedep, mspeed)
    ##   }
    ## })

    bkdive
}


".getSpeedCalib" <- function(time, zdepth, speed, dives, phase, ...)
{
    ## Value: A 2-element list with rate of depth change and speed for
    ## ascent and descent phases. Provides the data to perform calibration
    ## of speed.
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct; zdepth=corrected depth m; speed=speed in
    ## m/s; dives=3-col matrix as returned by detDive(); phase=factor
    ## dividing dive into sections (see labDivePhase) ...=passed to
    ## .descAsc (type, interval (in seconds), and z)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## required diving indices
    ok <- which(dives[, 2] == "D")
    dtimes <- time[ok]                    # diving times
    ddepth <- zdepth[ok]                  # diving depths
    dspeed <- speed[ok]                   # diving speeds
    dphase <- phase[ok]                   # diving phases
    dids <- dives[ok, 1]                  # dive IDs

    ## time-depth-speed mtx, with time in numeric (seconds)
    tdv <- matrix(c(dids, dtimes, ddepth, dspeed), ncol=4)

    perdive <- by(tdv, tdv[, 1], function(k) {
        curdphase <- dphase[dids == k[1, 1]]
        .descAsc(k, curdphase, ...)
    })

    descent <- do.call("rbind", lapply(perdive, "[[", "descent"))
    ascent <- do.call("rbind", lapply(perdive, "[[", "ascent"))

    list(descent=descent, ascent=ascent)
}


## CALIBRATING AND PLOTTING ###############################################

"rqPlot" <- function(rdepth, speed, rqFit, main="qtRegression",
                     xlab="rate of depth change (m/s)", ylab="speed (m/s)",
                     colramp=colorRampPalette(c("white", "darkblue")))
{
    ## Value: A quantile regression plot for TDR speed calibration
    ## --------------------------------------------------------------------
    ## Arguments: rdepth=rate of depth change (m), speed=speed (m/s),
    ## rqFit=quantile regression fit object; main=title to display in the
    ## plot
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Bandwidths for the x and y axis for the kernel plot
    bandw <- c(bw.nrd(rdepth), bw.nrd(speed))
    z <- bkde2D(cbind(rdepth, speed), bandwidth=bandw)

    axlims <- range(rdepth, speed, na.rm=TRUE)
    par(pty="s")
    image(z$x1, z$x2, z$fhat, xlim=axlims, ylim=axlims, col=colramp(256),
          main=main, xlab=xlab, ylab=ylab, cex.lab=1.3, las=1)
    box()
    points(rdepth, speed, pch=".")
    abline(rqFit)
    abline(0, 1, lty=2)
    mtext(bquote(y == .(round(coef(rqFit)[1], 3)) +
                 .(round(coef(rqFit)[2], 3)) * x))
}


"doSpeedCalib" <- function(rates, speed, calType="pooled", bad=c(0, 0),
                           filename, postscript=FALSE, ...)
{
    ## Value: Quantile regressions of speed on rate of depth change
    ## --------------------------------------------------------------------
    ## Arguments: rates=list returned from .getSpeedCalib,
    ## speed=vector with uncalibrated speeds,
    ## calType=string ("ascent", "descent", or "pooled") which one?
    ## bad=vector (length 2) with lower threshold rate of depth change and
    ## speed, respectively, below which data should be ignored, respectively,
    ## below which data should be excluded,
    ## filename=base name for output eps files, ...=for rqPlot (colramp, etc.)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    require(KernSmooth) || stop("KernSmooth package is required for calibrations")
    require(quantreg) || stop("quantreg package is required for calibrations")

    desc <- na.omit(rates[[1]])
    asc <- na.omit(rates[[2]])
    ## Subset rates of depth change > 0, > specified, and speed > specified
    desc <- desc[desc[, 2] > 0 & desc[, 2] > bad[1] & desc[, 3] > bad[2], ]
    asc <- asc[asc[, 2] > 0 & asc[, 2] > bad[1] & asc[, 3] > bad[2], ]
    pooled <- rbind(desc, asc)
    full <- list(descent=desc, ascent=asc, pooled=pooled)
    fullD <- lapply(full, function(x) apply(x[, 2:3], 2, density, "nrd"))
    fullDx <- range(sapply(fullD, function(x) sapply(x, function(k) range(k$x))))
    fullDy <- range(sapply(fullD, function(x) sapply(x, function(k) range(k$y))))

    prefix <- gsub("(.*).csv", "\\1", filename)
    if (postscript) {
        outfile <- paste(prefix, "_speedcal.eps", sep="")
        postscript(outfile, paper="special", width=6, height=6,
                   horizontal=FALSE, ## pointsize=14,
                   title=paste(prefix, "speed calibration"))
    }
    layout(matrix(c(1, 2, 4, 3), 2, 2), respect=TRUE)
    par(mar=c(4.5, 2, 3.5, 0))
    for (i in names(full)) {
        ratedep <- full[[i]][, 2]
        mspeed <- full[[i]][, 3]
        rqfit <- rq(mspeed ~ ratedep, tau=0.02, na.action=na.omit)
        rqPlot(ratedep, mspeed, rqfit, paste(prefix, "--", i), ...)
        if (i == calType) {
            corrSpeed <- (speed - coef(rqfit)[1]) / coef(rqfit)[2]
            rqcalibs <- list(coefficients=coef(rqfit), corrSpeed=corrSpeed)
        }
    }
    plot(fullDx, fullDy, type="n", xlab="x",
         ylab="density", cex.lab=1.3, las=1)
    leg.lty <- 3:1
    for (i in seq(along=fullD)) {
        cur.d <- fullD[[i]]
        lty <- leg.lty[i]
        lines(cur.d[[1]], lty=lty)
        lines(cur.d[[2]], lty=lty, col=2)
    }
    leg.t <- paste(rep(names(fullD), 2), rep(c("vert. v", "TDR v"), each=3))
    legend("topright", leg.t, lty=rep(leg.lty, 2), col=rep(1:2, each=3),
           cex=0.6, bty="n", x.intersp=0.6)
    if (postscript) dev.off()
    if (calType != "none") rqcalibs
}


## TEST ZONE #########################################################
