"plotDive" <- function(time, depth, speed=NULL, xlim=NULL, phaseCol=NULL)
{
    ## Value: Plot of time, depth, speed, mostly for ZOC, returns
    ## (invisibly) a list with coordinates for each zoc'ed time window.
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct; depth and speed=numeric vectors of depth
    ## and speed
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    require(tcltk) || stop("tcltk support is absent")
    rx <- range(as.numeric(time))       # max and min of dates
    diffrx <- diff(rx)
    xlim <- x10 <- if(is.null(xlim)) {  # define xlim if not there already
        rx + (diffrx * 0.01)            # add 1% to each side
    } else as.numeric(xlim)
    xlmid <- xm0 <- mean(xlim)         # two vars with date range midpoint
    xr0 <- diff(xlim)                     # range of xlim
    xZoom <- tclVar(100)                  # initialize zoom factor
    xlmid <- tclVar(xlmid)                # initialize date range midpoint
    xZ <- as.numeric(tclvalue(xZoom)) # these 2 are to be dynamically changed
    xM <- as.numeric(tclvalue(xlmid))
    ylim <- rev(range(depth, na.rm=TRUE))
    yMax <- tclVar(ylim[1])
    yTop <- as.numeric(tclvalue(yMax))
    replot <- function(...) {
        if(is.null(speed)) {
            par(lab=c(10, 10, 7), las=1, xaxs="i", cex.axis=0.7,
                cex.lab=1.3, bty="n", mar=c(4, 4, 1, 1) + 0.1)
        } else {
            par(lab=c(10, 10, 7), las=1, xaxs="i", cex.axis=0.7,
                cex.lab=1.3, bty="n", mar=c(4, 4, -0.1, 1) + 0.1)
            layout(matrix(c(2, 1), nrow=2, ncol=1), heights=c(0.7, 1.5))
        }
        xZ <<- as.numeric(tclvalue(xZoom))
        xM <<- as.numeric(tclvalue(xlmid))
        xr.half <- (xr0/2) * 100/xZ
        xlim <- xM + c(-xr.half, xr.half)
        orig <- structure(0, class=class(time), tzone=attr(time, "tzone"))
        xticks <- orig + seq(from=xlim[1], to=xlim[2], length=20)
        yTop <<- as.numeric(tclvalue(yMax))
        ylim <- c(yTop, ylim[2])
        morn.uniq <- unique(format(time, format="%F 06:00:00"))
        morn <- as.POSIXct(morn.uniq, tz=attr(time, "tzone")) + 86400
        night.uniq <- unique(format(time, format="%F 18:00:00"))
        night <- as.POSIXct(night.uniq, tz=attr(time, "tzone"))
        plot(depth ~ time, type="n", xlim=xlim, ylim=ylim,
             xlab="date (mm-dd hh:mm:ss)",
             ylab="depth (m)", xaxt="n", yaxt="n")
        usr <- par("usr")
        xleft <- pmax(night, usr[1])
        xright <- pmin(morn, usr[2])
        rect(xleft, usr[3], xright, usr[4], col="black")
        axis.POSIXct(side=1, time, at=xticks, lwd=2, format="%m-%d %H:%M:%S")
        axis(side=2, lwd=2)
        lines(time, depth, col="blue")
        if (!is.null(phaseCol) & nlevels(phaseCol[drop=TRUE]) < 11) {
            phaseCol <- phaseCol[drop=TRUE]
            colors <- rainbow(nlevels(phaseCol))
            points(time, depth, col=colors[phaseCol], pch=19, cex=0.4)
            legend("bottomright", legend=levels(phaseCol), col=colors,
                   pch=19, cex=0.7, ncol=nlevels(phaseCol), bg="white")
        }
        legend("bottomleft", legend=c("18:00 - 06:00"), fill=c("black"),
               cex=0.7, bg="white", y.intersp=1.2)
        if(!is.null(speed)) {
            par(mar=c(-0.1, 4, 1, 1) + 0.1)
            ylim <- range(speed, na.rm=TRUE)
            plot(speed ~ time, type="n", xaxt="n", ylim=ylim,
                 xlab="", xlim=xlim, bty="n", ylab="speed (m/s)")
            rect(xleft, usr[3], xright, usr[4], col="black")
            lines(time, speed, col="green")
            if (!is.null(phaseCol)) {           # we already have 'colors'
                points(time, speed, col=colors[phaseCol], pch=19, cex=0.4)
            }
            axis(side=2, lwd=2)
        }
    }
    replot.maybe <- function(...) {
        if(as.numeric(tclvalue(xZoom)) != xZ ||
           as.numeric(tclvalue(xlmid)) != xM ||
           as.numeric(tclvalue(yMax)) != yTop) replot()
    }
    coords <- list()
    zocrange <- function() {
        coords[[length(coords) + 1]] <<- locator(2)
        tkgrab.release(base)
    }

    base <- tktoplevel()
    tkwm.title(base, "diveMove")
    tkwm.deiconify(base)
    tkgrab.set(base)
    tkfocus(base)

    base.frame <- tkframe(base, borderwidth=3)

    x.frame <- tkframe(base.frame)
    xr.frame <- tkframe(x.frame, relief="groove", borderwidth=5)
    dep.frame <- tkframe(x.frame, relief="groove", borderwidth=5)
    tkpack(xr.frame, dep.frame, side="left", anchor="s")

    xmid.frame <- tkframe(base.frame, relief="groove", pady=5)

    but.frame <- tkframe(base)
    zoc.pts <- tkbutton(but.frame, text="Zero-Offset Correct a Range",
                        command=zocrange)
    q.but <- tkbutton(but.frame, text="      Quit       ",
                      command=function() tkdestroy(base))
    tkpack(zoc.pts, q.but, side="left", padx=20)

    tkpack(base.frame, x.frame, xmid.frame, but.frame)

    ## Zoom
    diffx <- diff(as.numeric(time))
    diffxOK <- min(diffx[diffx > 0]) * 40 # zoom up to 40 observations
    maxZoom <- (diffrx / diffxOK) * 100 # maximum zoom depends on time range
    tkpack(tklabel(xr.frame, text="Date Zoom (%)"))
    tkpack(tkscale(xr.frame, command=replot.maybe, from=100,
                   to=maxZoom, showvalue=TRUE, variable=xZoom,
                   length=200, orient="horiz"))
    ## Pan
    tkpack(tklabel(xmid.frame, text="Pan through Date"))
    tkpack(tkscale(xmid.frame, command=replot.maybe,
                   from=xm0 - xr0, to=xm0 + xr0, showvalue=FALSE,
                   variable=xlmid, resolution=xr0/2000, length=200,
                   orient="horiz"))
    ## Maximum depth selection
    tkpack(tklabel(dep.frame, text="Max. Depth (m)"))
    tkpack(tkscale(dep.frame, command=replot.maybe,
                   from=0, to=max(depth, na.rm=TRUE), length=150,
                   showvalue=TRUE, variable=yMax, orient="horiz"))

    replot()
    tkwait.window(base)
    invisible(coords)
}
