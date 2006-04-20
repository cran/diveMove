"plotDive" <- function(time, depth, vel=NULL, xlim=NULL, phaseCol=NULL)
{
    ## Purpose: Interactive plot of time, depth, velocity, mostly for ZOC.
    ## 	      Returns a list with coordinates for each zoc'ed time window.
    ## --------------------------------------------------------------------
    ## Arguments: time=chron object with date and time;
    ## depth and vel=numeric vectors of depth and velocity;
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    require(tcltk) || stop("tcltk support is absent")
    require(chron) || stop("chron is required to handle time")
    rx <- range(as.numeric(time))     # max and min of dates
    xlim <- x10 <- if(is.null(xlim)) {    # define xlim if not there already
        rx + (diff(rx) * 0.01)              # add 1% to each side
    } else {
        as.numeric(xlim)
    }
    xlmid <- xm0 <- mean(xlim)            # two vars with date range midpoint
    xr0 <- diff(xlim)                     # range of xlim
    xZoom <- tclVar(100)                  # initialize zoom factor
    xlmid <- tclVar(xlmid)                # initialize date range midpoint
    xZ <- as.numeric(tclvalue(xZoom))     # these 2 are to be dynamically changed
    xM <- as.numeric(tclvalue(xlmid))
    ylim <- c(max(depth, na.rm=TRUE), min(depth, na.rm=TRUE))
    yMax <- tclVar(ylim[1])
    yTop <- as.numeric(tclvalue(yMax))
    replot <- function(...) {
        if(is.null(vel)) {
            par(lab=c(10, 10, 7), las=1, xaxs="i", cex.axis=0.7,
                cex.lab=1.3, bty="n", mar=c(4, 4, 1, 1) + 0.1)
        } else {
            par(lab=c(10, 10, 7), las=1, xaxs="i", cex.axis=0.7,
                cex.lab=1.3, bty="n", mar=c(4, 4, 0, 1) + 0.1)
            layout(matrix(c(2, 1), nrow=2, ncol=1), heights=c(0.7, 1.5))
        }
        xZ <<- as.numeric(tclvalue(xZoom))
        xM <<- as.numeric(tclvalue(xlmid))
        xr.half <- (xr0/2) * 100/xZ
        xlim <- xM + c(-xr.half, xr.half)
        xticks <- seq(from=xlim[1], to=xlim[2], length.out=20)
        xticks <- chron(xticks)
        xlabels <- gsub("[(]([0-9]+/[0-9]+)/[0-9]+ ([0-9]+:[0-9]+):[0-9]+.*",
                        "\\1 \\2", as.character(xticks))
        yTop <<- as.numeric(tclvalue(yMax))
        ylim <- c(yTop, ylim[2])
        morn <- chron(dates=unique(dates(time)),
                      times=rep("06:00:00",
                          times=length(unique(dates(time))))) + 1
        night <- chron(dates=unique(dates(time)),
                       times=rep("18:00:00",
                           times=length(unique(dates(time)))))
        plot(depth ~ time, type="n", xlim=xlim, ylim=ylim,
             xlab="date (dd/mm hh:mm)",
             ylab="depth (m)", xaxt="n", yaxt="n")
        usr <- par("usr")
        rect(night, usr[3], morn, usr[4], col="black")
        axis(side=1, lwd=2, at=xticks, labels=xlabels)
        axis(side=2, lwd=2)
        lines(time, depth, col="blue")
        if (!is.null(phaseCol)) {
            colors <- rainbow(nlevels(phaseCol))
            points(time, depth, col=colors[phaseCol], pch=19, cex=0.4)
            legend("bottomright", legend=levels(phaseCol), col=colors,
                   pch=19, cex=0.7, ncol=nlevels(phaseCol), bg="white")
        }
        grid(nx=0, ny=NULL)
        legend("bottomleft", legend=c("06:00 - 18:00", "18:00 - 06:00"),
               fill=c("white", "black"), cex=0.7, bg="white",
               y.intersp=1.2)
        if(!is.null(vel)) {
            par(mar=c(0, 4, 1, 1) + 0.1)
            ylim <- range(vel, na.rm=TRUE)
            plot(vel ~ time, type="n", xaxt="n", ylim=ylim,
                 xlab="", xlim=xlim, bty="n", ylab="velocity (m/s)")
            rect(night, usr[3], morn, usr[4], col="black")
            lines(time, vel, col="green")
            if (!is.null(phaseCol)) {           # we already have 'colors'
                points(time, vel, col=colors[phaseCol], pch=19, cex=0.4)
            }
            grid(nx=0, ny=NULL)
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
    xmid.frame <- tkframe(x.frame, relief="groove", borderwidth=5)
    dep.frame <- tkframe(x.frame, relief="groove", borderwidth=5)
    tkpack(xmid.frame, xr.frame, dep.frame, side="left", anchor="s")

    but.frame <- tkframe(base)
    zoc.pts <- tkbutton(but.frame, text="ZOC a range",
                        command=zocrange)
    q.but <- tkbutton(but.frame, text="      Quit       ",
                      command=function() tkdestroy(base))
    tkpack(zoc.pts, q.but, side="left", padx=20)

    tkpack(base.frame, x.frame, but.frame)

    ## Zoom
    tkpack(tklabel(xr.frame, text="Zoom in Date [%]"))
    tkpack(tkscale(xr.frame, command=replot.maybe, from=100,
                   to=30000, showvalue=TRUE, variable=xZoom,
                   length=200, orient="horiz"))
    ## Pan
    tkpack(tklabel(xmid.frame, text="Pan through Date"))
    tkpack(tkscale(xmid.frame, command=replot.maybe,
                   from=xm0 - xr0, to=xm0 + xr0, showvalue=TRUE,
                   variable=xlmid, resolution=xr0/2000, length=200,
                   orient="horiz"))
    ## Maximum depth selection
    tkpack(tklabel(dep.frame, text="Max. depth (m)"))
    tkpack(tkscale(dep.frame, command=replot.maybe,
                   from=10, to=1000, length=150, showvalue=TRUE,
                   variable=yMax, orient="horiz"))

    if(is.null(vel)) {x11(width=12)} else {x11(width=12, height=9)}
    replot()
    tkwait.window(base)
    coords
}
