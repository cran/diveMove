"calibrateDepth" <-  function(x, landerr=70, seaerr=3610,
                              divethres=4, offset)
{
    ## Purpose: Detect water/land phases in TDR object, zoc data, detect
    ## 	      dives and their phases, and label them.  Return a TDRcalibrate
    ## 	      object.
    ## --------------------------------------------------------------------
    ## Arguments: x=a TDR object; landerr, seaerr and divethres (see
    ## detPhase, detDive, and labDivePhase documentation
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDR")) stop ("x is not a TDR object")
    ## Detect trips and dives
    detp <- detPhase(getTime(x), getDepth(x), landerr=landerr,
                     seaerr=seaerr, getDtime(x))
    zd <- if (missing(offset)) {
        zoc(getTime(x), getDepth(x))
    } else zoc(getTime(x), getDepth(x), offset=offset)
    if (!is.null(zd)) x@depth <- zd
    detd <- detDive(getDepth(x), detp[[2]], divethres, getDtime(x))

    ## label phases of dives with their activity
    phaselabs <- labDivePhase(x, detd[, 1])

    new("TDRcalibrate",
        tdr=x,
        gross.activity=detp,
        dive.activity=detd,
        dive.phases=phaselabs,
        land.threshold=landerr,
        sea.threshold=seaerr,
        dive.threshold=divethres)
}


"calibrateVel" <- function(x, type="all", calType="pooled", bad=c(0, 0), z=0,
                           filename=slot(getTDR(x), "file"), coefs, ...)
{
    ## Value: TDRcalibrate object with calibrated velocity and calibration
    ## coefficients
    ## --------------------------------------------------------------------
    ## Arguments: x=a TDRcalibrate object; type, z, calType, bad,
    ## filename, ... see doVelCalib.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x@tdr, "TDRvel")) {
        stop ("tdr slot in x is not a TDRvel object")
    }
    tt <- getTDR(x)
    if (!missing(coefs)) {
        vel <- (getVeloc(tt) - coefs[1]) / coefs[2]
        x@tdr@velocity <- vel
        x@vel.calib.coefs <- coefs
        x
    } else {
        ddepthvel <- .getVelCalib(time=getTime(tt),
                                  zdepth=getDepth(tt),
                                  vel=getVeloc(tt),
                                  dives=getDAct(x),
                                  phase=getDPhaseLab(x),
                                  type=type, z=z,
                                  interval=getDtime(tt))

        calibrate <- doVelCalib(ddepthvel, vel=getVeloc(tt),
                                calType=calType, bad=bad,
                                filename=filename, ...)

        if (calType != "none") {
            x@tdr@velocity <- calibrate[[2]]
            x@vel.calib.coefs <- calibrate[[1]]
        }
        x
    }
}
