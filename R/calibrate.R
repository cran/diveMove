"calibrateDepth" <-  function(x, landerr=65, seaerr=3605,
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
    ## Detect trips
    detp <- detPhase(tdrTime(x), depth(x), landerr=landerr,
                     seaerr=seaerr, dtime(x))
    zd <- if (missing(offset)) {
        zoc(tdrTime(x), depth(x))
    } else zoc(tdrTime(x), depth(x), offset=offset)
    if (!is.null(zd)) x@depth <- zd
    detd <- detDive(tdrTime(x), depth(x), detp[[2]],
                    divethres, dtime(x))

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


"calibrateVel" <- function(x, type="all", calType="pooled",
                           bad=c(0, 0), z=0,
                           filename=slot(tdr(x), "file"), coefs, ...)
{
    ## Purpose: Calibrate velocity
    ## --------------------------------------------------------------------
    ## Arguments: x=a TDRcalibrate object; type, z, calType, bad, filename,
    ## ... see documentation for .getVelCalib and doVelCalib.  Return a
    ## TDRcalibrate object
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(slot(x, "tdr"), "TDRvel")) {
        stop ("tdr slot in x does not contain a TDRvel object")
    }
    tt <- tdr(x)
    if (!missing(coefs)) {
        vel <- (velocity(tt) - coefs[1]) / coefs[2]
        x@tdr@velocity <- vel
        x@vel.calib.coefs <- coefs
        x
    } else {
        ddepthvel <- .getVelCalib(time=tdrTime(tt),
                                  zdepth=depth(tt),
                                  vel=velocity(tt),
                                  dives=diveAct(x),
                                  phase=dPhaseLab(x),
                                  type=type, z=z,
                                  interval=dtime(tt))

        calibrate <- doVelCalib(ddepthvel, vel=velocity(tt),
                                calType=calType, bad=bad,
                                filename=filename, ...)

        if (calType != "none") {
            x@tdr@velocity <- calibrate[[2]]
            x@vel.calib.coefs <- calibrate[[1]]
        }
        x
    }
}
