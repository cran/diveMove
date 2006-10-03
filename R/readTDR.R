".getInterval" <- function(time)
{
    ## Value: numeric; the mode of intervals between time readings
    ## --------------------------------------------------------------------
    ## Arguments: time=POSIXct
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    stopifnot(all(!is.na(time)))
    if (length(time) < 2) {
        interval <- 0
    } else {
        tab <- table(difftime(time[-1], time[-length(time)],
                              units="secs", tz="GMT"))
        interval <- as.numeric(names(tab[which.max(tab)]))
    }
    interval
}


"readTDR" <- function(file, dateCol=1, timeCol=2, depthCol=3, speedCol=6,
                      subsamp=5, dtformat="%d/%m/%Y %H:%M:%S", tz="GMT")
{
    ## Value: TDR or TDRspeed object from *.csv file
    ## --------------------------------------------------------------------
    ## Arguments: file=path to file to read; dateCol=col no. with date,
    ## timeCol=col no. with time, depthCol=col no. with depth,
    ## speedCol=col no. with speed, subsamp=subsample at this interval,
    ## dtformat=format to interpret the pasted date and time columns,
    ## tz=time zone to assume
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    srcfile <- basename(file)
    tdrtype <- sub(".*(mk.).*", "\\1", tolower(srcfile))
    rawdat <- read.csv(file, header=TRUE,
                       na.strings="", as.is=TRUE)
    dtpasted <- paste(rawdat[, dateCol], rawdat[, timeCol])
    datetime <- as.POSIXct(strptime(dtpasted, format=dtformat), tz=tz)
    origint <- .getInterval(datetime)
    if(!identical(all.equal(origint, subsamp), TRUE)) {
        steptim <- as.numeric((subsamp) / origint)
        stepind <- seq(from=1, to=length(datetime), by=round(steptim))
        datetime <- datetime[stepind]
        rawdat <- rawdat[stepind, ]
    }
    if (tdrtype != "mk8") {
        new("TDR", file=srcfile, time=datetime,
            depth=rawdat[, depthCol], dtime=.getInterval(datetime))
    } else {
        new("TDRspeed", file=srcfile, time=datetime, depth=rawdat[, depthCol],
            speed=rawdat[, speedCol], dtime=.getInterval(datetime))
    }
}
