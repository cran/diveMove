".createChron" <- function(date, time, dtformat=c("d/m/y", "h:m:s"))
{
    ## Purpose: Create a chron object from date and time
    ## --------------------------------------------------------------------
    ## Arguments: date=time=date and timevectors, respectively, the latter
    ## can be missing, dtformat=chron format for interpreting the above
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (is.null(time)) {
        splitdate <- strsplit(date, " ")
        chron(dates=sapply(splitdate, "[", 1),
              times=sapply(splitdate, "[", 2),
              format=dtformat)
    } else {
        chron(dates=date, times=time, format=dtformat)
    }
}


".getInterval" <- function(time)
{
    ## Purpose: Get the mode of intervals between time readings
    ## --------------------------------------------------------------------
    ## Arguments: time=chron object (no missing values allowed)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    stopifnot(all(!is.na(time)))
    if (length(time) < 2) {
        interval <- 0
    } else {
        tab <- table(diff(time))
        interval <- as.numeric(names(tab[which.max(tab)]))
    }

    interval
}


"readTDR" <- function(file, dateCol=1, timeCol=2, depthCol=3, velCol=6,
                      subsamp=5, dtformat=c("d/m/y", "h:m:s"))
{
    ## Purpose: Read *.csv file and create an object of class tdr or tdrVel
    ## --------------------------------------------------------------------
    ## Arguments: file=path to file to read;
    ## subsamp=subsample the file at this interval
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    srcfile <- basename(file)
    tdrtype <- sub(".*(mk.).*", "\\1", tolower(srcfile))
    rawdat <- read.csv(file, header=TRUE,
                       na.strings="", as.is=TRUE)

    datetime <- .createChron(rawdat[, dateCol], rawdat[, timeCol],
                             dtformat=dtformat)

    origint <- .getInterval(datetime)
    if(!identical(all.equal(origint, subsamp/86400), TRUE)) {
        steptim <- as.numeric((subsamp/86400) / origint)
        stepind <- seq(from=1, to=length(datetime), by=round(steptim))
        datetime <- datetime[stepind]
        rawdat <- rawdat[stepind, ]
    }

    if (tdrtype != "mk8") {
        new("TDR", file=srcfile, time=datetime,
            depth=rawdat[, depthCol], dtime=.getInterval(datetime))
    } else {
        new("TDRvel", file=srcfile, time=datetime, depth=rawdat[, depthCol],
            velocity=rawdat[, velCol], dtime=.getInterval(datetime))
    }
}
