"readLocs" <- function(file, loc.idCol, idCol, dateCol, timeCol=NULL,
                       dtformat=c("m/d/y", "h:m:s"), classCol, lonCol,
                       latCol, alt.lonCol=NULL, alt.latCol=NULL)
{
    ## Purpose: Read file with ARGOS locations and set up a data frame for
    ## further analyses.
    ## --------------------------------------------------------------------
    ## Arguments: file=quoted file name, including path, of file to read,
    ## loc.idCol=column number containing the location id,
    ## idCol=column number identifying locations belonging to different
    ## groups, dateCol=column number containing dates and, optionally, times,
    ## timeCol=optional column number containing times,
    ## latCol and lonCol=latitude and longitude column numbers, respectively,
    ## alt.latCol and alt.lonCol=alternative latitude and longitude columns,
    ## respectively, classCol=ARGOS classification,
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    srcfile <- basename(file)
    inLocs <- read.csv(file, header=TRUE, na.strings="", as.is=TRUE)

    datetime <- .createChron(inLocs[, datecol], inLocs[, timeCol],
                             dtformat=dtformat)
    ## Set up data frame with loc id, animal id, time, year, doy, period,
    ## pttid, class, newclass, lat, lon, latalt, lonalt
    locs <- data.frame(loc.id=inLocs[, loc.idCol], id=inLocs[, idCol],
                       time=datetime, lon=inLocs[, lonCol],
                       lat=inLocs[, latCol], class=inLocs[, classCol])
    if (!is.null(alt.lonCol)) locs$alt.lon <- inLocs[, alt.lonCol]
    if (!is.null(alt.latCol)) locs$alt.lat <- inLocs[, alt.latCol]
    comment(locs) <- srcfile

    locs[order(locs[, 2], locs[, 3]), ]         # sort by seal id and time
}

## TEST ZONE --------------------------------------------------------------
