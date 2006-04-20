"stampDive" <- function(x, ignoreZ=TRUE)
{
    ## Purpose:  Stamp each dive with trip number, trip type, and trip start
    ##  	       and end time
    ## --------------------------------------------------------------------
    ## Arguments:  x=TDRcalibrate object, ignoreZ=ignore Z phases?
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDRcalibrate")) stop ("x needs to be a TDRcalibrate object")
    act <- grossAct(x, "trip.act")
    diveid <- diveAct(x, "dive.id")

    if (ignoreZ) {
        tt <- tdrTime(tdr(x))
        interval <- dtime(tdr(x))
        act[act == "Z"] <- "L"
        attlist <- getAct(tt, act, interval) # recalculate
        phaseid <- as.numeric(gsub("[[:alpha:]]* ([[:digit:]]+)",
                                   "\\1", attlist[[1]])) # what phase.id is now
    } else {
        attlist <- grossAct(x)
        phaseid <- grossAct(x, "phase.id")
    }

    beg <- rep(attlist[[3]], table(phaseid))
    end <- rep(attlist[[4]], table(phaseid))
    trip.no <- numeric(length(act))       # vector of 0s
    phaseid[act == "L"] <- 0             # phase.id on land should be 0
    ## make a sequence for phase.id > 0 from 1:number of such phases
    trip.no[act != "L"] <- rep(seq(along=table(phaseid[phaseid > 0])),
                table(phaseid[phaseid > 0]))
    ok <- match(unique(diveid[diveid > 0]), diveid) # required subscripts
    trip.no <-  trip.no[ok]
    trip.type <- act[ok]

    data.frame(trip.no, trip.type, beg=chron(beg[ok]), end=chron(end[ok]))
}
