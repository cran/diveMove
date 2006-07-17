"getAct" <- function(time, act, interval)
{
    ## Purpose: Calculate durations of activity phases
    ## --------------------------------------------------------------------
    ## Arguments: time=chron object with date and time;
    ## act=factor representing activity for each row
    ## interval=sampling interval (d) in chron units
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Lagged differences in activity; 0=no change from previous activity
    diffact <- diff(as.numeric(act))
    ## vector defining the points where activity changes
    ## First element=first time - first lagged time difference, followed by
    ## all times when activity changed, followed by
    ## last element=last time + first lagged difference
    timbr <- c(time[1] - interval,
               time[which(diffact != 0)],
               time[length(time)] + interval)
    ## Create the factor to break all sequences with the same activity
    cuttimbr <- cut(time, br=timbr)
    timsplit <- split(time, cuttimbr)
    begtim <- sapply(timsplit, "[", 1)
    endtim <- sapply(timsplit, function(x) {x[length(x)]})
    duration <- endtim - begtim + interval

    list(time.br=cuttimbr,
         time.peract=duration,
         beg.time=begtim,
         end.time=endtim)
}

"detPhase" <- function(time, depth, landerr=65, seaerr=3605, ...)
{
    ## Purpose: Detect phases of activity in TDR record
    ## --------------------------------------------------------------------
    ## Arguments: time=chron vector with date/time
    ## depth=numeric vector with depth readings (m)
    ## ...=sampling interval in chron units (d), to pass to getAct
    ## landerr=duration (in s) of on-land readings that should be at-sea
    ## aquaerr=duration (in s) of at-sea readings to be taken as leisure
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Define a factor with default "land" values to code activity
    ## levels: L=land, W=wet (at-sea), U=underwater (0 - dive threshold),
    ## D=diving, Z=wet (leisure)
    act <- factor(rep("L", length(time)), levels=c("L", "W", "U", "D", "Z"))
    ## 10's when animal is wet; i.e. when depth is being recorded
    act[!is.na(depth)] <- "W"

    ## First run calculates times in each activity phase from the raw data
    rawacts <- getAct(time, act, ...)
    ## Correct activity according to land error (landerr); on-land
    ## activity for less than a minute (60 s) is error, so animal is considered
    ## to be still at sea
    land <- rawacts[[2]][rawacts[[2]] < landerr/86400]
    act[rawacts[[1]] %in% names(land) & act == "L"] <- "W"

    ## Second run for correcting at-sea phases that are shorter than a threshold
    ## that should be considered as leisure (3600 s)
    leiacts <- getAct(time, act, ...)
    leisure <- leiacts[[2]][leiacts[[2]] < seaerr/86400]
    act[leiacts[[1]] %in% names(leisure) & act == "W"] <- "Z"

    ## Final run to determine times with all corrected activities
    finacts <- getAct(time, act, ...)
    nphase <- length(levels(finacts[[1]]))

    if(act[1] == "L" & act[length(act)] == "L") {
        message("Record is complete\n", nphase, " phases detected")
    } else {
        if(act[1] != "L" & act[length(act)] != "L") {
            message("Record is truncated at the beginning and at the end\n",
                    nphase, " phases detected")
        } else {
            if(act[1] != "L") {
                message("Record is truncated at the beginning\n", nphase,
                        " phases detected")
            } else {
                message("Record is truncated at the end\n", nphase,
                        " phases detected")
            }
        }
    }

    indphases <- as.numeric(finacts[[1]])
    names(finacts[[3]]) <- seq(length(names(finacts[[3]])))
    names(finacts[[4]]) <- seq(length(names(finacts[[4]])))

    ## Return list with vector indexing all per-row activities,
    ## a vector with the activities themselves, two chron vectors
    ## with the starting and ending times for each phase
    list(phase.id=indphases,
         trip.act=act,
         trip.beg=finacts[[3]],
         trip.end=finacts[[4]])
}
