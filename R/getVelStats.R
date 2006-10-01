## This one simply uses the mean velocity to calculate the angles and tdist
".getVelStats" <- function(x, vdist)
{
    ## Purpose: Get velocity related statistics on a section of a dive
    ## --------------------------------------------------------------------
    ## Arguments: x=matrix with a dive's section data (time, vel);
    ## vdist=vertical distance travelled during this time
    ## If vdist is missing, then it's all horizontal movements (no angles)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (nrow(x) > 1) {
        vel <- x[-1, 2]
        time <- x[, 1]
        difft <- diff(as.numeric(time)) # seconds
        mvel <- mean(vel, na.rm=TRUE)
        tdist <- sum(difft * vel, na.rm=TRUE)
        if (!missing(vdist)) {
            angle <- asin(ifelse(vdist < tdist, vdist/tdist, NA)) * (180 / pi)
            cbind(tdist=tdist, mean.vel=mvel, angle=angle)
        } else {
            cbind(tdist=tdist, mean.vel=mvel, angle=NA)
        }
    } else {
        matrix(ncol=3)
    }
}
