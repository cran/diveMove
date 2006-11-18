## CLASSES
setClass("TDR",
         representation=representation(file="character", dtime="numeric",
             time="POSIXct", depth="numeric", concurrentData="data.frame"),
         prototype=prototype(concurrentData=data.frame()),
         validity=function(object) {
             if (length(object@time) != length(object@depth)) {
                 return("depth and time must have equal lengths")
             }
             if (!slot(object, "dtime")) return("dtime cannot be missing")
             return(TRUE)
         })

.speedNames <- c("velocity", "speed")
setClass("TDRspeed", contains="TDR",
         validity=function(object) {
             ccData <- object@concurrentData
             ccDataNames <- names(ccData)
             speedCol <- ccDataNames %in% .speedNames
             if (length(ccDataNames[speedCol]) != 1) {
                 return("speed is not available in concurrentData slot")
             } else if (!is.numeric(ccData[, speedCol])) {
                 return("speed must be of class numeric")
             }
             return(TRUE)
         })

setClass("TDRcalibrate",
         representation=representation(tdr="TDR", gross.activity="list",
             dive.activity="data.frame", dive.phases="factor",
             land.threshold="numeric", sea.threshold="numeric",
             dive.threshold="numeric", speed.calib.coefs="numeric"),
         prototype=prototype(speed.calib.coefs=c(0, 1)),
         validity=function(object) {
             if (length(slot(object, "land.threshold")) > 1) {
                 return("land.threshold must be a single number")
             }
             if (length(slot(object, "sea.threshold")) > 1) {
                 return("sea.threshold must be a single number")
             }
             if (length(slot(object, "dive.threshold")) > 1) {
                 return("dive.threshold must be a single number")
             }
             if (length(slot(object, "speed.calib.coefs")) != 2) {
                 return("speed.calib.coefs must be a length-2 vector")
             }
         })


## TEST ZONE --------------------------------------------------------------
