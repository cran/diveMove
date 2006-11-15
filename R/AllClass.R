## CLASSES
setClass("TDR",
         representation=representation(file="character", dtime="numeric",
             time="POSIXct", depth="numeric"),
         validity=function(object) {
             if (length(object@time) != length(object@depth)) {
                 return("depth and time must have equal lengths")
             }
             if (!slot(object, "dtime")) return("dtime cannot be missing")
         })

setClass("TDRspeed",
         representation=representation("TDR", speed="numeric"),
         contains="TDR",
         validity=function(object) {
             speed.len <- length(slot(object, "speed"))
             tim.len <- length(slot(object, "time"))
             if (speed.len != tim.len) {
                 return("speed and time must have equal lengths")
             }
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
