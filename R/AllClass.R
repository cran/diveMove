## CLASSES
setOldClass("chron")

setClass("TDR",
         representation=representation(file="character", dtime="numeric",
             time="chron", depth="numeric"),
         validity=function(object) {
             if (length(slot(object, "time")) != length(slot(object, "depth"))) {
                 return("depth and time have unequal lengths")
             }
             if (!slot(object, "dtime")) return("dtime cannot be missing")
             ## if (!slot(object, "file")) return("file cannot be missing")
         })

setClass("TDRvel",
         representation=representation("TDR", velocity="numeric"),
         contains="TDR",
         validity=function(object) {
             if (length(slot(object, "velocity")) != length(slot(object, "time"))) {
                 return("velocity and time have unequal lengths")
             }
         })

setClass("TDRcalibrate",
         representation=representation(tdr="TDR", gross.activity="list",
             dive.activity="data.frame", dive.phases="factor",
             land.threshold="numeric", sea.threshold="numeric",
             dive.threshold="numeric", vel.calib.coefs="numeric"),
         prototype=prototype(vel.calib.coefs=c(0, 1)),
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
             if (length(slot(object, "vel.calib.coefs")) != 2) {
                 return("vel.calib.coefs must be a length-2 vector")
             }
         })


## TEST ZONE --------------------------------------------------------------
