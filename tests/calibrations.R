library(diveMove)

(sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                             package="diveMove"),
                  concurrentCols=4:6, speed=TRUE))
(dcalib <- calibrateDepth(sealX, landerr=3610, offset=3))
(dcalib <- calibrateDepth(sealX, offset=3))

(vcalib <- calibrateSpeed(dcalib, calType="ascent"))
(vcalib <- calibrateSpeed(dcalib))
