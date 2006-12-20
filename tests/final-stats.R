library(diveMove)

(sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                             package="diveMove"), concurrentCols=4:6))
(dcalib <- calibrateDepth(sealX, offset=3))
tdrstats <- diveStats(dcalib)
head(tdrstats)
head(stamps <- stampDive(dcalib))
(att <- attendance(dcalib, FALSE))
(att <- attendance(dcalib, TRUE))

(sealX <- readTDR(system.file(file.path("data", "sealMK8.csv"),
                             package="diveMove"),
                  speed=TRUE, concurrentCols=4:6))
(dcalib <- calibrateDepth(sealX, offset=3))
(vcalib <- calibrateSpeed(dcalib, z=1))
tdrstats <- diveStats(vcalib)
head(tdrstats)
head(stamps <- stampDive(vcalib))
(att <- attendance(vcalib, FALSE))
(att <- attendance(vcalib, TRUE))
