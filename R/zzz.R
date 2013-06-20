## $Id: zzz.R 590 2013-04-03 15:24:46Z sluque $

## ".onLoad" <- function(lib, pkg) {
##     packageStartupMessage("diveMove ",
##                           utils::packageVersion("diveMove"),
##                           " loaded")
## }

".onAttach" <- function(lib, pkg)
{
    version <- utils::packageVersion("diveMove")
    packageStartupMessage("This is diveMove ", version,
                          ". For overview type vignette(\"diveMove\")",
                          appendLF=TRUE)
}
