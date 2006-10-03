"labDive" <- function(act, string, interval)
{
    ## Value: Label dives along vector of same length as input.  Return a
    ## matrix labelling each dive and postdive reading
    ## --------------------------------------------------------------------
    ## Arguments: act=factor with values to label, string=character string
    ## to search in act to be labelled sequentially, interval=sampling
    ## interval in the input
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    dive <- vector(mode="numeric", length=length(act))
    dive[act == string] <- 1
    runs <- rle(dive)
    rawid <- rep(seq(runs$lengths), runs$lengths)

    diveid <- rawid
    diveid[dive == 0] <- 0               # non-dive are 0, and dives conseq:
    diveid[dive != 0] <- rep(seq(along=table(diveid)[-1]), table(diveid)[-1])
    pdid <- numeric(length(rawid))        # dives are 0 and postdives conseq:
    pdinds <- rawid %in% (unique(rawid[dive == 1]) + 1)
    pdid[pdinds] <- rep(seq(along=table(rawid[pdinds])), table(rawid[pdinds]))
    cbind(dive.id=diveid, postdive.id=pdid)
}


"detDive" <- function(zdepth, act, divethres=4, ...)
{
    ## Value: A data frame; detecting dives, using a depth threshold
    ## --------------------------------------------------------------------
    ## Arguments: zdepth=depth vector of zoc'ed data, act=factor with
    ## land/sea activity IDs (2nd element returned by detPhase), with
    ## values "W" for at-sea, divethres=dive threshold in m/s ...=sampling
    ## interval in (s), to pass to labDive
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Get the indices of below surface activity and label as "U"
    underw <- which(act == "W" & zdepth > 0)
    act[underw] <- "U"

    labuw <- labDive(act, "U", ...) # label underwater excursions
    ## Max depth of each "U" phase
    uwmax <- tapply(zdepth[underw], labuw[underw, 1], max, na.rm=TRUE)
    ## Change each "U" phase to "D" if its max depth > dive threshold
    act[labuw[, 1] %in% as.numeric(names(uwmax[uwmax > divethres]))] <- "D"

    inddive <- labDive(act, "D", ...)
    ndives <- length(unique(inddive[act == "D", 1]))
    message(ndives, " dives detected")

    ## Return data frame with vectors of dive indices, adjusted activity,
    ## and postdive indices
    data.frame(dive.id=inddive[, 1], dive.activity=act,
               postdive.id=inddive[, 2])
}


".cutDive" <- function(x)
{
    ## Value: Create a factor that breaks a dive into descent,
    ## descent/bottom, bottom, bottom/ascent, ascent, and/or
    ## descent/ascent given a proportion of maximum depth for bottom time.
    ## Return a character matrix with orig ID and corresponding label.
    ## --------------------------------------------------------------------
    ## Arguments: x=a 2-col matrix with index in original TDR object and
    ## non NA depths.  A single dive's data.
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    ## Descent detection
    descdd <- diff(x[, 2])              # sequential differences
    ## subscript of diffs <= 0 -- no further increase in depth
    descinf <- which(descdd <= 0)
    ## descent ends at inflection point, or first row if non detected We
    ## allow for a maximum of 1 histeresis event at the beginning and end
    ## of dive
    descind <- if (length(descinf) > 0) {
        if (length(descinf) > 1 & descinf[2] != descinf[1] + 1) {
            1:descinf[2]
        } else {
            1:descinf[1]
        }
    } else 1

    ## Ascent detection
    ascdd <- diff(rev(x[, 2]))            # we do it from end of dive
    ascinf <- which(ascdd <= 0)           # same as above
    ascind <- if (length(ascinf) > 0) {
        if (length(ascinf) > 1 & ascinf[2] != ascinf[1] + 1) {
            (nrow(x) - ascinf[2] + 1):nrow(x)
        } else {
            (nrow(x) - ascinf[1] + 1):nrow(x)
        }
    } else nrow(x)

    ## Bottom detection
    bottind <- c(descind[length(descind)],
                 setdiff(seq(length(x[, 1])), union(descind, ascind)),
                 ascind[1])

    ## descent is everything in descind that's not in union of bottind and ascind
    d <- setdiff(descind, union(bottind, ascind))
    ## descent/bottom is what's common to descind and bottind
    db <- intersect(descind, bottind)
    ## bottom is everything in bottind that's not in union of descind and ascind
    b <- setdiff(bottind, union(descind, ascind))
    ## bottom/ascent is what's common to ascind and bottind
    ba <- intersect(ascind, bottind)
    ## ascent is everything in ascind that's not in union of descind and bottind
    a <- setdiff(ascind, union(descind, bottind))
    ## descent/ascent is what's common to descind and ascind
    da <- intersect(descind, ascind)

    labs <- character(nrow(x))
    labs[d] <- "D"
    labs[db] <- "DB"
    labs[b] <- "B"
    labs[ba] <- "BA"
    labs[a] <- "A"
    labs[da] <- "DA"
    ## If there are repetitions, keep the last one to avoid missing ascent labels
    rowids <- unique(c(x[d, 1], x[db, 1], x[b, 1], x[ba, 1], x[a, 1], x[da, 1]))
    cbind(rowids, labs)
}


"labDivePhase" <- function(x, diveID)
{
    ## Value: A factor labelling portions of dives
    ## --------------------------------------------------------------------
    ## Arguments: x=class TDR object, diveID=numeric vector indexing each
    ## dive (non-dives should be 0)
    ## --------------------------------------------------------------------
    ## Author: Sebastian Luque
    ## --------------------------------------------------------------------
    if (!is(x, "TDR")) stop("x must be a TDR object")
    ok <- which(diveID > 0 & !is.na(getDepth(x))) # required diving indices
    ddepths <- getDepth(x)[ok]               # diving depths
    dids <- diveID[ok]                    # dive IDs
    ## We send a matrix of indices and non-NA depths
    td <- matrix(data=c(ok, ddepths), ncol=2) # times & depth dives only
    perdivetd <- by(td, dids, .cutDive)

    labdF <- do.call(rbind, perdivetd)
    ff <- factor(rep("X", length(diveID)), levels=c(unique(labdF[, 2]), "X"))
    ff[as.numeric(labdF[, 1])] <- labdF[, 2]
    ff
}

".diveIndices" <- function(diveID, diveNo)
{
    ## Value: A numeric vector with the indices of dives (and their
    ## beginning/end indices) in diveID
    ## --------------------------------------------------------------------
    ## Arguments: diveID=numeric vector numbering all dives and non-dives,
    ## diveNo=numeric vector of unique dive indices to extract fromdiveID.
    ## --------------------------------------------------------------------
    ## Author: Sebastian P. Luque
    ## --------------------------------------------------------------------
    ok <- which(diveID %in% diveNo)
    okl <- setdiff(ok - 1, ok)
    okr <- setdiff(ok + 1, ok)
    sort(c(okl, ok, okr))               # add the surface points
}
