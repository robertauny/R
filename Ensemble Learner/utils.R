#!/usr/bin/env Rscript

############################################################################
##
## File:      utils.R
##
## Purpose:   General utilities
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Apr. 12, 2016
##
############################################################################

# defined constants
ta.utils.constants.name  <- "name"
ta.utils.constants.file  <- "file"

ta.utils.constants.sp.eq <- "="
ta.utils.constants.sp.ul <- "_"

ta.utils.constants.fl    <- "inst.txt"

# file name utility
ta.utils.functions.file <- function(x=NULL,f=NULL) {
    ret  <- NULL
    if( !is.null(f) ) {
        sp   <- "/"
        fl   <- strsplit(f,sp,fixed=TRUE)
        if( !is.null(fl) ) {
            f1   <- unlist(fl)
            f2   <- f1[length(f1)]
            p1   <- paste(f1[1:(length(f1)-1)],collapse=sp)
            p2   <- paste(x,f2,sep="")
            p3   <- paste(p1,p2,sep="/")
            ret  <- list(nm=f2,di=p1,dr=p3)
        }
        else
            ret  <- list(nm=f,di=NULL,dr=paste(x,f,sep=""))
    }
    return(ret)
}

# unique rows
ta.utils.functions.unique <- function(dat=NULL) {
    ret  <- c()
    if( !is.null(dat) ) {
        if( is.matrix(dat) ) for( i in 1:nrow(dat) ) ret  <- c(ret,paste(dat[i,],collapse=""))
        else                                         ret  <-       paste(dat    ,collapse="")
    }
    return(ret)
}

# just return what's passed (for concurrency)
ta.utils.functions.return <- function(dat=NULL) {
    return(dat)
}

ta.utils.functions.inst <- function(typ=NULL) {
    ret  <- NULL
    if( file.exists(ta.utils.constants.fl) && file.size(ta.utils.constants.fl) > 0 ) {
        if( !is.null(typ) ) {
            f    <- file(ta.utils.constants.fl)
            dat  <- readLines(f)
            close(f)
            if( !is.null(dat) ) {
                gnm  <- grep(paste(typ,ta.utils.constants.sp.eq,sep=""),dat)
                if( length(gnm) > 0 ) {
                    strc <- as.vector(unlist(strsplit(dat[gnm],ta.utils.constants.sp.eq,fixed=TRUE)))
                    ret  <- strc[2]
                }
            }
        }
    }
    return(ret)
}

ta.utils.functions.counts <- function(val=NULL,pct=NULL,iter=NULL) {
    if( !(is.null(val) || is.null(pct) || is.null(iter)) ) {
        cnt  <- (1.0-pct)*val
        for( i in 1:iter ) {
            cnt  <- (1.0+0.02) * (1.0-0.02) * cnt
            print(paste("CNT:",cnt))
        }
    }
}
