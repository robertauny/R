#!/usr/bin/env Rscript

############################################################################
##
## File:      run.R
##
## Purpose:   General run utility
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Nov. 9, 2016
##
############################################################################

opt  <- commandArgs(trailingOnly=TRUE)
if( !(is.null(opt) || length(opt) == 0) ) {
    setwd(opt[1])
    source('ta.R')
    write(format(Sys.time(),"%Y-%m-%d %H:%M:%s"),"/tmp/time.out")
    atlas(FALSE)
    write(format(Sys.time(),"%Y-%m-%d %H:%M:%s"),"/tmp/time.out",append=TRUE)
}
