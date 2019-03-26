#!/usr/bin/env Rscript

############################################################################
##
## File:      ta.R
##
## Purpose:   Main service for the predictor.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Mar. 23, 2019
##
############################################################################

rm(list=ls())

# source required files with db.R being
# sourced in ml.R and config.R being
source("models.R")

# defined constants
ta.constants.ta.name    <- ta.config.constants.name
ta.constants.ta.type    <- "type"
ta.constants.ta.file    <- ta.ml.constants.file
ta.constants.ta.pfile   <- paste("p",ta.ml.constants.file,sep="")
ta.constants.ta.sqlfile <- "sqlfile"
ta.constants.ta.tsample <- "tsample"
ta.constants.ta.psample <- "psample"

ta.constants.ta.fl      <- ta.config.functions.search(ta.config.variables
                                                     ,ta.constants.ta.name
                                                     ,ta.constants.ta.file   )
ta.constants.ta.pfl     <- ta.config.functions.search(ta.config.variables
                                                     ,ta.constants.ta.name
                                                     ,ta.constants.ta.pfile  )
ta.constants.ta.tsamp   <- ta.config.functions.search(ta.config.variables
                                                     ,ta.ml.constants.ml.name
                                                     ,ta.constants.ta.tsample)
ta.constants.ta.psamp   <- ta.config.functions.search(ta.config.variables
                                                     ,ta.ml.constants.ml.name
                                                     ,ta.constants.ta.psample)

ta <- function(trn=FALSE) {
    fl   <- ifelse(trn,ta.constants.ta.fl,ta.constants.ta.pfl)
    if( is.null(fl) ) return(NULL)
    rdat <- ta.ml.functions.conv.file(fl)
    ret  <- c()
    if( !(is.null(rdat) || nrow(rdat) == 0 || ncol(rdat) == 0) ) {
        if( is.list(rdat) ) rdat <- as.matrix(rdat,ncol=ncol(rdat))
        if( !(is.null(ta.constants.ta.tsamp) || is.null(ta.constants.ta.psamp)) ) {
            samp <- ifelse(trn,ta.constants.ta.tsamp,ta.constants.ta.psamp)
            rdat <- rdat[sample(1:nrow(rdat),ceiling(as.numeric(samp)*nrow(rdat))),]
        }
        # training of the models
        if( trn ) {
            ret  <- ta.ml.functions.train(rdat,ta.ml.constants.ml.algo)
            if( !is.null(ret) ) {
                if( !is.null(ta.ml.constants.fl) ) {
                    write(t(colnames(ret))
                         ,ta.ml.constants.fl
                         ,ncolumns=ncol(ret)
                         ,sep=","
                         ,append=FALSE)
                    write(t(ret)
                         ,ta.ml.constants.fl
                         ,ncolumns=ncol(ret)
                         ,sep=","
                         ,append=TRUE)
                }
            }
        }
        # prediction of the data
        else {
            sqlf <- ta.config.functions.search(ta.config.variables
                                              ,ta.constants.ta.name
                                              ,ta.constants.ta.sqlfile)
            if( !is.null(sqlf) )
                ret  <- ta.db.functions.data.sqlfile(sqlf)
            else {
                if( !is.null(ta.ml.constants.fl) ) {
                    fl   <- ta.utils.functions.file(paste(ta.db.constants.predictions
                                                         ,ta.ml.constants.misc.sp
                                                         ,sep="")
                                                   ,ta.ml.constants.fl)
                    pfl  <- paste(fl$dr,".csv",sep="")
                    write(t(ta.ml.constants.columns.preds)
                         ,pfl
                         ,ncolumns=length(ta.ml.constants.columns.preds)
                         ,sep=","
                         ,append=FALSE)
                }
                fl   <- ta.utils.functions.file(paste(ta.ml.constants.boost
                                                     ,ta.ml.constants.misc.sp
                                                     ,sep="")
                                               ,ta.ml.constants.fl)
                pfl  <- fl$dr
                if( file.exists(pfl) && file.size(pfl) > 0 ) unlink(pfl)
                sz   <- ta.ml.constants.defaults.block
                blks <- nrow(rdat)/sz
                if( blks - floor(blks) > 0.0 ) blks <- ceiling(blks)
                if( ta.config.constants.dc > 1 )
                    ret  <- foreach( b = 1:blks, .combine=rbind, .inorder=TRUE ) %dopar% {
                        bg   <- ((b-1)*sz) + 1
                        nd   <- min(b*sz,nrow(rdat))
                        datr <- as.matrix(rdat[bg:nd,])
                        ta.ml.functions.predict(datr,ta.ml.constants.ml.algo)
                    }
                else {
                    ret  <- c()
                    for( b in 1:blks ) {
                        bg   <- ((b-1)*sz) + 1
                        nd   <- min(b*sz,nrow(rdat))
                        datr <- as.matrix(rdat[bg:nd,])
                        ret  <- rbind(ret,ta.ml.functions.predict(datr,ta.ml.constants.ml.algo))
                    }
                }
                if( !is.null(ret) ) {
                    if( !is.null(ta.ml.constants.fl) ) {
                        fl   <- ta.utils.functions.file(paste(ta.db.constants.predictions
                                                             ,ta.ml.constants.misc.sp
                                                             ,sep="")
                                                       ,ta.ml.constants.fl)
                        pfl  <- paste(fl$dr,".csv",sep="")
                        tret <- ta.ml.functions.validate(ret)
                        vret <- tret$ret
                        write(t(vret),pfl,ncolumns=ncol(vret),sep=",",append=TRUE)
                        ret  <- ta.ml.functions.data.validate(tret$ret)
                    }
                }
            }
            if( !is.null(ret) ) {
                r    <- ta.ml.functions.score.f1(ret)
                if( !is.null(ta.ml.constants.fl) ) {
                    fl   <- ta.utils.functions.file(paste(ta.ml.constants.misc.scores
                                                            ,ta.ml.constants.misc.sp
                                                            ,sep="")
                                                      ,ta.ml.constants.fl)
                    pfl  <- paste(fl$dr,".csv",sep="")
                    write(t(c("acc","rec","prc","fnr")),pfl,ncolumns=length(r$ascrs),sep=","            )
                    write(t(r$ascrs[2:5])              ,pfl,ncolumns=length(r$ascrs),sep=",",append=TRUE)
                }
            }
        }
    }
    return(ret)
}
