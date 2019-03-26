#!/usr/bin/env Rscript

############################################################################
##
## File:      models.R
##
## Purpose:   Machine learning models for multiple methods and the ensemble
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Apr. 12, 2016
##
############################################################################

library("neuralnet"   ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("randomForest",warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("glm2"        ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("gam"         ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("rpart"       ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("e1071"       ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)

source("ml.R")

# defined constants
ta.ml.constants.algo.ens      <- "ens"
ta.ml.constants.algo.rf       <- "rf"
ta.ml.constants.algo.gam      <- "gam"
ta.ml.constants.algo.glm      <- "glm"
ta.ml.constants.algo.svm      <- "svm"
ta.ml.constants.algo.rp       <- "rp"

ta.ml.functions.ens.train     <- NA
ta.ml.functions.rf.train      <- ta.ml.functions.multi.train
ta.ml.functions.gam.train     <- ta.ml.functions.multi.train
ta.ml.functions.glm.train     <- ta.ml.functions.multi.train
ta.ml.functions.svm.train     <- ta.ml.functions.multi.train
ta.ml.functions.rp.train      <- ta.ml.functions.multi.train

ta.ml.functions.ens.predict   <- NA
ta.ml.functions.rf.predict    <- ta.ml.functions.multi.predict
ta.ml.functions.gam.predict   <- ta.ml.functions.multi.predict
ta.ml.functions.glm.predict   <- ta.ml.functions.multi.predict
ta.ml.functions.svm.predict   <- ta.ml.functions.multi.predict
ta.ml.functions.rp.predict    <- ta.ml.functions.multi.predict

ta.ml.constants.ml.algos      <- c(ta.ml.constants.algo.ens
                                  ,ta.ml.constants.algo.rf
                                  ,ta.ml.constants.algo.gam
                                  ,ta.ml.constants.algo.glm
                                  ,ta.ml.constants.algo.svm
                                  ,ta.ml.constants.algo.rp)

ta.ml.functions.training      <- c(NA
                                  ,randomForest
                                  ,gam
                                  ,glm
                                  ,svm
                                  ,rpart)

ta.ml.functions.trainers      <- c(ta.ml.functions.ens.train
                                  ,ta.ml.functions.rf.train
                                  ,ta.ml.functions.gam.train
                                  ,ta.ml.functions.glm.train
                                  ,ta.ml.functions.svm.train
                                  ,ta.ml.functions.rp.train)

ta.ml.functions.predictors    <- c(ta.ml.functions.ens.predict
                                  ,ta.ml.functions.rf.predict
                                  ,ta.ml.functions.gam.predict
                                  ,ta.ml.functions.glm.predict
                                  ,ta.ml.functions.svm.predict
                                  ,ta.ml.functions.rp.predict)

ta.ml.functions.vote.test <- function(vec=NULL,wgts=NULL) {
    ret  <- ta.db.constants.misc.zero
    if( !(is.null(vec) || is.null(wgts)) ) {
        ret1 <- ta.db.constants.misc.zero
        # add an extra 1 for the bias weight
        # or add 0 to drop the bias
        nvec <- c(vec,ta.db.constants.misc.one)
        lnv  <- length(nvec)
        lwt  <- length(wgts)
        if( lnv == lwt ) {
            mone <- -ta.db.constants.misc.one
            w0   <- which(nvec==ta.db.constants.misc.zero)
            if( length(w0) > 0 ) nvec[w0] <- mone
            val  <- nvec%*%wgts
            a1   <- abs(val-ta.db.constants.misc.zero)
            a2   <- abs(val-ta.db.constants.misc.one )
            if( a1 - a2 > 0.0 ) ret1 <- ta.db.constants.misc.one
        }
        ret2 <- ta.db.constants.misc.zero
        wcnt <- which(as.numeric(vec)==ta.db.constants.misc.one)
        if( length(wcnt) > 0 ) {
            val  <- length(wcnt)/length(vec)
            if( val > ta.ml.constants.defaults.floor )
                ret2 <- ta.db.constants.misc.one
        }
        if( ret1 == ret2 )
            ret  <- ret2
        else
            ret  <- vec[which(ta.ml.constants.ml.algos==ta.ml.constants.algo.rf)]
    }
    return(ret)
}

ta.ml.functions.vote <- function(mat=NULL) {
    wts  <- function(p=NULL,w=NULL) {
        ret  <- NULL
        if( !(is.null(p) || is.null(w)) ) {
            mone <- -ta.db.constants.misc.one
            prd  <- p
            for( i in 1:ncol(prd) ) {
                w0   <- which(prd[,i]==ta.db.constants.misc.zero)
                if( length(w0) > 0 ) prd[w0,i] <- rep(mone,length(w0))
            }
            y    <- rep(ta.db.constants.misc.one,length(p))
            x    <- prd
            df   <- data.frame(y=y,x=x)
            n    <- names(df)
            f    <- as.formula(paste("y~",paste(n[!n%in%"y"],collapse="+"),sep=""))
            fit  <- suppressWarnings(neuralnet(formula=f,data=df,hidden=ll,startweights=w,linear.output=FALSE))
            ret  <- as.matrix(cbind(fit$weights[[1]][[1]],fit$weights[[1]][[2]]))
        }
        return(ret)
    }
    ret  <- NULL
    if( !is.null(mat) ) {
        l    <- ta.ml.functions.ens.length(ta.ml.functions.predictors)
        ll   <- length(l)
        rwgts<- matrix(rnorm((ll+1)^2),nrow=(ll+1),ncol=(ll+1))
        wgts <- rwgts
        pret <- mat[,2:(2+ll-1)]
        pret <- matrix(as.numeric(pret),ncol=ll,nrow=length(pret)/ll)
        aret <- as.numeric(mat[,2+ll])
        if( !is.null(ta.ml.constants.fl) ) {
            fl   <- ta.utils.functions.file(paste(ta.ml.constants.boost
                                                 ,ta.ml.constants.misc.sp
                                                 ,sep="")
                                           ,ta.ml.constants.fl)
            pfl  <- fl$dr
            if( file.exists(pfl) && file.size(pfl) > 0 ) {
                wdat <- NULL
                tmp  <- try(read.csv(pfl,header=FALSE),silent=TRUE)
                if( !inherits(tmp,"try-error") ) wdat <- tmp
                else                             wgts <- rwgts
                nwts <- nrow(wdat)*ncol(wdat)
                wa   <- which(aret==ta.db.constants.misc.one)
                if( length(wa) > 0 ) wgts <- wts(matrix(pret[wa,],nrow=length(wa),ncol=ncol(pret))
                                                ,ifelse(nwts==length(wgts)
                                                       ,as.vector(wdat)
                                                       ,as.vector(wgts)))
                tf   <- file(pfl,"w")
                if( isOpen(tf) ) {
                    write.table(wgts,file=tf,quote=FALSE,col.names=FALSE,row.names=FALSE,qmethod="double",sep=",")
                    close(tf)
                }
            }
            else {
                wa   <- which(aret==ta.db.constants.misc.one)
                if( length(wa) > 0 ) wgts <- wts(matrix(pret[wa,],nrow=length(wa),ncol=ncol(pret)),as.vector(wgts))
                tf   <- file(pfl,"w")
                if( isOpen(tf) ) {
                    write.table(wgts,file=tf,quote=FALSE,col.names=FALSE,row.names=FALSE,qmethod="double",sep=",")
                    close(tf)
                }
            }
        }
        else {
            wa   <- which(aret==ta.db.constants.misc.one)
            if( length(wa) > 0 ) wgts <- wts(matrix(pret[wa,],nrow=length(wa),ncol=ncol(pret)),as.vector(wgts))
        }
        vote <- c()
        for( i in 1:nrow(mat) )
            vote <- c(vote,ta.ml.functions.vote.test(as.numeric(mat[i,2:(2+ll-1)]),wgts[,ncol(wgts)]))
        ret  <- matrix(c(mat[,1],vote,mat[,3])
                      ,nrow=nrow(mat)
                      ,ncol=length(ta.ml.constants.columns.all))
        colnames(ret) <- ta.ml.constants.columns.all
    }
    return(ret)
}

ta.ml.functions.ens.length <- function(funcs=NULL) {
    ret  <- NULL
    if( !is.null(funcs) ) {
        wens <- which(ta.ml.constants.ml.algos==ta.ml.constants.algo.ens)
        if( length(wens) > 0 ) ret  <- (1:length(funcs))[-wens]
        else                   ret  <-  2:length(funcs)
    }
    return(ret)
}

ta.ml.functions.ens.train <- function(dat=NULL,typ=NULL) {
    l    <- ta.ml.functions.ens.length(ta.ml.functions.trainers)
    ret  <- NULL
    if( ta.config.constants.dc > 1 )
        ret  <- foreach( i = l, .combine=ta.ml.functions.rbind, .inorder=TRUE ) %dopar%
            ta.ml.functions.trainers[i][[1]](dat,ta.ml.constants.ml.algos[i],ta.ml.functions.training[i][[1]])
    else {
        for( i in l ) {
            trn  <- ta.ml.functions.trainers[i][[1]](dat,ta.ml.constants.ml.algos[i],ta.ml.functions.training[i][[1]])
            ret  <- rbind(ret,trn)
        }
    }
    return(ret)
}

ta.ml.functions.ens.predict <- function(dat=NULL,typ=NULL) {
    epredict <- function(ret=NULL,ind=0) {
        pred <- NULL
        if( ind > 0 ) {
            wact <- which(colnames(dat)==ta.db.constants.actuals)
            dact <- dat[,wact]
            uact <- unique(dact)
            if( length(uact) > 1 ) {
                pred <- ta.ml.functions.predictors[ind][[1]](dat,ta.ml.constants.ml.algos[i],ta.ml.functions.training[i][[1]])
                if( !is.null(pred) ) {
                    pred           <- matrix(pred,nrow=nrow(pred),ncol=ncol(pred))
                    if( ncol(pred) == length(ta.ml.constants.columns.all) )
                        colnames(pred) <- ta.ml.constants.columns.all
                    else
                                 pred  <- NULL
                }
            }
            else {
                wusr <- which(colnames(dat)==ta.ml.constants.misc.user)
                act  <- as.character(dat[,wact])
                pred <- cbind(rep(dat[,wusr],length(act))
                             ,rep(act       ,length(act))
                             ,rep(act       ,length(act)))
                colnames(pred) <- ta.ml.constants.columns.all
            }
        }
        return(pred)
    }
    l    <- ta.ml.functions.ens.length(ta.ml.functions.predictors)
    ret  <- NULL
    cnt  <- 0
    for( i in l ) {
        pred <- epredict(ret,i)
        if( is.null(ret) ) {
            ret  <- pred
            cnt  <- cnt + 1
        }
        else {
            if( !is.null(pred) && nrow(pred) == nrow(ret) ) {
                ret  <- ta.ml.functions.cbind(ret,pred)
                cnt  <- cnt + 1
            }
        }
    }
    if( !is.null(ret) ) ret  <- ta.ml.functions.vote(ret)
    return(ret)
}

ta.ml.functions.trainers[1][[1]]   <- ta.ml.functions.ens.train
ta.ml.functions.predictors[1][[1]] <- ta.ml.functions.ens.predict

# default to the ensemble as the trainer and predictor
walgo<- which(ta.ml.constants.ml.algos==ta.ml.constants.algo.ens)
if( !is.null(ta.ml.constants.ml.algo) ) walgo<- which(ta.ml.constants.ml.algos==ta.ml.constants.ml.algo)
ta.ml.functions.train     <- ta.ml.functions.trainers[walgo][[1]]
ta.ml.functions.predict   <- ta.ml.functions.predictors[walgo][[1]]
ta.ml.functions.trainer   <- ta.ml.functions.training[walgo][[1]]
