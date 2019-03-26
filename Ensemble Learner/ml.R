#!/usr/bin/env Rscript

############################################################################
##
## File:      ml.R
##
## Purpose:   Machine learning utilities
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Apr. 12, 2016
##
############################################################################

library("matlib"      ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("rlang"       ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("ModelMetrics",warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("pROC"        ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("png"         ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("stringr"     ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)

source("db.R")

# defined constants
ta.ml.constants.label            <- "label"
ta.ml.constants.file             <- "file"
ta.ml.constants.algorithm        <- "algorithm"
ta.ml.constants.boost            <- "boost"
ta.ml.constants.num              <- "num"
ta.ml.constants.rmv              <- "rmv"
ta.ml.constants.block            <- "block"
ta.ml.constants.floor            <- "floor"
ta.ml.constants.conv             <- "convolution"

ta.ml.constants.misc.true        <- "TRUE"
ta.ml.constants.misc.false       <- "FALSE"
ta.ml.constants.misc.yes         <- "YES"
ta.ml.constants.misc.no          <- "NO"
ta.ml.constants.misc.models      <- "models"
ta.ml.constants.misc.scores      <- "scores"
ta.ml.constants.misc.confusion   <- "confusion"
ta.ml.constants.misc.sp          <- ta.utils.constants.sp.ul
ta.ml.constants.misc.num         <- ta.db.constants.misc.num
ta.ml.constants.misc.user        <- ta.config.functions.search(ta.config.variables
                                                              ,"ta"
                                                              ,"pcol")

ta.ml.constants.ml.name          <- "ml"
ta.ml.constants.ml.algo          <- ta.config.functions.search(ta.config.variables
                                                              ,ta.ml.constants.ml.name
                                                              ,ta.ml.constants.algorithm)
ta.ml.constants.ml.rmv           <- ta.config.functions.search(ta.config.variables
                                                              ,ta.ml.constants.ml.name
                                                              ,ta.ml.constants.rmv)

ta.ml.constants.fl               <- ta.config.functions.search(ta.config.variables
                                                              ,ta.ml.constants.ml.name
                                                              ,ta.ml.constants.file)
ta.ml.constants.columns.all      <- c(ta.ml.constants.misc.user
                                     ,ta.db.constants.predictions
                                     ,ta.db.constants.actuals)
ta.ml.constants.columns.val      <- "VALIDATES?"
ta.ml.constants.columns.preds    <- c(ta.ml.constants.columns.all,ta.ml.constants.columns.val)
ta.ml.constants.columns.scores   <- c(ta.ml.constants.misc.user,"ACCURACY")

ta.ml.constants.defaults.block   <- ta.config.functions.search(ta.config.variables
                                                              ,ta.ml.constants.ml.name
                                                              ,ta.ml.constants.block)
ta.ml.constants.defaults.block   <- ifelse(is.null(ta.ml.constants.defaults.block)
                                          ,1000
                                          ,as.numeric(ta.ml.constants.defaults.block))
ta.ml.constants.defaults.floor   <- ta.config.functions.search(ta.config.variables
                                                              ,ta.ml.constants.ml.name
                                                              ,ta.ml.constants.floor)
ta.ml.constants.defaults.floor   <- ifelse(is.null(ta.ml.constants.defaults.floor)
                                          ,0.5
                                          ,as.numeric(ta.ml.constants.defaults.floor))
ta.ml.constants.defaults.conv    <- ta.config.functions.search(ta.config.variables
                                                              ,ta.ml.constants.ml.name
                                                              ,ta.ml.constants.conv )
ta.ml.constants.defaults.conv    <- ifelse(is.null(ta.ml.constants.defaults.conv )
                                          ,20
                                          ,as.numeric(ta.ml.constants.defaults.conv ))

# convolutions of the vec
ta.ml.functions.conv.num <- function(vec=NULL) {
    nms  <- NULL
    if( !is.null(vec) ) {
        conv <- ta.ml.constants.defaults.conv
        name <- str_pad(vec,max(max(nchar(vec)),conv),"right")
        if( ta.config.constants.dc > 1 )
            nms  <- foreach( i = 1:length(name), .combine=rbind, .inorder=TRUE ) %dopar% {
                nm   <- as.numeric(charToRaw(name[i]))
                nmr  <- matrix(nm,nrow=1,ncol=length(nm))
                ta.utils.functions.return(nmr)
            }
        else
            for( i in 1:length(name) ) {
                nm   <- as.numeric(charToRaw(name[i]))
                if( i == 1 ) nms  <- matrix(nm,nrow=1,ncol=length(nm))
                else         nms  <- rbind(nms,nm)
            }
        rownames(nms) <- NULL
        if( ncol(nms) > conv ) nms  <- matrix(nms[,1:conv],nrow=nrow(nms),ncol=conv)
    }
    return(nms)
}

# convolutions apply
ta.ml.functions.conv.apply <- function(vec=NULL) {
    ret  <- c()
    if( !is.null(vec) )
        for( i in 1:length(vec) )
            ret[i] <- sum(convolve(vec[i],vec[-i],type="open"))
    return(ret)
}

# convolutions actual
ta.ml.functions.conv.trans <- function(mat=NULL) {
    ret  <- NULL
    if( !(is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) ) {
        nret <- c()
        for( i in 1:nrow(mat) )
            nret <- c(nret,ta.ml.functions.conv.apply(mat[i,]))
        ret  <- t(matrix(nret,nrow=ncol(mat),ncol=(length(nret)/ncol(mat))))
    }
    return(ret)
}

# convolutions of the file
ta.ml.functions.conv.file <- function(fl=NULL) {
    ret  <- NULL
    if( !is.null(fl) ) {
        conv <- ta.ml.constants.defaults.conv
        dat  <- read.csv(fl)
        if( !(is.null(dat) || nrow(dat) == 0 || ncol(dat) == 0) ) {
            dat  <- as.matrix(dat)
            dat  <- dat[order(dat[,1]),]
            name <- enc2utf8(as(dat[,1],"character"))
            name <- ta.ml.functions.conv.trans(ta.ml.functions.conv.num(name))
            sex  <- ifelse(as.character(dat[,2])=="M",1,0)
            ret  <- cbind(as.character(dat[,1]),name,sex)
            colnames(ret) <- c(ta.ml.constants.misc.user
                              ,paste("conv",1:ncol(name),sep="")
                              ,ta.db.constants.actuals)
        }
    }
    return(ret)
}

# accuracy scores
ta.ml.functions.score.arp <- function(dat=NULL) {
    tp   <- 0
    tn   <- 0
    fp   <- 0
    fn   <- 0
    acc  <- 0.0
    rec  <- 0.0
    prc  <- 0.0
    val  <- NULL
    if( !is.null(dat) ) {
        val  <- rep(ta.ml.constants.misc.false,nrow(dat))
        wprd <- which(colnames(dat)==ta.db.constants.predictions)
        wact <- which(colnames(dat)==ta.db.constants.actuals)
        for( i in 1:nrow(dat) ) {
            prd  <- dat[i,wprd]
            act  <- dat[i,wact]
            if( prd == ta.db.constants.misc.one ) {
                if( act == ta.db.constants.misc.one ) {
                    val[i] <- ta.ml.constants.misc.true
                    tp     <- tp + 1
                }
                else
                    fp     <- fp + 1
            }
            else {
                if( act == ta.db.constants.misc.zero ) {
                    val[i] <- ta.ml.constants.misc.true
                    tn     <- tn + 1
                }
                else
                    fn     <- fn + 1
            }
        }
        dn1 <- tp + fp + tn + fn
        if( !(dn1 == 0.0) ) acc <- (tn+tp)/dn1
        dn2 <- tp           + fn
        if( !(dn2 == 0.0) ) rec <-     tp /dn2
        dn3 <- tp + fp
        if( !(dn3 == 0.0) ) prc <-     tp /dn3
    }
    return(list(tp=tp,tn=tn,fp=fp,fn=fn,acc=acc,rec=rec,prc=prc,fnr=fn/(fn+tp),val=val))
}

# f1 scores
ta.ml.functions.score.f1 <- function(dat=NULL) {
    # note that the scoring system doesn't
    # penalize for a false negative, but it
    # does penalize for a false positive
    scrs <- ta.ml.functions.score.arp(dat)
    f1   <- 0.0
    if( !is.null(dat) ) {
        tp   <- scrs$tp
        tn   <- scrs$tn
        fp   <- scrs$fp
        fn   <- scrs$fn
        # F1 score
        if( tp == 0 && fp == 0 && fn == 0 )
            f1   <- 1.0
        else {
            dn2  <- (2*tp) + fp + fn
            if( !(dn2 == 0) ) f1  <- (2*tp)/dn2
        }
    }
    return(list(ascrs=c(f1=f1,acc=scrs$acc,rec=scrs$rec,prc=scrs$prc,fnr=scrs$fnr),val=scrs$val))
}

# validate the predictions
ta.ml.functions.validate <- function(dat=NULL) {
    ret  <- dat
    scrs <- list(ascrs=c(f1=0.0,acc=0.0,rec=0.0,prc=0.0,fnr=0.0),val=NULL)
    if( !is.null(ret) ) {
        scrs                     <- ta.ml.functions.score.f1(ret)
        scr                      <- rep(ta.ml.constants.misc.no,length(scrs$val))
        scrst                    <- which(scrs$val==TRUE)
        if( length(scrst) > 0 )
            scr[scrst]           <- rep(ta.ml.constants.misc.yes,length(scrst))
        ret                      <- cbind(ret,scr)
        colnames(ret)[ncol(ret)] <- ta.ml.constants.columns.val
    }
    return(list(ret=ret,scrs=scrs))
}

# histogram function
ta.ml.functions.hist <- function(dat=NULL,trn=NULL,m=10) {
    if( !(is.null(dat) || is.null(trn)) ) {
        if( !is.null(ta.ml.constants.fl) ) {
            fl   <- ta.utils.functions.file(paste("hist"
                                                 ,ta.ml.constants.misc.sp
                                                 ,ifelse(trn,"train","predict")
                                                 ,ta.ml.constants.misc.sp
                                                 ,sep="")
                                           ,ta.ml.constants.fl)
            pfl  <- paste(fl$dr,".png",sep="")
            png(pfl,width=1000,height=600)
            sdat <- sort(dat)
            us   <- unique(sdat)
            cnts <- c()
            for( i in us ) {
                ws   <- which(sdat==i)
                lws  <- length(ws)
                if( is.null(cnts) ) cnts <- lws
                else                cnts <- rbind(cnts,lws)
            }
            h    <- hist(sdat
                        ,main=paste("Distribution of Positive Predictions:",pn)
                        ,ylim=c(0,m*max(cnts))
                        ,ylab="Count"
                        ,xlab="Frequency")
            x    <- seq(0,100,10)
            y    <- dnorm(x,mean=mean(sdat),sd=sd(sdat))
            y    <- y * diff(h$mids[1:2]) * length(sdat)
            lines(x,y,col="red",lwd=2)
            dev.off()
            img  <- readPNG(pfl)
            grid::grid.raster(img)
        }
    }
}

# generate images from the validation data
ta.ml.functions.data.validate <- function(d=NULL) {
    dat  <- d
    if( !is.null(d) ) {
        dat  <- as.matrix(dat)
        dnm  <- c(ta.db.constants.predictions,ta.db.constants.actuals)
        wp   <- which(colnames(dat)==dnm[1])
        wa   <- which(colnames(dat)==dnm[2])
        prd  <- as.numeric(dat[,wp])
        act  <- as.numeric(dat[,wa])
        cm   <- confusionMatrix(actual=act,predicted=prd)
        if( !is.null(ta.ml.constants.fl) ) {
            fl   <- ta.utils.functions.file(paste(ta.ml.constants.misc.confusion
                                                 ,ta.ml.constants.misc.sp
                                                 ,sep="")
                                           ,ta.ml.constants.fl)
            pfl  <- paste(fl$dr,".csv",sep="")
            write(c("label",ta.db.constants.misc.zero,ta.db.constants.misc.one)
                 ,pfl
                 ,ncolumns=3
                 ,sep=",")
            write(t(cbind(matrix(c(ta.db.constants.misc.zero,ta.db.constants.misc.one),nrow=2)
                         ,matrix(cm,nrow=2,ncol=2)))
                 ,pfl
                 ,ncolumns=3
                 ,sep=","
                 ,append=TRUE)
            fl   <- ta.utils.functions.file(paste("roc"
                                                 ,ta.ml.constants.misc.sp
                                                 ,sep="")
                                           ,ta.ml.constants.fl)
            pfl  <- paste(fl$dr,".png",sep="")
            if( length(unique(prd)) > 1 || length(unique(act)) > 1 ) {
                m    <- paste("ROC Curve:",ta.ml.constants.misc.user)
                prp  <- roc(act,prd,print.auc=TRUE)
                png(pfl,width=1000,height=600)
                plot(prp,type='l',col='red',xlab="Specificity",main=m)
                dev.off()
                img  <- readPNG(pfl)
                grid::grid.raster(img)
            }
            else
                write("NO ROC: Single Level",pfl)
        }
        cma  <- cm[4]
        cmb  <- cm[2]
        cmc  <- cm[3]
        cmd  <- cm[1]
        sens <- cma/(cma+cmc)
        spfc <- cmd/(cmb+cmd)
        ba   <- (sens+spfc)/2.0
        prc  <- cma/(cma+cmb)
        rec  <- cma/(cma+cmc)
        if( !is.null(ta.ml.constants.fl) ) {
            fl   <- ta.utils.functions.file(paste("stats"
                                                 ,ta.ml.constants.misc.sp
                                                 ,sep="")
                                           ,ta.ml.constants.fl)
            pfl  <- paste(fl$dr,".csv",sep="")
            write(c("sens","spfc","ba","prc","rec"),pfl,ncolumns=5,sep=","            )
            write(c( sens , spfc , ba , prc , rec ),pfl,ncolumns=5,sep=",",append=TRUE)
        }
    }
    return(dat)
}

ta.ml.functions.multi.train <- function(dat=NULL,typ=NULL,trainer=NULL) {
    if( !(is.null(dat) || is.null(typ)) ) {
        if( !is.null(trainer) ) ta.ml.functions.trainer <- trainer
        # perform the training
        rdat <- dat
        if( !is.null(ta.ml.constants.ml.rmv) ) {
            nm   <- as.vector(unlist(strsplit(ta.ml.constants.ml.rmv,",",fixed=TRUE)))
            num  <- which(colnames(rdat)%in%nm)
            if( !is.null(num) ) rdat <- rdat[,-num]
        }
        wact <- which(colnames(rdat)==ta.db.constants.actuals  )
        wusr <- which(colnames(rdat)==ta.ml.constants.misc.user)
        dz   <- as.numeric(rdat[,wact])
        nm   <- colnames(rdat)[(1:ncol(rdat))[-c(wusr,wact)]]
        num  <- which(colnames(rdat)%in%nm)
        if( length(num) > 0 ) {
             zn  <- matrix(as.character(rdat[,-c(wact,num)]),nrow=nrow(rdat),ncol=(ncol(rdat)-(length(num)+1)))
            nzn  <- matrix(as.numeric(  rdat[,        num ]),nrow=nrow(rdat),ncol=             length(num)    )
        }
        else {
             zn  <- matrix(as.character(rdat[,-wact]),nrow=nrow(rdat),ncol=(ncol(rdat)-1))
            nzn  <- NULL
        }
        if( is.null(nzn) ) df   <- data.frame(dz,zn)
        else               df   <- data.frame(dz,nzn)
        cfit <- suppressWarnings(ta.ml.functions.trainer(dz~.,data=df))
        # save the model for later usage
        if( !is.null(ta.ml.constants.fl) ) {
            fl   <- ta.utils.functions.file(paste(typ
                                                 ,ta.ml.constants.misc.sp
                                                 ,sep="")
                                           ,ta.ml.constants.fl)
            if( !(is.null(cfit) || is.null(fl)) ) {
                if( !(is.null(fl$di) || dir.exists(fl$di)) ) dir.create(fl$di,recursive=TRUE)
                save(cfit,file=fl$dr)
            }
        }
    }
}

ta.ml.functions.multi.predict <- function(dat=NULL,typ=NULL,trainer=NULL) {
    ret  <- c()
    if( !(is.null(dat) || is.null(typ)) ) {
        if( !is.null(trainer) ) ta.ml.functions.trainer <- trainer
        rdat <- dat
        wusr <- which(colnames(rdat)==ta.ml.constants.misc.user)
        if( length(wusr) > 0 ) usrs <- as.character(rdat[,wusr])
        if( !is.null(ta.ml.constants.ml.rmv) ) {
            nm   <- as.vector(unlist(strsplit(ta.ml.constants.ml.rmv,",",fixed=TRUE)))
            num  <- which(colnames(rdat)%in%nm)
            if( !is.null(num) ) rdat <- rdat[,-num]
        }
        wact <- which(colnames(rdat)==ta.db.constants.actuals  )
        wusr <- which(colnames(rdat)==ta.ml.constants.misc.user)
        if( length(wact) > 0 ) {
            act  <- as.character(rdat[,wact])
            # read the correct model file
            if( !is.null(ta.ml.constants.fl) ) {
                fl   <- ta.utils.functions.file(paste(typ
                                                     ,ta.ml.constants.misc.sp
                                                     ,sep="")
                                               ,ta.ml.constants.fl)
                if( file.exists(fl$dr) && file.size(fl$dr) > 0 ) {
                    # load the appropriate model and predict
                    dz   <- as.numeric(rdat[,wact])
                    nm   <- colnames(rdat)[(1:ncol(rdat))[-c(wusr,wact)]]
                    num  <- which(colnames(rdat)%in%nm)
                    if( length(num) > 0 ) {
                         zn  <- matrix(as.character(rdat[,-c(wact,num)]),nrow=nrow(rdat),ncol=(ncol(rdat)-(length(num)+1)))
                        nzn  <- matrix(as.numeric(  rdat[,        num ]),nrow=nrow(rdat),ncol=             length(num)    )
                    }
                    else {
                         zn  <- matrix(as.character(rdat[,-wact]),nrow=nrow(rdat),ncol=(ncol(rdat)-1))
                        nzn  <- NULL
                    }
                    if( is.null(nzn) ) df   <- data.frame(dz=0,zn)
                    else               df   <- data.frame(dz=0,nzn)
                    l    <- load(fl$dr)
                    fit  <- get("cfit")
                    pd   <- suppressWarnings(predict(fit,df))
                    pdl  <- which(pd<ta.ml.constants.defaults.floor)
                    if( length(pdl) > 0 ) {
                        pd[pdl] <- rep(ta.db.constants.misc.zero,length(pdl))
                        if( length(pdl) < length(pd) )
                            pd[-pdl] <- rep(ta.db.constants.misc.one,length(pd)-length(pdl))
                    }
                    else {
                        pdl  <- which(pd>=ta.ml.constants.defaults.floor)
                        if( length(pdl) > 0 ) pd[pdl] <- rep(ta.db.constants.misc.one ,length(pdl ))
                        else                  pd      <- rep(ta.db.constants.misc.zero,length(pd))
                    }
                    ret  <- cbind(usrs,pd,act)
                }
            }
        }
    }
    return(ret)
}

ta.ml.functions.rbind <- function(a,b) {
    ret  <- a
    if( !is.null(b) ) {
        ncb  <- ncol(b)
        if( !is.null(a) ) {
            nca  <- ncol(a)
            if( nca == ncb ) ret  <- rbind(a,b)
        }
        else
            ret  <- b
    }
    return(ret)
}

ta.ml.functions.cbind <- function(a,b) {
    if( !(is.null(a) || is.null(b)) ) {
        ret           <- cbind(a[,1:(ncol(a)-1)],b[,(ncol(b)-1):ncol(b)])
        colnames(ret) <- c(colnames(a)[1:(ncol(a)-1)],colnames(b)[(ncol(b)-1):ncol(b)])
    }
    else {
        if( is.null(b) ) ret  <- a
        else             ret  <- b
    }
    return(ret)
}
