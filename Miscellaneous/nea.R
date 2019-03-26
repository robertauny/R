#!/usr/bin/env Rscript

############################################################################
##
## File:      nea.R
##
## Purpose:   Use time series methods for doing classification of
##            sensor data.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      June 18, 2016
##
############################################################################

# remove all created objects
rm(list=ls())

ranges <- function(ind,pre,fdat) {
    p <- pre
    b <- min(ind)
    e <- max(ind)
    u <- unlist(p)
    if( is.null(u) ) {
        if( fdat[e+1,2] > fdat[e,2] ) {
            r <- cbind(b,e)
            p <- rbind(p,r)
        }
    }
    else {
        q <- matrix(u,ncol=2)
        z <- TRUE
        for( j in nrow(q) ) {
            m1 <- b:e
            m2 <- q[j,1]:q[j,2]
            z <- all(is.na(match(m1,m2)))
            if( !z ) break
        }
        if( z                       &&
            fdat[b-1,2] > fdat[b,2] &&
            fdat[e+1,2] > fdat[e,2] ) {
            r <- cbind(b,e)
            p <- rbind(p,r)
        }
    }
    return(p)
}

# function to find valleys in
# the sensor data set and normalize
valleys <- function(fdat) {
    if( nrow(fdat) > 2 ) {
        vals <- unique(fdat[,2])
        v    <- list()
        for( i in vals ) {
            rows <- which(fdat[,2]==i)
            if( length(rows) > 0 ) {
                srow <- c()
                for( j in rows ) {
                    srow <- c(srow,j)
                    if( (j+1) %in% rows ) next
                    if( length(srow) > lb ) v <- ranges(srow,v,fdat)
                    srow <- c()
                }
            }
        }
    }
    return(v)
}

# function to find peaks in
# the sensor data set and normalize
peaks <- function(fdat) { return(valleys(-fdat)) }

# prediction period in years
f <- NULL
while( is.null(f) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n FILE NAME #                                                                                  ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = sensor_data.csv ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        f <- paste(Sys.getenv("HOME"),"/data/sensor_data.csv",sep="")
    }
    else {
        f <- opt
    }
    # get the data
    dat1 <- read.csv(f,header=TRUE)
    # check the default location in case
    # file name is not fully qualified
    if( is.null(dat1) ) dat1 <- read.csv(paste(Sys.getenv("HOME")
                                              ,"/data/"
                                              ,f
                                              ,sep="")
                                        ,header=TRUE)
    # otherwise, ask for another file name
    if( is.null(dat1) ) {
        f <- NULL
    }
}

# prediction period in years
lb <- NULL
while( is.null(lb) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n LOWER BOUND ON ANOMALIES (INT) #                                                             ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 7 , loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        lb <- 7
    }
    else {
        if( opt < 1 ) f <- NULL
    }
}

print("OPTIONS")

# just the time and the resistance
dat  <- matrix(unlist(dat1[which(dat1[,2]=="1"),c(1,ncol(dat1))]),ncol=2)

# smoothing all of those minor fluctuations
# in the data set
dat[,1] <- seq(1,nrow(dat))
dat[,2] <- round(dat[,2])

# normalize the data for training
v <- valleys(dat)
if( !is.null(v) ) {
    vm   <- matrix(unlist(v),ncol=2)
    ndat <- dat[vm[,1],2]
    print("DATA")
    # perform kmeans on the data to
    # determine the class centers
    kdat <- stats::kmeans(ndat,3)
    # perform kmeans on the test data
    tdat <- stats::kmeans(ndat,kdat$centers)
    # compute the accuracy of classifying the same data
    # can only compute accuracy when classifying against
    # labeled data ... otherwise doesn't make sense
    accu <- length(which(tdat$cluster!=kdat$cluster))/length(tdat$cluster)
    # write the data to a file
    odat <- matrix(c(Exposure=seq(1,length(tdat$cluster))
                    ,Class=tdat$cluster
                    ,Accuracy=ifelse(tdat$cluster==kdat$cluster,1,0))
                  ,ncol=3)
    f    <- "/tmp/output/results.csv"
    write(c("Exposure","Class","Accuracy(Boolean)")
         ,file=f
         ,sep=","
         ,ncolumns=3)
    write(t(odat)
         ,file=f
         ,ncolumns=3
         ,sep=","
         ,append=TRUE)
    # plot the clusters
    plot(kdat$cluster)
    plot(tdat$cluster)
}

print("MODEL")
