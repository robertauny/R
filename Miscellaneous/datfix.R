#!/usr/bin/env Rscript

############################################################################
##
## File:      datfix.R
##
## Purpose:   Use spark to analyze titanic data.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Jun 28, 2017
##
############################################################################

library("sparklyr")

# remove all created objects
rm(list=ls())

# the training data
dir  <- paste(Sys.getenv("HOME"),"/data/datarpm/",sep="")
fl   <- paste(dir,"train.csv",sep="")

# read the data file
dat  <- read.csv(fl)

# only get the passenger ID and age columns
wcol <- which(colnames(dat)%in%c("PassengerId","Age"))

if( length(wcol) > 0 ) {
    sdat <- dat[,wcol]
    age  <- which(colnames(sdat)=="Age")
    na   <- which(is.na(sdat[,age]))
    if( !(length(na)%in%c(0,length(sdat))) ) {
        # clone the data for the
        # first imputation method
        c1     <- sdat[,age]
        c1[na] <- rep(mean(as.numeric(c1[-na])),length(na))
        #write(c1,"/tmp/test.out")
        # clone the data for the
        # second imputation method
        c2 <- sdat[,age]
        na <- which(is.na(c2))
        for( i in na ) {
            # previous non-null value
            p <- max(which(!is.na(c2[1:(i-1)])))
            # next non-null value
            n <- min(which(!is.na(c2[(i+1):length(c2)])))
            if( i == 1 )
                # take the next non-null value
                c2[i] <- c2[n]
            else {
                if( i == length(c2) )
                    # take the previous non-null value
                    c2[i] <- c2[p]
                else
                    # otherwise take the mean of the
                    # previous and next non-null values
                    c2[i] <- mean(as.numeric(c2[c(p,n)]))
            }
        }
        sdat[,age] <- c2
        #ord        <- order(as.numeric(sdat[,-age]),as.numeric(sdat[,age]))
        ord        <- order(as.numeric(sdat[,-age]))
        sdat       <- sdat[ord,]
        #write(c2,"/tmp/test.out",append=TRUE)
        write(sdat[,age],"/tmp/test.out",ncolumns=1)
        # modify the training data
        ndat <- dat[ord,]
        ndat[,which(colnames(ndat)=="Age")] <- c2
        #write(t(ndat),"/tmp/test.out",ncolumns=ncol(ndat),sep=",",append=TRUE)
        # get the right columns
        ag   <- which(colnames(ndat)=="Age")
        fr   <- which(colnames(ndat)=="Fare")
        em   <- which(colnames(ndat)=="Embarked")
        # use the spark cluster to generate
        # a random forest of passenger embark
        # as a function of the passenger's
        # age and fare
        dmp1 <- ndat[,c(ag,fr,em)]
        #write(t(dmp1)
             #,"/tmp/test.out"
             #,ncolumns=ncol(dmp1)
             #,sep=","
             #,append=TRUE)
        # connect to the local spark cluster
        sc   <- spark_connect(master="local"
                             ,spark_home="/usr/local/spark"
                             ,version="2.1.1")
        sdat1<- copy_to(sc,dmp1,overwrite=TRUE)
        trn  <- ml_random_forest(sdat1
                                ,response="Embarked"
                                ,features=c("Age","Fare")
                                ,type="classification"
                                ,na.action=NULL)
        # read the data file
        fl   <- paste(dir,"test.csv",sep="")
        dat1 <- read.csv(fl)
        # get the right columns
        ag   <- which(colnames(dat1)=="Age")
        fr   <- which(colnames(dat1)=="Fare")
        # using the trained model, predict the test data
        # on the same independent variables of
        # the passenger's age and fare
        dmp2 <- dat1[,c(ag,fr)]
        sdat2<- copy_to(sc,dmp2,overwrite=TRUE)
        test <- sdf_predict(trn,newdata=sdat2)
        # print the result of the test prediction
        write("******* SUMMARY *******","/tmp/test.out",append=TRUE)
        write(summary(test),"/tmp/test.out",append=TRUE)
        # disconnect from the spark cluster
        spark_disconnect(sc)
    }
}
