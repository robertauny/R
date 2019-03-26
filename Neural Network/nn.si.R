#!/usr/bin/env Rscript

############################################################################
##
## File:      nn.si.R
##
## Purpose:   Use time series methods for doing predictive analytics
##            on schedule interruptions.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Apr. 16, 2015
##
############################################################################

library("RJDBC")
library("forecast")
library("nnet")
library("neuralnet")
library("NeuralNetTools")

# remove all created objects
rm(list=ls())

# validate the model
validate <- NULL
while( is.null(validate) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n VALIDATION RUN ? (1 = y OR 2 = n)                                                            ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 1 ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        validate <- 1
    }
    else {
        validate <- as.numeric(opt[1])
        if( validate < 1 || validate > 2 ) {
            validate <- NULL
        }
    }
    if( validate == 2 ) {
        # simulate the future
        tot <- NULL
        while( is.null(tot) ) {
            # we'll ask the user for input and proceed with what they give
            cat("\n\n\n NUMBER SIMULATIONS (1 - 10)                                                          ")
            opt <- scan(what=character(),nmax=1,quiet=TRUE)
            # default option = 3 ... otherwise, loop until we get a valid option
            if( is.null(opt) || length(opt) == 0 ) {
                tot <- 3
            }
            else {
                tot <- as.numeric(opt[1])
                if( tot < 1 || tot > 10000 ) {
                    tot <- NULL
                }
            }
        }
    }
    else {
        tot <- 3
    }
}

# ATA
ata <- NULL
while( is.null(ata) ) {
    # we'll ask the user for input and proceed with what they give
    # yet, through experimentation, the default was determined
    cat("\n\n\n ENTER ATA (e.g. 78-30) :                                                                     ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # no default option ... otherwise, loop until we get a valid option
    if( !(is.null(opt) || length(opt) == 0) ) {
        ata <- opt
        # ATA as a function of ...
        ataby <- c("AC_AGE"
                  ,"FLIGHT_LENGTH"
                  ,"FLIGHT_HOURS"
                  ,"CYCLES"
                  ,"UTILIZATION"
                  ,"LAST_EVENT_TIME")
        by <- NULL
        while( is.null(by) ) {
            # we'll ask the user for input and proceed with what they give
            cat("\n\n\n BY 1 = AC_AGE
    2 = FLIGHT_LENGTH
    3 = FLIGHT_HOURS
    4 = CYCLES
    5 = UTILIZATION
    6 = LAST_EVENT_TIME ? :  ")
            opt <- scan(what=character(),nmax=1,quiet=TRUE)
            # default option = 1 ... otherwise, loop until we get a valid option
            if( is.null(opt) || length(opt) == 0 ) {
                by <- 1
            }
            else {
                by <- as.numeric(opt[1])
                if( by < 1 || by > 6 ) {
                    by <- NULL
                }
            }
        }
    }
    else {
        ata <- NULL
    }
}

# prediction period in years
years <- NULL
while( is.null(years) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n PREDICTION PERIOD (# YEARS) #                                                                ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 2 ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        if( by == 1 ) {
            years <- 2
        }
        else {
            years <- 5
        }
    }
    else {
        years <- as.numeric(opt[1])
        if( years < 1 || years > 5 ) {
            years <- NULL
        }
    }
}

# model bias
bias <- NULL
while( is.null(bias) ) {
    # we'll ask the user for input and proceed with what they give
    # yet, through experimentation, the default was determined
    cat("\n\n\n ENTER MODEL BIAS BETWEEN 0 & 1 :                                                             ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option depends on model ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        if( by == 1 ) {
            bias <- 1.0
        }
        if( by == 2 ) {
            bias <- 0.00001
        }
        if( by == 3 ) {
            bias <- 0.15
        }
        if( by == 4 ) {
            bias <- 0.05
        }
        if( by == 5 ) {
            bias <- 0.10
        }
        if( by == 6 ) {
            bias <- 0.25
        }
    }
    else {
        bias <- as.numeric(opt[1])
        if( bias < 0 || bias > 1 ) {
            bias <- NULL
        }
    }
}

print("OPTIONS")

# set up the driver parameters
drv <- JDBC('com.mysql.jdbc.Driver'
           ,'/opt/mysql/mysql-connector-java-5.1.34-bin.jar'
           ,identifier.quote="'")

print("DRIVER")

# connection
user <- Sys.getenv("USER")
pass <- ""

conn <- dbConnect(drv
                 ,'jdbc:mysql://localhost:3306/ARMS'
                 ,user
                 ,pass)

print("CONN")

# SQL to get the data to be used in the model of schedule interruptions
# we are modeling a conditional distribution of schedule inerruptions
# given the age of the aircraft (in cycles)
# this is for the purposes of comparing all time series models
# with the neural network as a model of the normal approximation
# to the binomial
#
# SQL to get the data to be used in the model of schedule interruptions
oataby <- ataby[by]
if( ataby[by] == "AC_AGE" ) {
    nataby    <- paste(",CYCLES",sep="")
    aataby    <- oataby
    ataby[by] <- paste("CEIL(DATEDIFF(CURRENT_TIME                                                                         "
                      ,"             ,STR_TO_DATE(DELIVERY_DT                                                              "
                      ,"                         ,'%c/%e/%Y'))/30)*30                                       AS             "
                      ,oataby
                      ,",CEIL(CYCLES/1000)*1000                                                             AS CYCS        "
                      ,sep="")
}
if( ataby[by] == "FLIGHT_LENGTH" ) {
    nataby    <- paste(",CYCLES,",oataby,sep="")
    aataby    <- "FLENGTH"
    ataby[by] <- paste("CEIL(FLIGHT_LENGTH)                                                                 AS             "
                      ,aataby
                      ,",CEIL(CYCLES/1000)*1000                                                             AS CYCS        "
                      ,sep="")
}
if( ataby[by] == "FLIGHT_HOURS" ) {
    nataby    <- paste(",CYCLES,",oataby,sep="")
    aataby    <- "HOURS"
    ataby[by] <- paste("CEIL(                                                                                              "
                      ,oataby
                      ,"/1000)*1000                                                                         AS             "
                      ,aataby
                      ,",CEIL(CYCLES/1000)*1000                                                             AS CYCS        "
                      ,sep="")
}
if( ataby[by] == "CYCLES" ) {
    nataby    <- paste(",",oataby,sep="")
    aataby    <- "CYCS"
    ataby[by] <- paste("CEIL(                                                                                              "
                      ,oataby
                      ,"/1000)*1000                                                                         AS             "
                      ,aataby
                      ,sep="")
}
if( ataby[by] == "UTILIZATION" ) {
    nataby    <- paste(",CYCLES,",oataby,sep="")
    aataby    <- "UTIL"
    ataby[by] <- paste("CEIL(UTILIZATION)                                                                   AS             "
                      ,aataby
                      ,",CEIL(CYCLES/1000)*1000                                                             AS CYCS        "
                      ,sep="")
}
if( ataby[by] == "LAST_EVENT_TIME" ) {
    nataby    <- paste(",CYCLES",sep="")
    aataby    <- oataby
    ataby[by] <- paste("DATEDIFF(CURRENT_TIME                                                                              "
                      ,"        ,STR_TO_DATE(EVENT_DT                                                                      "
                      ,"                    ,'%c/%e/%Y'))                                                   AS             "
                      ,oataby
                      ,",CEIL(CYCLES/1000)*1000                                                             AS CYCS        "
                      ,sep="")
}
query <- paste(" SELECT SUM(N_SCHED_INTR)                                                                   AS INTR        "
              ,"      , ATA_4_D,                                                                                           "
              ,         ataby[by]
              ,"      , EXTRACT(MONTH FROM STR_TO_DATE(EVENT_DT,'%c/%e/%Y'))                                AS MONTH       "
              ,"      , EXTRACT(YEAR  FROM STR_TO_DATE(EVENT_DT,'%c/%e/%Y'))                                AS YEAR        "
              ,"      ,@MEDIAN := (SELECT CASE WHEN CNT % 2 <> 0 THEN (CNT + 1) / 2                                        "
              ,"                          ELSE (CNT/2)+0.5                                                                 "
              ,"                          END                                                                              "
              ,"                   FROM (SELECT COUNT(*) AS CNT FROM ARMS.SCHEDULE) D)                      AS MEDIAN      "
              ,"      , ROWN                                                                                               "
              ,"      ,@PLANES := (SELECT CAST(PLANES AS UNSIGNED)                                                         "
              ,"                   FROM ARMS.BCS_PLANES                                                                    "
              ,"                   WHERE MONTH = EXTRACT(MONTH FROM STR_TO_DATE(MNTH_END_DT,'%c/%e/%Y'))                   "
              ,"                   AND   YEAR  = EXTRACT(YEAR FROM STR_TO_DATE(MNTH_END_DT,'%c/%e/%Y')))    AS PLANES      "
              ,"      , IPLANES                                                                                            "
              ," FROM (SELECT DATE_FORMAT(STR_TO_DATE(DELIVERY_DT,'%Y-%c-%d'),'%c/%e/%Y')                   AS DELIVERY_DT "
              ,"      ,       DATE_FORMAT(STR_TO_DATE(EVENT_DT,'%Y-%c-%d'),'%c/%e/%Y')                      AS EVENT_DT    "
              ,"      ,       ATA_4_D                                                                                      "
              ,               nataby
              ,"      ,       N_SCHED_INTR                                                                                 "
              ,"      ,      @ROWNUMBER := @ROWNUMBER + 1                                                   AS ROWN        "
              ,"      ,      @IPLANES := (SELECT COUNT(DISTINCT ID) FROM ARMS.SCHEDULE)                     AS IPLANES     "
              ,"       FROM ARMS.SCHEDULE, (SELECT @ROWNUMBER := 0) E                                                      "
              ,"       ORDER BY EVENT_DT) A                                                                                "
              ," WHERE ATA_4_D =                                                                                          '"
              ,                  ata
              ,"'                                                                                                          "
              ," GROUP BY ATA_4_D,                                                                                         "
              ,           aataby
              ," ORDER BY ATA_4_D,                                                                                         "
              ,           aataby
              ,";                                                                                                          "
              ,sep="")

print("QUERY")

# execute the query and get rid of the first row
dat  <- dbGetQuery(conn,query) 

# the extra points make a non-zero number modulo period=per
# then we'll align our data according to this period
per  <- 1

# get rid of some rows for the graph
#dat  <- dat[-1,]

# median and row numbers for the capture defined below
med  <- as.numeric(dat$MEDIAN)
rown <- as.numeric(dat$ROWN)

# get the data in correct form and extract all columns individually
mth  <- as.numeric(dat$MONTH)
# as.numeric takes mod 2000 to adjust numbering
yr   <- as.numeric(dat$YEAR)

# planes accrue 100 additional cycles per week
# and planes hitting the 60000 cycle threshold are
# removed from the fleet since they are no longer safe to fly
#
# as such, we need to adjust the plane counts based upon these
# 2 facts/assumptions ... the way that we will handle the removals
# is to make some assumptions
#
# 1. we will take the fleet as one atomic entity with the accumulated
# cycles in a given month evenly distributed across all sub-entities that
# make up the entire entity i.e., all planes currently in operation during
# the month evenly accumulated the same number of cycles during the month
# this may not be reality, but the reason for the assumption will become
# more clear below
#
# 2. because each sub-entity accumulates the same number of cycles during
# a given month, we will assume that wear due to the accumulation of cycles
# on the atomic entity (the fleet) is evenly distributed across the whole
# of its entirety
#
# this is being done because we want to assume uniformity of wear
# across the entirety of the atomic entity ... as such, when it comes to
# wear across the atomic entity, we will make the assumption that one sub
# entity is taking the hit for the rest, so that from one month to the next,
# we will remove a plane (or fractional part thereof) from the fleet as
# a result of the accumulation of additional cycles, without having to be
# concerned about which of the planes actually has reached the 60000
# cycle threshold, if any at all ... the wear is on the entire entity
# and we just remove a percentage of the entity, according to the amount
# of wear, at least for modeling purposes
#
cyc  <- as.numeric(dat$CYCS)
pln  <- as.numeric(dat$PLANES)
ipln <- as.numeric(dat$IPLANES)
adj  <- function() {
    fac = 1/28
    cycs <- seq(1,min(length(pln),length(cyc)),)
    plns <- pln
    if( length(cycs) > 1 ) {
        for( i in 2:length(pln) ) {
            if( mth[i] == 1  ||
                mth[i] == 3  ||
                mth[i] == 5  ||
                mth[i] == 7  ||
                mth[i] == 8  ||
                mth[i] == 10 ||
                mth[i] == 12 ) {
                mult = fac * 31
            }
            else {
                if( mth[i] == 4 ||
                    mth[i] == 6 ||
                    mth[i] == 9 ||
                    mth[i] == 11 ) {
                    mult = fac * 30
                }
                else {
                    mult = fac * 28
                }
            }
            if( i > length(cycs) ) {
                cycs[i] <- cycs[i] + (mult*pln[i-1])
            }
            else {
                cycs[i] <- cyc[i]
            }
            plns[i] <- plns[i] - (cycs[i]-cycs[i-1])/60000
        }
    }
    else {
        plns <- plns - cycs/60000
    }
    return(plns)
}
pln  <- adj()
#
freq <- as.numeric(dat$INTR)
# normalize for fleet levels at the time
# and normalize for fleet levels attaining the number
# of indep. var. units
freq <- freq * (pln/ipln) # (pln/max(pln))

# conversion of freq to an approximately normally distributed random variable
#
# in a typical regression model whereby we
# start with the assumption that the model of our observations
# is the sum of a deterministic function plus some additive noise
#
# in the case of linear regression, the assumption is that the
# deterministic function is a line and the noise is gaussian, with
# mean 0 and standard deviation of 1, with an implicit assumption
# that the data are linearly separable
#
# in the case of non-linear regression, we don't have the added assumption
# that our data are linearly separable, so we will not be concerned
# about meeting (nor checking) this requirement, since the set of data
# observations which lie on the regression line or on the wrong side of
# the regression line is a set of probability measure zero (i.e. it's a
# finite set (or possibly countably infinite, if we generate a countably
# infinite set of observations, which won't happen)
# hence, we can ignore separability
#
# by converting the original frequencies into a normal random
# variable, we turn the problem into one of estimating the density/distribution
# of a population drawn from a Gaussian process with variance given by
# the variance of the error in the model and mean given by our model
# of the deterministic function mentioned above
#
# we could very well estimate the model parameters (weights/coefficients)
# of our model by means of a maximum likelihood or any other technique
# that makes efficient use of an assumed normal density ... it is my hope
# that this conversion will make the model of the data more accurate
#
# because of the linearity of the mean operator, it is an easy task
# to convert from the best estimate of our deterministic function,
# which is always the mean of the distribution, as determined by the data,
# when the criteria is minimization of the modeling error
# as such, we get a model of the original data set after converting back
mf   <- mean(freq)
vf   <- mean((freq-mf)^2)
freq <- (freq-mf)/sqrt(vf)

# independent variable
vby  <- as.numeric(with(dat,get(aataby)))
if( aataby == "LAST_EVENT_TIME" ) {
    # for last event time, we are going to simulate
    # Oracle's lag function in code rather than in query
    pre <- vby[1:(length(vby)-1)]
    nxt <- vby[2:length(vby)]
    # in performing the lag, we lose an x-axis value at
    # the end which we will simulated by mean imputation
    #
    # this mean imputation will be accomplished by
    # subtracting the average change in value from
    # the last value in the array and using the greater
    # of it and zero (times can't be less than zero, unless
    # you want to consider the past, beyond the current
    # data set's beginning, which we don't want to do)
    #
    # note that because of vby = current date minus event date
    # and the number of days between the previous date and the
    # current date is always >= the number of days between
    # the current date and the next date, then subtracting pre
    # date from the nxt date gives the length of time between dates
    #
    lag <- nxt - pre
    av  <- 0
    for( i in 1:(length(lag)-1) ) {
        av <- av + abs(lag[i+1]-lag[i])
    }
    vby <- c(lag,av/length(av))
    # taking the inverse of decreasing time will cause
    # time to increase in the x-axis
    # 
    # also, this is best for display, as we expect that the
    # number of schedule interruptions increases with decreasing
    # time since last event and we want to display an increasing
    # trend as a function of decreasing time since last event
    vby <- 1/vby
}

# bind the useful data together
bdat <- cbind(freq,vby)

# get the missing values from the fit of the original
# data set that may have had holes in it
len  <- function() {
    l <- max(bdat[,2])-min(bdat[,2])
    if( length(bdat[,2]) >= 3 ) {
        for( i in 3:length(bdat[,2]) ) {
            nl <- abs(bdat[i,2]-bdat[i-1,2])
            if( 0 < nl && nl < l ) {
                l <- nl
            }
        }
    }
    return(l)
}
s    <- seq(min(bdat[,2]),max(bdat[,2]),len())
s    <- sort(unique(c(s,bdat[,2])))
tvar <- 0
sidx <- c()
if( length(s) != length(bdat[,2]) ) {
    bbnd <- as.matrix(cbind(rep(0,length(s)),s),ncol=2)
    id   <- match(s,bdat[,2])
    cnt  <- 1
    # to fill the holes in our data set, we will
    # loop through the current data set and identify
    # the holes, then use the chosen model to get a prediction
    # of the frequency value that we expect to be in that hole
    #
    # the data have been converted to a normal distribution ... thus, for smoothing
    # purposes, we will actually use this information when building
    # the model of the data to fill the hole
    #
    # what follows amounts to mean imputation, whereby we estimate the data
    # with the mean (fit) and use this information to fill any holes in
    # the data set to make the model a bit more robust
    ncnt <- 0
    for( i in id ) {
        if( is.na(i) ) {
            # let's keep track of all of the values which are
            # estimated via the process of mean imputation
            # using this neural net
            sidx <- c(sidx,cnt)
            # create a data frame for assignment in the prediction
            # and for use in building the model
            y  <- bbnd[1:(cnt-1),1]
            x  <- bbnd[1:(cnt-1),2]
            df <- data.frame(y,x)
            # build a neural net here since we need an estimator
            # to fill in holes in the original data set
            #
            # assumed normality of the data set after conversion makes
            # estimation of the holes in the data set 
            # to be a lot more robust, since normality is a safe assumption
            # by making an application of the central limit theorem
            # and judging by the data, a normal approximation is a good
            # one for frequencies of interruptions
            #
            # linear terms in the indep. var. and dnorm(indep. var.) plus their interaction
            # is the formula used to produce the frequencies for those
            # methods that require a formula in addition to raw data
            tnn <- neuralnet(# number of sched. interruptions should be modeled as a function of
                             # the number of the indep. variable,
                            ,data=df
                            ,formula=y~x
                             # 1 node for each variable in the model within the hidden layer
                             # per "A Neural Network Model of Schedule Interruptions"
                             # not necessary to do deep learning with 10 or more hidden layers
                            ,hidden=1
                            ,lifesign="minimal"
                            ,linear.output=FALSE # model will be non-linear
                            ,threshold=0.1)
            # compute the missing value using the neural net
            bbnd[cnt,1] <- compute(tnn,s[cnt])$net.result
            tvar <- tvar + tnn$result.matrix[1,1]
            ncnt <- ncnt + 1
        }
        else {
            bbnd[cnt,1] <- bdat[i,1]
        }
        cnt  <- cnt + 1
    }
    bdat <- bbnd
    freq <- bdat[,1]
    vby  <- bdat[,2]
    if( !(ncnt == 0) ) {
        tvar <- tvar/ncnt
    }
}

print("DATA")

# align the period/frequency as the number of most recent data points
# that forms the largest multiple of per just defined above
idx  <- 1:length(bdat[,1])
if( validate == 1 ) {
    # only capture the data before the median for now
    # the rest will be used in validation
    # aside from this, the neural network will model
    # a conditional distribution
    #idx <- which(rown<=med[length(med)])
    idx  <- idx[1:ceiling(length(idx)/(5/3))]
    sdat <- bdat[-idx,]
    bdat <- bdat[idx,]
}

# make a time series
myts <- (sqrt(vf)*ts(bdat[,1]
                    ,frequency=per
                    ,start=bdat[1,2])) + mf

# plot the historical data
sfrq <- (sqrt(vf)*bdat[,1]) + mf
pyr  <- (max(bdat[,2])-min(bdat[,2]))/(max(yr)-min(yr)) # x-axis units per year (on avg)
if( by == 1 ) {
    xstr <- "AC Age (Days)"
    pyr  <- 365
}
if( by == 2 ) {
    xstr <- "Flight Length"
}
if( by == 3 ) {
    xstr <- "Flight Hours"
}
if( by == 4 ) {
    xstr <- "AC Age (Cycles)"
}
if( by == 5 ) {
    xstr <- "Utilization"
}
if( by == 6 ) {
    xstr <- "(Inverse of) Time Since Last Event"
}
plot(bdat[1:2,2]
    ,myts[1:2]
    ,type="l"
    ,col="blue"
    ,xlab=xstr
    ,ylab="Schedule Interruptions"
    ,xlim=c(min(bdat[,2]),max(bdat[,2])+(years*pyr))
    ,ylim=c(min(sfrq),max(sfrq)+(1.0*(max(sfrq)-min(sfrq)))))
text(0.5*((max(bdat[,2])+(years*pyr))-min(sfrq))
    ,max(sfrq)+(0.35*(max(sfrq)-min(sfrq)))
    ,paste("ATA: ",ata,sep=""))
for( i in idx[2:(length(idx)-1)] ) {
    lines(bdat[i:(i+1),2],myts[i:(i+1)],type='l',col=ifelse(i%in%sidx,'red','blue'))
}

#
# build the neural net here since in the default case
# we will just go with a prediction using an nn model
# of the original data
#
# assumed normality of the data set after conversion makes
# estimation of the holes in the data set for the ARIMA model
# to be a lot more robust, since normality is a safe assumption
# by making an application of the central limit theorem
#
nn   <- neuralnet(# number of sched interruptions should be modeled as a function of
                  # the number of cycles,
                 ,data=bdat
                 ,formula=bdat[,1]~bdat[,2]
                  # 1 nodes for each variable in the model within the hidden layer
                  # per "A Neural Network Model of Schedule Interruptions"
                  # not necessary to do deep learning with 10 or more hidden layers
                 ,hidden=1
                 ,lifesign="minimal"
                 ,linear.output=FALSE # model will be non-linear
                 ,threshold=0.1)

# future time
tf  <- function() {
    a <- cbind(yr,mth)
    x <- a[order(a[,1],a[,2]),]
    if( x[length(x)/2,2]%%12 == 0 ) {
        ret <- cbind(x[length(x)/2,1]+1,1)
    }
    else {
        ret <- cbind(x[length(x)/2,1]
                    ,x[length(x)/2,2]%%12+1)
    }
    for( i in seq(2,years*12,) ) {
        if( ret[length(ret[,1]),2]%%12 == 0 ) {
            ret <- rbind(ret,cbind(ret[length(ret[,1]),1]+1,1))
        }
        else {
            ret <- rbind(ret
                        ,cbind(ret[length(ret[,1]),1]
                              ,ret[length(ret[,1]),2]%%12+1))
        }
    }
    return(ret)
}

# new fractional time calculations so we can plot
# the prediction beginning at the end of the historical data plot
ttf  <- as.matrix(tf(),ncol=2)
a    <- max(bdat[,2])+((length(ttf[,1])/12)*pyr*years)
b    <- a - (length(freq)-length(myts))*(pyr/12)
ab   <- (a-b)/(pyr/12)
nttf <- seq(max(bdat[,2])
           ,max(bdat[,2])+((length(ttf[,1])/12)*pyr*years)
           ,pyr/12)
if( validate == 1 ) {
    if( ab > 0 ) {
        nttf <- nttf[1:min(length(nttf),ab)]
    }
    else {
        nttf <- nttf[which(!is.na(nttf))]
    }
}

# actuals of the fit of the data to be used later
nfit <- bias*((sqrt(vf)*compute(nn,nttf)$net.result[,1])+mf)

#
# auto regressive integrated moving average model of our residuals
#
# order of polynomial of constants is c(p,d,q) where p = AR, d = I, q = MA from ARIMA
#
# p = 1 means that the random variable (observation) at the current time
# is a linear function of the most recent observation (1 back in time)
# plus some additive white noise (Gaussian, mean zero, constant variance)
# plus some deterministic constant
#
# d = 0 references seasonality whereby the order of our observations
# don't change with respect to time, so that different periods in time
# which are of the same length will always appear to be the same in seasonality
# as such, d > 0 requires that our random process is not stationary (seasonal)
#
# q = 10 means that if we model our observations as a linear combo of
# constants of order p times some random variable, then this is equivalent
# to a linear combination of constants of order q=10 times some random white noise process
#
# for a more complete explanation see
# http://en.wikipedia.org/wiki/Autoregressive_integrated_moving_average
#
# from the site and the preceding explanation, seasonal should be more clear
#
# conditional sum of squares is the default method applied for finding the AR coefficients
#
# instead, we will force arima to use maximum likelihood estimation, so that those
# coefficients don't have to be between -1 and 1, which is required for a stationary
# model (as noted above, we are assuming a non-stationary model and we are not
# differencing the series in order to produce a stationary model out of this (assumed)
# non-stationary model)
#
# instead, we are forcing the return of a stationary model by setting the period to be 1
# and applying maximum likelihood estimation, which will return an asymptotically normally
# distributed model of the data from a forced stationary time series with period 1
#
# maximum likelihood estimation (MLE), as you may recall, is a method of density estimation whereby
# we know the density upto some unknown parameters, which we would like to estimate in order
# to give the best estimate of noisy data, where the assumption is made that the data are
# independently sampled from some common distribution (IID), with fixed, constant mean and variance
#
rfit <- myts - (bias*((sqrt(vf)*compute(nn,time(myts))$net.result[,1])+mf))
fit  <- arima(rfit
             ,order=c(1,1,10)
             ,method="ML"
             ,seasonal=list(order=c(1,0,0),period=per))

# here, we are going to calculate the residual errors from the fit
# we need this so that we can get a better estimate of the "future"
# by predicting the errors over the nn fit and adding them to the
# nn estimate of the future
#
# why do something like this ? because the fit is just an estimate of the mean
# so that the future fit is just an estimate of the future mean
# but we want to model the future data, which is the sum of the mean and the error
#
# we will use a slightly biased estimate of the mean (fit) for modeling purposes
# recall that the best estimator is unbiased, with minimum variance, where
# minimum variance is defined as achieving the Rao-Cramer lower bound,
# defined as the true variance in the estimator divided by the number
# of points which make up the estimate for a gaussian model
#
# also, bias in the estimate is calculated as the expectation of the estimator
#
# since the estimator is linear (i.e. E(aX) = aE(X) where "a" is a real number
# and "X" is a random variable), then we can simply multiply the estimator (fit)
# by a bias constant to add bias to the estimator
#
# we will be adding a bit of bias to the estimator in order to get a more
# "natural" fit of the data when we add the biased estimator to the
# model of the residuals, both in the validation and the future prediction
#

# ARIMA model of a Gaussian approximation of the original data set to
# predict the biased residual fit at a future time determined by the user
pred <- predict(fit,n.ahead=length(nttf),se.fit=TRUE)

# convert the prediction to the prior distribution
pprd <- (bias*pred$pred) + nfit

# variance in the prediction is the sum
# of the errors from the predictions of the
# residuals and the fit
#
# note that by independence of the fit, hole fitter and the residuals
# the total variance is just the sum of the variances
# and because of the bias in the mean (fit), we have an extra
# squared bias term as a multiplier
var  <- (bias^2)*(pred$se^2+nn$result.matrix[1,1]+tvar)

#
# find the quantiles associated with a p-value of 0.01
#
# in a hypothesis test where the null hypothesis is that the predicted mean
# of our distribution of sched interruptions a function of the indep. var.
# is given by "fit" above, we want the frequency of sched interruptions
# associated with a probability of p=0.01, which is our level of
# significance, defining the point at which we reject the null hypothesis, in
# favor of the alternative, which is that "fit" will not accurately model
# our future observations and minimize the error in the fit, i.e. "fit" is not
# the mean of our distribution
#
# what we will have then, is a 99% confidence interval about the mean, indicating
# that we are 99% confident that the mean (i.e. our model of the observations) lies
# within the band about "fit"
#
qinf <- qnorm(.01,pprd,sd=sqrt(var))
qsup <- qnorm(.99,pprd,sd=sqrt(var))

idx <- seq(max(idx),max(idx)+length(nttf)-1,)

# plot the original data for validation
# using the prediction interval in question
if( validate == 1 ) {
    #ts2 <- (sqrt(vf)*ts(sdat[1:length(idx),1]
    ts2 <- (sqrt(vf)*ts(freq[idx]
                       ,frequency=per
                       ,start=nttf[1])) + mf
    lines(nttf,ts2,type="l",col="blue")
    # print the ratio of standard errors as a measure
    # of goodness of fit
    #
    # traditionally, we would use R^2 as a measure of
    # goodness ... however, for non-linear models, this
    # statistic is not good to use as values are typically
    # outside the range of zero to one, where
    # values close to zero are the best and values closer
    # to one indicates that the fit is no better than
    # the simple mean of the data, when the model is linear
    #
    # as R^2 is an expression of the percentage of error in
    # data that's ultimately captured by the model (bad for
    # generalization of the model), S is the ratio of standard error
    # that's been captured by the model (bad for generalization again)
    #
    # thus, as in R^2 for linear models, we want our non-linear model to
    # generalize well, which is tantamount to S values that are closer to zero
    print(paste("S = "
               ,sqrt(mean((pprd-mean(pprd))^2))/sqrt(mean((ts2-mean(ts2))^2))
               ,sep=""))
}

# in the event that we are doing a forecast
# we should have a mean of our distribution of future
# data and a confidence region that takes a
# bell shape as in the normal distribution shape, with
# the mean of the distribution being the obtained above
#
# however, this eventuality is fortuitous, as now we can
# sample from the normal distribution
#
# then we will have the prediction from the mathematics
# which tends to the mean of the most recent history,
# by the Markov property and the central limit theorem
# as well as a simulated future, by sampling from the
# normal distribution
# 
# similar to an averaged ensemble of decision trees
# over randomly sampled (with replacement) subsets of a
# data set which is to be classified, we will run a number
# of simulations of the future, as determined by the user
# and return the average of the ensemble
#
# the prediction will always tend to the mean
# but, i take advantage of the fact that the variance of the prediction
# is bell shaped,which is indicative of a normal distribution
#
# so the actual data, from which the mean will be derived, is normal
#
# thus, i sample, by way of a normal distribution, to get the red prediction
#
# i simulate that kind of way a number of times, then i average the simulations
#
# the mathematics is so good, that the simulations tend to the mean,
# which is another result that is gauranteed by the law of large numbers from probability

idx <- idx - rep(min(idx)-1,length(idx))
smp <- rep(0,length(idx))
avg <- rep(0,length(idx))
cnt <- 0
while( cnt < tot ) {
    for( i in idx ) {
        smp[i] <- rnorm(1,sd=(qsup-qinf)[i]/2) + pprd[i]
    }
    avg <- avg + smp
    cnt <- cnt + 1
}

avg <- avg/tot
avg[which(avg<min(myts))] <- min(myts)

lines(avg~nttf,type="l",col="red")

print("MODEL")
