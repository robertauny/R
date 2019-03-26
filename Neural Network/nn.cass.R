#!/usr/bin/env Rscript

############################################################################
##
## File:      nn.cass.R
##
## Purpose:   A neural network for maximizing cost savings, with cassandra db.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Jan. 21, 2016
##
############################################################################

library("RJDBC")
#library("forecast")
#library("nnet")
#library("neuralnet")
#library("NeuralNetTools")

# remove all created objects
rm(list=ls())

# nodes the model
dbg <- NULL
while( is.null(dbg) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n # Debug (0 - No OR 1 - Yes) ?                                 ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 10 ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        dbg <- 1
    }
    else {
        dbg <- as.numeric(opt[1])
        if( !( dbg == 0 || dbg == 1) ) {
            dbg <- NULL
        }
    }
}

# nodes the model
nodes <- NULL
while( is.null(nodes) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n # Nodes (1 - 10) ?                                            ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 10 ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        nodes <- 10
    }
    else {
        nodes <- as.numeric(opt[1])
        if( nodes < 1 ) {
            nodes <- NULL
        }
    }
}

# nodes the model
pct <- NULL
while( is.null(pct) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n # Percent unaffected users (0.80 - 1.0) ?                     ")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 0.80 ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        pct <- 0.80
    }
    else {
        pct <- as.numeric(opt[1])
        if( pct < 0.80 || pct > 1.0 ) {
            pct <- NULL
        }
    }
}

# modeling constant
con <- NULL
while( is.null(con) ) {
    # we'll ask the user for input and proceed with what they give
    cat("\n\n\n # Modeling constant ( > 0.0 ) ?                        \n\n\n")
    opt <- scan(what=character(),nmax=1,quiet=TRUE)
    # default option = 0.80 ... otherwise, loop until we get a valid option
    if( is.null(opt) || length(opt) == 0 ) {
        con <- 1
    }
    else {
        con <- as.numeric(opt[1])
        if( con < 0.0 ) {
            con <- NULL
        }
    }
}

if( dbg == 1 ) print("DRIVER")

# set up the driver parameters
drv <- JDBC('org.apache.cassandra.cql.jdbc.CassandraDriver'
           ,'/opt/cassandra/cassandra_driver.jar'
           ,identifier.quote="'")

if( dbg == 1 ) print("CONN")

# connection
conn <- dbConnect(drv,'jdbc:cassandra://192.168.1.65:9160/intuit')

if( dbg == 1 ) print("QUERY")

# CQL to get the data to be used in the model of counts of screen users
query1 <- paste(" SELECT SCREEN                      "
               ,"      , COST                        "
               ," FROM INTUIT.SCREEN_COSTS;          "
               ,sep="")
query2 <- paste(" SELECT SCREEN                      "
               ,"      , USER                        "
               ," FROM INTUIT.SCREEN_USERS;          "
               ,sep="")

# execute the query and get rid of the first row
dat1 <- dbGetQuery(conn,query1) 
dat2 <- dbGetQuery(conn,query2) 

if( dbg == 1 ) print("DATA")

# cassandra doesn't support joins nor counts
# so we have to simulate a join and distinct
# count here after collecting our data into variables
screen1 <- dat1$screen
screen2 <- dat2$screen
users   <- as.numeric(dat2$user)
costs   <- as.numeric(dat1$cost)

idx1 <- match(screen1,screen2)
idx2 <- match(screen2,screen1)

screen <- screen1[!is.na(idx1)]
users  <- users[!is.na(idx2)]
costs  <- costs[!is.na(idx1)]

if( dbg == 1 ) print("GAUSSIAN")

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
mf    <- mean(users)
vf    <- mean((users-mf)^2)
users <- (users-mf)/sqrt(vf)

if( dbg == 1 ) print("INITIAL")

a  <- matrix(data=0.0,nrow=length(users),ncol=nodes)
b  <- matrix(data=0.0,nrow=length(a[,1]),ncol=length(a[1,]))
H  <- matrix(data=0.0,nrow=length(a[,1]),ncol=length(a[1,]))

if( dbg == 1 ) print("KERNEL")

# generate random normal sampling for the functions that model the data
for( i in 1:nodes ) {
    a[,i] <- rnorm(length(a[,i]))
    b[,i] <- rnorm(length(b[,i]))
    # for use in an extreme neural network
    # we require a function with random parameters,
    # which will be multiplied by the weights to approx.
    # the given function
    #
    # we normalized the data and we will approximate
    # that normal distribution with normal densities
    # where each of the normal densities has randomly
    # generated mean and variance, per the requirements
    # in the setup of the extreme neural network
    #
    # we will divide the unit interval into length(users)
    # slices to represent each screen, where length(users)
    # contains exactly the number of screens in the data
    #
    # the original normal densities didn't work as well as
    # intended.  thus, a quadratic approximation of the
    # densities is used instead, with much better results
    #H[,i] <- exp(-b[,i]^{-2}*sum((users-a[,i])^2))
    H[,i] <- sqrt(b[,i]^{2}+sum((users-a[,i])^2))
}

if( dbg == 1 ) print("WEIGHTS")

# matrix of target values where
# we will approximate the values with an
# inverse of nodes term so that in the end
# we will have greater than pct*users
# unaffected by the unmodified screens
#Tm <- (nodes)^{-1}*users;
Tm <- users;

# matrix of synaptic weights which are computed
# for each of the nodes in the extreme neural net
#
# the idea is that we have one node for each of the
# non-linear moment conditions in the Taylor series
# expansion of the density function from which the
# sampled data comes ... as you may recall, the more
# terms in the expansion of the Taylor series we have
# the better estimate of the density function value
#
# for each data point, we generate a new expansion
# (row in H) and get a better estimate of the value
# through the expansion ... the more data we have,
# the better the estimate
#
# in addition, the more nodes we have, the more terms
# in Taylor series expansion of the density function,
# which is why the dot product of the weights with the random
# functions converges to the density in the limit (theorem)
beta <- solve(((con)^{-1}*diag(nodes))+(t(H)%*%H))%*%t(H)%*%Tm
if( length(users) <= nodes ) {
    beta <- t(H)%*%solve(((con)^{-1}*diag(length(users)))+(H%*%t(H)))%*%Tm
}

if( dbg == 1 ) print("SCREENS")

# we will loop through different possibilities of
# the threshold on each of the pct and the value
# at which we will consider the betas values
# to be one for binary weighting, which will tell
# us which of the screens to drop to maximize cost
pcosts <- 0.0
# for each percent of unaffected users > 80%
# until we can maximize costs
ones <- rep(1.0,length(users))
# randomly select 5 values between pct
# and 1.0 when checking for a good pct value
sq   <- pct + (round(100*(1.0-pct)*runif(5))/100)
for( j in sq ) {
    I     <- diag(length(users))
    tbeta <- rep(0.0,length(users))
    # for each screen that we can do without
    # updating while keeping unaffected > 80%
    # and while maximizing costs
    #
    # each row in H corresponds to user counts
    # for the screen represented by that row
    # we want to zero out the row for each
    # node (column) in H and measure its effects
    # both on the percentage of affected users
    # and the associated costs, once we have
    # calulated which screens to not update
    jt  <- j %*% Tm
    sjt <- sum(jt)
    # randomly use 5 distinct values
    for( k in 1:length(users) ) {
        # zero out the row corresponding to the screen
        Hn     <- H
        Hn[k,] <- rep(0.0,nodes)
        # we will test to make sure that none
        # of the nodes violates our conditions
        # for unaffected users and while still
        # maximizing costs
        hb  <- Hn %*% beta
        shb <- sum(hb)
        if( dbg == 1 ) {
            print( c("shb = ",shb) )
            print( c("sjt = ",sjt) )
        }
        if( shb < sjt ) {
            tbeta[k] <- 1.0
        }
    }
    # once we finish populating the changes to
    # the vector that gets us the list of screens
    # to replace, we want to know if the associated
    # cost savings are greater than for the last vec
    ncosts <- t(ones-tbeta)%*%costs
    if( pcosts < ncosts ) {
        kbeta  <- tbeta
        pcosts <- ncosts
    }
}

if( dbg == 1 ) print("MODEL")

# list of screens to not update will have zero values
print(kbeta)
