#!/usr/bin/env Rscript

############################################################################
##
## File:      db.R
##
## Purpose:   Database utilities
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Apr. 11, 2016
##
############################################################################

library("RRedshiftSQL",warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("DBI"         ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("RPostgreSQL" ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("RMySQL"      ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("dplyr"       ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("foreach"     ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("iterators"   ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)

source("config.R")

# defined constants
ta.db.constants.user               <- "user"
ta.db.constants.pass               <- "pass"
ta.db.constants.host               <- "host"
ta.db.constants.port               <- "port"
ta.db.constants.db                 <- "db"
ta.db.constants.actuals            <- "actuals"
ta.db.constants.predictions        <- "predictions"
ta.db.constants.table              <- "table"
ta.db.constants.vtable             <- "vtable"
ta.db.constants.num                <- "num"

ta.db.constants.postgres.name      <- "postgres"
ta.db.constants.mysql.name         <- "mysql"

ta.db.constants.misc.zero          <- 0
ta.db.constants.misc.one           <- 1

ta.db.constants.misc.num           <- ta.config.functions.search(ta.config.variables
                                                                ,ta.db.constants.postgres.name
                                                                ,ta.db.constants.num)

# postgres disconnect
ta.db.functions.disconnect.postgres <- function(conn=NULL) {
    if( !is.null(conn) ) {
        dbl  <- dbListResults(conn)[1][[1]]
        if( !is.null(dbl) ) dbClearResult(dbl)
        dbDisconnect(conn)
    }
}

# mysql disconnect
ta.db.functions.disconnect.mysql    <- ta.db.functions.disconnect.postgres

# postgres connection
ta.db.functions.connect.postgres    <- function() {
    # user
    user <- ta.config.functions.search(ta.config.variables
                                      ,ta.db.constants.postgres.name
                                      ,ta.db.constants.user)
    # password
    pass <- ta.config.functions.search(ta.config.variables
                                      ,ta.db.constants.postgres.name
                                      ,ta.db.constants.pass)
    # host
    host <- ta.config.functions.search(ta.config.variables
                                      ,ta.db.constants.postgres.name
                                      ,ta.db.constants.host)
    # database
    db   <- ta.config.functions.search(ta.config.variables
                                      ,ta.db.constants.postgres.name
                                      ,ta.db.constants.db)
    # port number
    port <- ta.config.functions.search(ta.config.variables
                                      ,ta.db.constants.postgres.name
                                      ,ta.db.constants.port)
    # driver connection
    drv  <- dbDriver("PostgreSQL")
    # disconnect any lingering connections
    conns<- dbListConnections(PostgreSQL())
    if( !is.null(conns) ) for( conn in conns ) ta.db.functions.disconnect.postgres(conn)
    # connection
    conn <- dbConnect(drv
                     ,dbname=db
                     ,host=host
                     ,port=port
                     ,user=user
                     ,password=pass)
    return(conn)
}

# mysql connection
ta.db.functions.connect.mysql <- function() {
    # connection
    conn <- dbConnect(MySQL(),"TA")
    return(conn)
}

# get the data
ta.db.functions.data <- function(con=NULL,dcon=NULL,tbl=NULL,qt=NULL) {
    dat  <- NULL
    if( !(is.null(con) || is.null(tbl) || is.null(dcon)) ) {
        conn <- con()
        if( !is.null(conn) ) {
            qry  <- paste("SELECT * FROM",tbl,";")
            if( !is.null(qt) )
                qry  <- paste("SELECT * FROM \"",tbl,"\";",sep="")
            dat  <- dbGetQuery(conn,qry)
            dcon(conn)
        }
    }
    # return the data
    return(dat)
}

# find the label that appears the most
ta.db.functions.most <- function(vec=NULL) {
    ret  <- NULL
    if( !is.null(vec) ) {
        rvec <- vec
        uvec <- unique(rvec)
        cnt  <- 0
        for( r in 1:length(uvec) ) {
            l    <- length(which(rvec==uvec[r]))
            if( l > cnt ) {
                ret  <- uvec[r]
                cnt  <- l
            }
            else {
                # most frequent in the entire vec
                # in order to break ties
                if( l == cnt ) {
                    l1   <- which(vec==ret)
                    l2   <- which(vec==uvec[r])
                    if( length(l2) > length(l1) )
                        ret  <- uvec[r]
                }
            }
        }
    }
    return(ret)
}

# join the data
ta.db.functions.data.join <- function(ptbl=NULL
                                     ,pcol=NULL
                                     ,mtbl=NULL
                                     ,mcol=NULL
                                     ,act=NULL
                                     ,fld=NULL) {
    dat  <- NULL
    if( !(is.null(ptbl) ||
          is.null(pcol) ||
          is.null(mtbl) ||
          is.null(mcol) ||
          is.null(act )) ) {
        pdat <- ta.db.functions.data(ta.db.functions.connect.postgres
                                    ,ta.db.functions.disconnect.postgres
                                    ,ptbl
                                    ,1)
        mdat <- ta.db.functions.data(ta.db.functions.connect.mysql
                                    ,ta.db.functions.disconnect.mysql
                                    ,mtbl)
        if( !(is.null(pdat) || is.null(mdat)) ) {
            wcol <- which(colnames(mdat)==mcol)
            if( length(wcol) > 0 ) {
                colnames(mdat)[wcol] <- pcol
                dat                  <- suppressWarnings(left_join(data.frame(mdat)
                                                                  ,data.frame(pdat)
                                                                  ,by=pcol))
                if( !is.null(dat) ) {
                    num  <- NULL
                    if( !is.null(ta.db.constants.misc.num) ) {
                        nm   <- as.vector(unlist(strsplit(ta.db.constants.misc.num,",",fixed=TRUE)))
                        num  <- which(colnames(dat)%in%nm)
                    }
                    for( i in 1:ncol(dat) ) {
                        wrow <- which(is.na(dat[,i]))
                        if( length(wrow) > 0 ) {
                            if( !is.null(num) ) {
                                if( i %in% num ) val  <- median(as.numeric(dat[-wrow,i]))
                                else             val  <- ta.db.functions.most(dat[-wrow,i])
                            }
                            else
                                                 val  <- ta.db.functions.most(dat[-wrow,i])
                            dat[wrow,i] <- rep(val,length(wrow))
                        }
                    }
                    wact <- which(colnames(dat)==act)
                    if( length(wact) > 0 ) {
                        dat                      <- cbind(dat,rep(ta.db.constants.misc.zero,nrow(dat)))
                        colnames(dat)[ncol(dat)] <- ta.db.constants.actuals
                        wfld                     <- grep(fld,as.character(dat[,wact]))
                        if( length(wfld) > 0 ) dat[wfld,ncol(dat)] <- ta.db.constants.misc.one
                    }
                }
            }
        }
    }
    # return the data
    return(dat)
}
