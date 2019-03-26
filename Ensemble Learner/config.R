#!/usr/bin/env Rscript

############################################################################
##
## File:      config.R
##
## Purpose:   Parse the XML document for config values.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Sep. 25, 2016
##
############################################################################

library("XML"      ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("parallel" ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)
library("doMC"     ,warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)

source('utils.R')

# configuration file
ta.config.constants.file           <- ta.utils.functions.inst("file")
ta.config.constants.file           <- ifelse(is.null(ta.config.constants.file)
                                            ,"/opt/ta/ta.xml"
                                            ,ta.config.constants.file)

ta.config.constants.node           <- "node"
ta.config.constants.item           <- "item"
ta.config.constants.value          <- "value"

ta.config.constants.name           <- "ta"
ta.config.constants.type           <- "type"

# detect cores for multi-core parallel processing
ta.config.constants.dc             <- detectCores()
if( is.na(ta.config.constants.dc) )
    ta.config.constants.dc         <- 1
registerDoMC(min(6,ta.config.constants.dc))

# parse the configuration file or
# return the name of the config file
ta.config.functions.parse <- function(f=ta.config.constants.file) {
    # config file will take the form
    #
    # <config>
    #      <node>
    #          <item>Value</item>
    #          <item>Value</item>
    #          <item>Value</item>
    #      </node>
    #      <node>
    #          <item>Value</item>
    #          <item>Value</item>
    #          <item>Value</item>
    #      </node>
    #      <node>
    #          <item>Value</item>
    #          <item>Value</item>
    #          <item>Value</item>
    #      </node>
    # </config>
    #
    # where node can be something like "db" for assigning the database items, "url", "user", "pass"
    ret <- NULL
    if( file.exists(f) && file.size(f) > 0 ) {
        mat1 <- c()
        mat2 <- c()
        mat3 <- c()
        tdat <- xmlTreeParse(f)
        #dat  <- tdat[[1]]$children for multiple config blocks
        dat  <- tdat[[1]]$children$config
        for( i in 1:length(dat) ) {
            node <- dat[[i]]
            for( j in 1:length(node) ) {
                item <- node[j]
                xmln <- xmlName(node)
                xmli <- xmlName(item[[1]])
                xmlv <- xmlValue(item[[1]])
                if( !(length(xmln) == 0 || length(xmli) == 0 || length(xmlv) == 0) ) {
                    mat1 <- c(mat1,xmln)
                    mat2 <- c(mat2,xmli)
                    mat3 <- c(mat3,xmlv)
                }
            }
        }
        ret           <- matrix(cbind(mat1,mat2,mat3),ncol=3)
        colnames(ret) <- c(ta.config.constants.node
                          ,ta.config.constants.item
                          ,ta.config.constants.value)
    }
    else
        ret  <- ta.config.functions.parse()
    return(ret)
}

# search the configuration info for
# the config parms and the value
ta.config.functions.search <- function(mat=NULL,node=NULL,item=NULL) {
    ret <- NULL
    if( !(is.null(mat) || is.null(node) || is.null(item)) ) {
        coln <- which(colnames(mat)==ta.config.constants.node)
        coli <- which(colnames(mat)==ta.config.constants.item)
        colv <- which(colnames(mat)==ta.config.constants.value)
        for( n in 1:nrow(mat) )
            if( mat[n,coln] == node && mat[n,coli] == item )
                ret  <- as.character(mat[n,colv])
    }
    return(ret)
}

# configuration information
ta.config.variables <- ta.config.functions.parse()
