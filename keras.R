#!/usr/bin/env Rscript

############################################################################
##
## File:      keras.R
##
## Purpose:   LSTM and MLP machine learning models using keras.
##
## Parameter: N/A
##
## Creator:   Robert A. Murphy
##
## Date:      Jan. 24, 2019
##
############################################################################

library("keras",warn.conflicts=FALSE,verbose=FALSE,quietly=TRUE)

atlas.ml.functions.model.lstm       <- function(units=NULL,rows=NULL,cols=NULL) {
    mod       <- NULL
    if( !(is.null(units) || is.null(rows) || is.null(cols)) ) {
        mod   <- keras_model_sequential()
        # create the model
        mod  %>% layer_cudnn_lstm(units=units,input_shape=list(rows/units,cols),return_sequences=TRUE) %>%
                 layer_batch_normalization()                                                           %>%
                 layer_cudnn_lstm(units=units                                  ,return_sequences=TRUE) %>%
                 layer_batch_normalization()                                                           %>%
                 layer_cudnn_lstm(units=units                                  ,return_sequences=TRUE) %>%
                 layer_batch_normalization()                                                           %>%
                 layer_conv_1d(filters=1
                              ,kernel_size=2
                              ,activation="sigmoid"
                              ,padding="same"
                              ,data_format="channels_last")
        # prepare the model for training
        mod  %>% compile(loss="binary_crossentropy",optimizer="adadelta")
    }
    return(mod)
}

atlas.ml.functions.model.mlp        <- function(units=NULL,rows=NULL,cols=NULL,clusters=NULL) {
    mod       <- NULL
    if( !(is.null(units) || is.null(rows) || is.null(cols) || is.null(clusters)) ) {
        mod   <- keras_model_sequential()
        # create the model
        #
        # a fully connected MLP that with dropout to achieve a desired
        #
        # number of clusters in the output layer
        if( clusters < units ) {
            clst  <- units - clusters
            mod  %>% layer_dense(units=cols,input_shape=list(rows/units,cols),activation="relu"   ) %>%
                     layer_dropout(rate=0.5*clst)                                                   %>%
                     layer_dense(units=(clusters-0.5*clst)                   ,activation="relu"   ) %>%
                     layer_dropout(rate=0.5*clst)                                                   %>%
                     layer_dense(units=clusters                              ,activation="softmax") %>%
            # prepare the model for training
            mod  %>% compile(loss="categorical_crossentropy"
                            ,optimizer="adadelta"
                            ,metrics="accuracy")
        }
    }
    return(mod)
}

atlas.ml.functions.model.fit        <- function(fnc=NULL,dat=NULL,odat=NULL,units=NULL,save=NULL) {
    ret  <- NULL
    if( !(is.null(fnc)    ||
          is.null(dat)    ||
          nrow(dat) == 0  ||
          ncol(dat) == 0  ||
          is.null(odat)   ||
          nrow(odat) == 0 ||
          ncol(odat) == 0 ||
          units=NULL) ) {
        tdat  <- array(dat,c(1,nrow(dat),ncol(dat)))
        mod   <- fnc(units,nrow(dat),ncol(dat))
        ret   <- mod  %>% fit(tdat
                             ,odat
                             ,batch_size=nrow(dat)/units
                             ,validation_split=0.05)
        if( !is.null(save) ) mod  %>% save_model_hdf5(save)
    }
    return(ret)
}

atlas.ml.functions.lstm             <- function(dat=NULL,odat=NULL,units=NULL,save=NULL) {
    ret  <- return(atlas.ml.functions.model.fit(atlas.ml.functions.model.lstm,dat,odat,units,save))
}

atlas.ml.functions.mlp              <- function(dat=NULL,odat=NULL,units=NULL,save=NULL) {
    ret  <- return(atlas.ml.functions.model.fit(atlas.ml.functions.model.mlp,dat,odat,units,save))
}
