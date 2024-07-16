## File Name: summary.frm_fb.R
## File Version: 0.192


#*******************************************************
# Summary for frm_em object
summary.frm_fb <- function( object, digits=4, file=NULL, ...)
{
    # open sink
    CDM::osink( file=file, suffix=paste0( '__SUMMARY.Rout') )

    cat('-----------------------------------------------------------------\n')
    mdmb_summary_print_model_description(object=object, pack='mdmb')

    cat('-----------------------------------------------------------------\n')
    CDM::cat_paste( 'Number of observations', xx(), object$ic$N, '\n\n' )
    CDM::cat_paste( 'Number of iterations', xx(), object$iter, '\n' )
    CDM::cat_paste( 'Number of burnin iterations', xx(), object$burnin, '\n\n' )
    CDM::cat_paste( 'Number of estimated parameters', xx(), object$ic$np, '\n' )

    #--- Model equations
    cat('-----------------------------------------------------------------\n')
    cat('Descriptive Statistics (Imputed Values)\n')
    obji <- object$desc_vars
    CDM::cdm_print_summary_data_frame(obji, digits=digits, from=2)

    #--- Predictor matrix
    cat('-----------------------------------------------------------------\n')
    cat('Predictor Matrix\n')
    print(object$predictorMatrix)

    #--- Model equations
    cat('-----------------------------------------------------------------\n')
    cat('Model Equations\n')
    ind0 <- object$ind0
    NM <- attr(ind0,'NM')
    models_vec <- rep(NA,NM+1)
    for (mm in seq( NM+1, 1 ) ){
        ind_mm <- ind0[[mm]]
        formula_mm <- frm_formula_character(ind_mm)
        model_mm <- paste0('Model ', mm, ': ', formula_mm)
        models_vec[mm] <- model_mm
        cat(' *', model_mm,'\n')
    }

    cat('-----------------------------------------------------------------\n')
    cat('Estimated Parameters\n')

    partable <- object$partable
    for (mm in seq(NM+1,1)){
        cat('\n*************************************\n')
        cat(models_vec[mm],'\n\n')
        partable_mm <- partable[ partable$model==mm, ]
        partable_mm$model <- NULL
        obji <- partable_mm
        rownames(obji) <- NULL
        iii <- seq( which( colnames(obji)=='est' ), ncol(obji) )
        for (ii in iii){
            obji[,ii] <- round( obji[,ii], digits)
        }
        if ( nrow(obji) > 0 ){
            print(obji)
        } else {
            cat('No estimated coefficients.\n')
        }
    }

    #--- technical summary MCMC algorithm
    cat('\n-----------------------------------------------------------------\n')
    cat('MCMC Algorithm Informations\n\n')
    obji <- object$tech_summary
    i1 <- which( colnames(obji)=='Nsampled' )
    i2 <- which( colnames(obji)=='Rhat' )
    for (ii in i1:i2){
        obji[,ii] <- round( obji[,ii], digits=2 )
    }
    obji$effsize <- round( obji$effsize, digits=1 )
    print(obji)
    print( summary(object$values_coda) )

    # close sink
    CDM::csink( file=file )
}
#*******************************************************
