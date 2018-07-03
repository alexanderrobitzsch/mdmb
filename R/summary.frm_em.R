## File Name: summary.frm_em.R
## File Version: 0.34


#*******************************************************
# Summary for frm_em object
summary.frm_em <- function( object, digits=4, file=NULL, ...)
{
    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------\n")
    sirt::sirt_summary_print_package_rsession(pack="mdmb")

    mdmb_summary_print_computation_time(object=object)

    sirt::sirt_summary_print_call(CALL=object$CALL)

    cat("-----------------------------------------------------------------\n")
    cat( "Number of observations","=", object$ic$N, "\n" )
    cat( "Number of iterations","=", object$iter, "\n\n" )
    cat( "Deviance","=", round( object$ic$deviance, 2 ), "\n" )
    cat( "Log likelihood","=", round( object$ic$loglike, 2 ), "\n" )

    cat("\n")
    cat( "Number of estimated parameters","=", object$ic$np, "\n" )


    #--- Model equations
    cat("-----------------------------------------------------------------\n")
    cat("Descriptive Statistics (obtained from EM Algorithm)\n")
    obji <- object$desc_vars
    CDM::cdm_print_summary_data_frame(obji, digits=digits, from=2)

    #--- Predictor matrix
    cat("-----------------------------------------------------------------\n")
    cat("Predictor Matrix\n")
    print( object$predictorMatrix)

    #--- Model equations
    cat("-----------------------------------------------------------------\n")
    cat("Model Equations\n")
    ind0 <- object$ind0
    NM <- attr(ind0,"NM")
    models_vec <- rep(NA,NM+1)
    for (mm in seq( NM+1, 1 ) ){
        ind_mm <- ind0[[mm]]
        formula_mm <- frm_formula_character(ind_mm)
        model_mm <- paste0("Model ", mm, ": ", formula_mm)
        models_vec[mm] <- model_mm
        cat(" *", model_mm,"\n")
    }

    #--- Nodes for numerical integration
    cat("-----------------------------------------------------------------\n")
    cat("Nodes for Numerical Integration\n")
    ind0 <- object$ind0
    NM <- attr(ind0,"NM")
    for (mm in seq( NM+1, 1 ) ){
        ind_mm <- ind0[[mm]]
        nodes_string <- frm_em_summary_print_nodes( dv_vars=ind_mm$dv_vars,
                            nodes=ind_mm$nodes,
                            nodes_description=ind_mm$nodes_description,
                            desc_vars=object$desc_vars )
        cat( nodes_string, "\n")
    }

    cat("-----------------------------------------------------------------\n")
    cat("Estimated Parameters\n")

    partable <- object$partable
    for (mm in seq(NM+1,1)){
        #mm <- 2
        cat("\n*************************************\n")
        cat(models_vec[mm],"\n\n")
        partable_mm <- partable[ partable$model==mm, ]
        partable_mm$model <- NULL
        obji <- partable_mm
        rownames(obji) <- NULL
        iii <- seq( which( colnames(obji)=="est" ), ncol(obji) )
        for (ii in iii){
            obji[,ii] <- round( obji[,ii], digits)
        }
        if ( nrow(obji) > 0 ){
            print(obji)
        } else {
            cat("No estimated coefficients.\n")
        }
        cat("\n")
        ind_mm <- ind0[[mm]]
        if (ind_mm$model=="linreg"){
            all_coefs <- object$all_coefs[[mm]]
            AC <- length(all_coefs)
            v1 <- paste0("Explained variance R^2=", round( all_coefs[AC], digits ), "\n")
            cat(v1)
        }
        if (ind_mm$model %in% c("logistic") ) {
            all_coefs <- object$all_coefs[[mm]]
            AC <- length(all_coefs)
            v1 <- paste0("Pseudo R^2 (McKelvey & Zavoina)=",
                    round( all_coefs[AC], digits ),    "\n")
            cat(v1)
        }
    }

    CDM::csink( file=file )
}
#*******************************************************
