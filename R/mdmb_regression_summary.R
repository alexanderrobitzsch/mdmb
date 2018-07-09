## File Name: mdmb_regression_summary.R
## File Version: 0.42


#*******************************************************
# Summary for logistic_regression object
mdmb_regression_summary <- function( object, digits=4, file=NULL, ...)
{

    type <- object$type

    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------\n")
    # package and R session
    mdmd_summary_print_model_description(object=object, pack="mdmb")

    cat( object$description, "\n\n")

    cat("-----------------------------------------------------------------\n")
    cat( "Number of observations","=", object$ic$n, "\n" )
    cat( "Number of iterations","=", object$iter, "\n\n" )
    cat( "Deviance","=", round( object$ic$deviance, 2 ), "\n" )
    cat( "Log likelihood","=", round( object$loglike, 2 ), "\n" )
    cat( "Log prior","=", round( object$logprior, 2 ), "\n" )
    cat( "Log posterior","=", round( object$logpost, 2 ), "\n" )

    cat("\n")
    cat( "Number of estimated parameters","=", object$ic$np, "\n" )

    cat( "  Number of estimated beta parameters","=", object$ic$np.beta, "\n" )
    cat( "  Number of estimated sigma parameters","=", object$ic$np.sigma, "\n" )
    cat( "  Number of estimated lambda parameters","=", object$ic$np.lambda, "\n" )
    cat( "  Number of estimated threshold parameters","=", object$ic$np.thresh, "\n" )
    cat("\n")

    cat( "AIC","=", round( object$ic$AIC, 2 ), " | penalty","=",
                round( object$ic$AIC - object$ic$deviance,2 ),
                "   | AIC=-2*LL + 2*p  \n" )
    cat( "AICc","=", round( object$ic$AICc, 2 )," | penalty","=",
                round( object$ic$AICc - object$ic$deviance,2 ) )
    cat("    | AICc=-2*LL + 2*p + 2*p*(p+1)/(n-p-1)  (bias corrected AIC)\n" )
    cat( "BIC","=", round( object$ic$BIC, 2 ), " | penalty","=",
                round( object$ic$BIC - object$ic$deviance,2 ),
            "   | BIC=-2*LL + log(n)*p  \n" )
    cat( "CAIC","=", round( object$ic$CAIC, 2 )," | penalty","=",
                round( object$ic$CAIC - object$ic$deviance,2 ) )
    cat("   | CAIC=-2*LL + [log(n)+1]*p  (consistent AIC)\n\n" )

    cat("-----------------------------------------------------------------\n")
    cat("Estimated Parameters\n")
    obji <- object$partable
    CDM::cdm_print_summary_data_frame(obji, digits=digits, from=2)
    cat("\n")

    #*** print thresholds
    if (type %in% c("oprobit") ){
        cat("-----------------------------------------------------------------\n")
        cat("Threshold Parameters\n")
        print( round( object$thresh, digits ) )
        cat("\n")
    }

    #-----------------------------------------
    # Explained Variance

    #*** logistic regression
    if (type %in% c("logistic", "oprobit") ){
        cat("Pseudo R-Square (McKelvey & Zavoina)","=",
            round( object$R2, digits ), "\n" )
    }
    #*** yjt and bct regression
    if (type %in% c("yjt","bct") ){
        cat("R2","=",     round( object$R2, digits ), "\n" )
    }

    # close sink
    CDM::csink( file=file )
}
#*******************************************************
