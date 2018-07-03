## File Name: fit_mdmb_distribution_summary.R
## File Version: 0.27


#*******************************************************
# Summary for logistic_regression object
fit_mdmb_distribution_summary <- function( object, digits=4, file=NULL, ...)
{

    type <- object$type

    # open sink
    CDM::osink( file=file, suffix=paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------\n")
    # package and R session
    sirt::sirt_summary_print_package_rsession(pack="mdmb")

    mdmb_summary_print_computation_time(object=object)

    sirt::sirt_summary_print_call(CALL=object$CALL)

    cat( object$description, "\n\n")

    cat("-----------------------------------------------------------------\n")
    cat( "Number of observations","=", object$N, "\n" )
    cat( "Number of estimated parameters","=", object$np, "\n" )
    cat( "Deviance","=", round( object$deviance, 2 ), "\n" )
    cat( "Log likelihood","=", round( object$loglike, 2 ), "\n" )

    cat("-----------------------------------------------------------------\n")
    cat("Estimated Parameters\n")

    obji <- object$partable
    CDM::cdm_print_summary_data_frame(obji, digits=digits, from=2)

    # close sink
    CDM::csink( file=file )
}
#*******************************************************
