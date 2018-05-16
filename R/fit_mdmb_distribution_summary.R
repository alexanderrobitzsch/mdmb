## File Name: fit_mdmb_distribution_summary.R
## File Version: 0.17


#*******************************************************
# Summary for logistic_regression object
fit_mdmb_distribution_summary <- function( object , digits = 4 , file=NULL , ...)
{

    type <- object$type

    # open sink
    CDM::osink( file = file , suffix = paste0( "__SUMMARY.Rout") )

    cat("-----------------------------------------------------------------\n")
    d1 <- utils::packageDescription("mdmb")
    cat( paste( d1$Package , " " , d1$Version , " (" , d1$Date , ")" , sep="") , 
                "\n\n" )    
    cat( "Date of Analysis:" , paste( object$s2 ) , "\n" )
    cat("Computation Time:" , print(object$s2 - object$s1), "\n\n")
    
    cat("Call:\n", paste( deparse(object$CALL), sep = "\n", collapse = "\n"), 
                "\n\n", sep = "")    
    
    cat( object$description , "\n\n")
    
    cat("-----------------------------------------------------------------\n")    
    cat( "Number of observations =" , object$N , "\n" )        
    cat( "Number of estimated parameters = " , object$np , "\n" )        
    cat( "Deviance = " , round( object$deviance , 2 ) , "\n" )
    cat( "Log likelihood ="  , round( object$loglike , 2 ) , "\n" )    
        
    cat("-----------------------------------------------------------------\n")
    cat("Estimated Parameters\n")
    
    obji <- object$partable
    NC <- ncol(obji)
    for (ii in 2:NC){
        obji[,ii] <- round( obji[,ii] , digits)
    }
    print(obji)        
            
    # close sink
    CDM::csink( file = file )        
}
#*******************************************************
