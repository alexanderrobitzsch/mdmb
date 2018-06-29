## File Name: fit_mdmb_distribution_remove_NA.R
## File Version: 0.07

fit_mdmb_distribution_remove_NA <- function( x, weights )
{
    # remove missing values
    ind <- ! is.na(x)
    if ( is.null(weights) ){
        n <- length(x)
        weights <- rep(1,n)
    }
    x <- x[ind]
    weights <- weights[ind]
    #--- output
    res <- list(x=x, weights=weights)
    return(res)
}
