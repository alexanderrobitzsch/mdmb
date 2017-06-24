
fit_mdmb_distribution_logLik_extract <- function( object ){
    # extract log-likelihood
    out <- - object$deviance / 2 
    # number of parameters
    attr(out, "df") <- object$np
    # extract number of observations
    attr(out, "nobs") <- object$N
    class(out) <- "logLik"
    return(out)
}
