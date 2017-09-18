## File Name: logLik_extract_ic.R
## File Version: 0.03
## File Last Change: 2017-01-23 19:35:13

logLik_extract_ic <- function( object ){
    # extract log-likelihood
    out <- - object$ic$deviance / 2 
    # number of parameters
    attr(out, "df") <- object$ic$np
    # extract number of observations
    attr(out, "nobs") <- object$ic$n
    class(out) <- "logLik"
    return(out)
}
