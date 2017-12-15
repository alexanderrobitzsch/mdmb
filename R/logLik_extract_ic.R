## File Name: logLik_extract_ic.R
## File Version: 0.04

logLik_extract_ic <- function( object )
{
    out <- - object$ic$deviance / 2 
    attr(out, "df") <- object$ic$np
    attr(out, "nobs") <- object$ic$n
    class(out) <- "logLik"
    return(out)
}
