## File Name: fit_mdmb_distribution_logLik_extract.R
## File Version: 0.09

fit_mdmb_distribution_logLik_extract <- function( object )
{
    out <- - object$deviance / 2
    attr(out, "df") <- object$np
    attr(out, "nobs") <- object$N
    class(out) <- "logLik"
    return(out)
}
