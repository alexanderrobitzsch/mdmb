## File Name: fit_oprobit.R
## File Version: 0.151

fit_oprobit <- function(x, par_init=NULL, weights=NULL)
{
    CALL <- match.call()
    res <- fit_mdmb_distribution(x=x, type='oprobit', df=Inf, par_init=par_init,
                    weights=weights)
    res$CALL <- CALL
    return(res)
}
