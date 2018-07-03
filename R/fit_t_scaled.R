## File Name: fit_t_scaled.R
## File Version: 0.18

fit_t_scaled <- function(x, df=Inf, par_init=NULL, weights=NULL)
{
    CALL <- match.call()
    res <- fit_mdmb_distribution(x=x, type="t_scaled", df=df, par_init=par_init,
                weights=weights)
    res$CALL <- CALL
    return(res)
}
