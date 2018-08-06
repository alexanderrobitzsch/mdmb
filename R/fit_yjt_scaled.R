## File Name: fit_yjt_scaled.R
## File Version: 0.11

fit_yjt_scaled <- function(x, df=Inf, par_init=NULL, lambda_fixed=NULL, weights=NULL, probit=FALSE)
{
    CALL <- match.call()
    res <- fit_mdmb_distribution(x=x, type="yjt_scaled", df=df, par_init=par_init,
                weights=weights, lambda_fixed=lambda_fixed, probit=probit)
    res$CALL <- CALL
    return(res)
}
