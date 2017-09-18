## File Name: fit_yjt_scaled.R
## File Version: 0.07
## File Last Change: 2017-01-23 19:35:11

fit_yjt_scaled <- function(x, df=Inf, par_init=NULL, lambda_fixed=NULL, weights=NULL)
{
	CALL <- match.call()
	res <- fit_mdmb_distribution(x=x, type="yjt_scaled", df=df, par_init=par_init, 
				weights=weights, lambda_fixed = lambda_fixed)
	res$CALL <- CALL
	return(res)
}
