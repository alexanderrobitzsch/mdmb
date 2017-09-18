## File Name: fit_bct_scaled.R
## File Version: 0.02
## File Last Change: 2017-01-23 19:35:11

fit_bct_scaled <- function(x, df=Inf, par_init=NULL, lambda_fixed=NULL, weights=NULL)
{
	CALL <- match.call()
	res <- fit_mdmb_distribution(x=x, type="bct_scaled", df=df, par_init=par_init, 
				weights=weights, lambda_fixed = lambda_fixed)
	res$CALL <- CALL
	return(res)
}
