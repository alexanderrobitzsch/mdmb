

yjt_regression <- function( formula , data , weights = NULL,
	beta_init = NULL, beta_prior = NULL , df = Inf , lambda_fixed = NULL, 
	control = NULL )
{
	CALL <- match.call()
	type <- "yjt"
	#--- wrapping general regression function in miceadds
	res <- mdmb_regression( formula=formula , data=data , type=type , 
				weights = weights,	beta_init = beta_init, 
				beta_prior = beta_prior , df = df , lambda_fixed = lambda_fixed ,
				control = control )
	res$CALL <- CALL
	#--- additional informations about model type
	class(res) <- "yjt_regression"
	return(res)
}
