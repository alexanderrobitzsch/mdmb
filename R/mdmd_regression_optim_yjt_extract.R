## File Name: mdmd_regression_optim_yjt_extract.R
## File Version: 0.01
## File Last Change: 2017-08-19 17:33:46

mdmd_regression_optim_yjt_extract <- function(x, index_beta, eps_shape, index_sigma,
		lambda_fixed, is_lambda_fixed, index_lambda )
{
	np <- length(x)
	beta <- x[index_beta]
	shape <- max( eps_shape , x[ index_sigma ] )
	if ( is_lambda_fixed ){
		lambda <- lambda_fixed 
	} else {
		lambda <- x[ index_lambda ]
	}
	#--- output
	res <- list( beta=beta, lambda=lambda, shape=shape, np=np)
	return(res)	
}
			
