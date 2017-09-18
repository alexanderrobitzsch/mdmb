## File Name: fit_mdmb_distribution_extract_results.R
## File Version: 0.05
## File Last Change: 2017-01-23 19:35:11

fit_mdmb_distribution_extract_results <- function( res0, lambda_fixed, 
		is_lambda_fixed, parnames)
{
	loglike <- - res0$value
	deviance <- -2*loglike		
	coefs <- res0$par
	np <- length(coefs)
	np0 <- length(parnames)
	if ( is_lambda_fixed ){	
		coefs <- c( coefs , lambda_fixed )
	}	
	names(coefs) <- parnames
	
	vcovs <- solve( res0$hessian )
	if ( is_lambda_fixed ){
		vcovs1 <- matrix( 0 , nrow=np0 , ncol=np0 )
		vcovs1[ 1:np , 1:np ] <- vcovs 	
		vcovs <- vcovs1
	}	
	rownames(vcovs) <- colnames(vcovs) <- parnames	
	
	#--- output
	res <- list( coefs = coefs, vcovs=vcovs, loglike=loglike, deviance=deviance)
	return(res)
}
