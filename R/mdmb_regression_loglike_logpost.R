## File Name: mdmb_regression_loglike_logpost.R
## File Version: 0.11

mdmb_regression_loglike_logpost <- function(mod, beta , beta_prior, is_prior, type, 
		is_lambda_fixed)
{
	logpost <- - mod$value	
	if (is_prior){	
		logprior <- eval_prior_list_sumlog( par=beta , par_prior = beta_prior )
	} else {
		logprior <- 0
	}
	loglike <- logpost - logprior
	hessian <- mod$hessian
	parnames <- names(beta)
	np <- length(beta)
	vcov1 <- MASS::ginv(hessian)
	if ( ( type %in% c("yjt","bct") ) & ( is_lambda_fixed ) ){
		vcov0 <- matrix( 0 , nrow=np , ncol=np )	
		vcov0[1:(np-1),1:(np-1)] <- vcov1
		vcov1 <- vcov0	
	}				
	#--- output
	res <- list(loglike=loglike, logpost=logpost, logprior=logprior,
			hessian=hessian, vcov1=vcov1)
	return(res)
}
