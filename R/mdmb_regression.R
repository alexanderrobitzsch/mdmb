

mdmb_regression <- function( formula , data , type , weights = NULL,
	beta_init = NULL, beta_prior = NULL , df = Inf , lambda_fixed = NULL ,
	control = NULL )
{
	CALL <- match.call()	
	s1 <- Sys.time()
	res <- remove_NA_data_frame( formula=formula , data=data , weights=weights)
	data <- res$data
	weights <- res$weights
	Xdes <- stats::model.matrix( object=formula , data=data )
	N <- nrow(Xdes)	
	offset_values <- offset_values_extract(formula=formula, data=data )
	Ndes <- ncol(Xdes)
	parnames <- colnames(Xdes)
	if (type %in% c("yjt","bct") ){
		parnames <- c( parnames , "sigma" , "lambda" )
	}		
	dv_var <- as.character( formula[[2]] )
	y <- data[,dv_var]
	#*** lambda fixed
	is_lambda_fixed <- TRUE
	if (  ( type %in% c("yjt","bct") ) & ( is.null( lambda_fixed ) ) ){
		is_lambda_fixed <- FALSE
	}
	
	if ( is.null(beta_init) ){
		par <- rep(0,Ndes)
		if ( type %in% c("yjt","bct") ){
			sd_y <- stats::sd(y)
			if ( is_lambda_fixed ){
				par <- c( par , sd_y  )
			} else {
				par <- c( par , sd_y , 1 )
			}
		}		
	} else {
		par <- beta_init
	}
	
	is_prior <- FALSE
	if ( ! is.null( beta_prior ) ){
		is_prior <- TRUE
	}
	
	x <- par	
	eps <- 1E-50
	index_sigma <- NULL
	index_lambda <- NULL
	
	#-----------------------------------------
	#---- define optimization function 
	
	#***********************************
	#*** logistic regression
	if ( type=="logistic"){
		description <- "Logistic regression"
		np <- length(x)
		index_beta <- 1:np
		fct_optim <- function(x){
			yp <- stats::plogis( Xdes %*% x + offset_values )
			ll_i <- ifelse( y == 1 , yp , 1 - yp )
			ll <- - sum( weights * log( ll_i + eps ) )
			#--- include prior distributions
			if (is_prior){
				ll <- ll - eval_prior_list_sumlog( par=x , par_prior = beta_prior )
			}
			#--- output
			return(ll)
		}    
	}
	#***********************************
	#*** yjt regression
	if ( type %in% c("yjt","bct") ){
		np <- length(x)
		index_beta <- seq(1,np-2 + is_lambda_fixed)
		index_sigma <- np - 1 + is_lambda_fixed
		if ( ! is_lambda_fixed){
			index_lambda <- np
		}
		eps_shape <- .01
		if (type=="yjt"){
			description <- paste0( "Scaled t regression with Yeo-Johnson transformation (df=" , df , ")")
			dens_fct <- dyjt_scaled
		}
		if (type=="bct"){
			description <- paste0( "Scaled t regression with Box-Cox transformation (df=" , df , ")")
			dens_fct <- dbct_scaled			
		}
		
		#--- define optimization function
		fct_optim <- function(x){
			np <- length(x)
			beta <- x[index_beta]
			shape <- max( eps_shape , x[ index_sigma ] )
			if ( is_lambda_fixed ){
				lambda <- lambda_fixed 
			} else {
				lambda <- x[ index_lambda ]
			}
			y_pred <- Xdes %*% beta + offset_values
			ll_i <- dens_fct( y , location=y_pred, shape=shape, lambda=lambda,
						df = df )
			ll <- - sum( weights * log( ll_i + eps ) )
			#--- include prior distributions
			if (is_prior){
				ll <- ll - eval_prior_list_sumlog( par=x , par_prior = beta_prior )
			}
			#--- output
			return(ll)
		}    
	}
	#***********************************					

	
	#-------------------------------------------
	#---- optimization using optim
	mod1 <- stats::optim( par=par , fn=fct_optim , method = "L-BFGS-B", 
				hessian = TRUE , control=control)		
				
	#--- extract parameters
	beta <- mdmb_regression_extract_parameters( mod=mod1 , parnames=parnames , 
				type=type , is_lambda_fixed=is_lambda_fixed , 
				lambda_fixed = lambda_fixed )			
				
	#--- extract log-likelihood, log prior and log-posterior
	res0 <- mdmb_regression_loglike_logpost(mod=mod1, beta=beta , 
				beta_prior=beta_prior, is_prior=is_prior, type=type, 
				is_lambda_fixed = is_lambda_fixed)
	loglike <- res0$loglike
	logprior <- res0$logprior
	logpost <- res0$logpost	
	hessian <- res0$hessian
	vcov1 <- res0$vcov1
	
	#--- predictions
	res0 <- mdmb_regression_predict( Xdes=Xdes , beta=beta , 
				offset_values=offset_values , type=type , y=y, index_beta=index_beta )	
	linear.predictor <- res0$linear.predictor
	fitted.values <- res0$fitted.values
	
	#---- individual likelihood
	loglike_case <- mdmb_regression_loglike_case(y=y, 
						linear.predictor=linear.predictor, 
						fitted.values=fitted.values, type=type , beta = beta , df=df )	
						
	#---- information criteria
	deviance <- - 2 * loglike
	ic <- mdmb_regression_ic( N=N, beta=beta , deviance=deviance , type=type,
				index_beta = index_beta,
				index_sigma=index_sigma , index_lambda=index_lambda )
	
	#---- summary table
	partable <- mdmb_regression_summary_table( beta=beta, vcov1=vcov1 )
	
	#---- calculate R^2
	R2 <- mdmb_regression_R2( linear.predictor=linear.predictor,  y=y,
				type=type, beta = beta, index_sigma = index_sigma )
	s2 <- Sys.time()
	
	#--- output	
	res <- list(coefficients=beta, vcov = vcov1,
			partable = partable , 
			y = y , X = Xdes , weights = weights , 
			fitted.values = fitted.values , linear.predictor = linear.predictor , 
			loglike = loglike , deviance = deviance , 
			logprior = logprior , logpost = logpost , 
			like_case = loglike_case ,  ic = ic , formula = formula , 
			offset_values = offset_values , 
			R2 = R2 , parnames = parnames , beta_prior = beta_prior , df = df , 
			is_prior = is_prior , fct_optim = fct_optim, type = type , 
			CALL = CALL , converged = mod1$converged , 
			iter = mod1$counts["function"] , description = description , s1=s1, s2=s2
			)
	class(res) <- "mdmb_regression"
	return(res)
}
