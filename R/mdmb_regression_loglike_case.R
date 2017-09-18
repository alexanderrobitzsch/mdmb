## File Name: mdmb_regression_loglike_case.R
## File Version: 0.08
## File Last Change: 2017-01-23 19:35:13

#**** evaluate individual likelihood
mdmb_regression_loglike_case <- function(y, linear.predictor, 
	fitted.values, type , beta, df  )
{
	np <- length(beta)
	#**********************
	# logistic regression
	if (type=="logistic"){
		loglike_case <- ifelse( y == 1 , fitted.values , 1 - fitted.values )
	}
	#**********************
	# yjt regression
	if (type=="yjt"){
		sigma <- beta[ np-1 ]
		lambda <- beta[ np ]
		loglike_case <- dyjt_scaled( y , location=linear.predictor , shape = sigma ,
							lambda = lambda , df = df )
	}
	#**********************
	# bct regression
	if (type=="bct"){
		sigma <- beta[ np-1 ]
		lambda <- beta[ np ]
		loglike_case <- dbct_scaled( y , location=linear.predictor , shape = sigma ,
							lambda = lambda , df = df )
	}	
	#------------------------------------
	#--- log-likelihood
	eps <- 1E-50
	loglike_case <- log( loglike_case + eps )
	#------------------------------------
	return(loglike_case)
}
	
