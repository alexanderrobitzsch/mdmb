## File Name: mdmb_regression_R2.R
## File Version: 0.10
## File Last Change: 2017-01-23 19:35:13

#**** evaluate individual likelihood
mdmb_regression_R2 <- function( linear.predictor,  y, type, beta , index_sigma  )
{			
	R2 <- NULL
	np <- length(beta)
	
	#***** logistic regression
	#  R2 form McKelvey and Zavoina
	if (type=="logistic"){	
		var_y_pred <- stats::var(linear.predictor)
		var_resid <- 3.141593^2 / 3		
	}
	
	#***** yjt regression
	if (type %in% c("yjt","bct") ){	
		sigma <- beta[ index_sigma]
		var_y_pred <- stats::var(linear.predictor)
		if (type=="yjt"){
			yt <- yj_trafo( y = y , lambda = beta[np] )
		}
		if (type=="bct"){
			yt <- bc_trafo( y = y , lambda = beta[np] )
		}		
		var_y <- stats::var( yt	)
		var_resid <- var_y - var_y_pred
	}
	#*** R2 formula
	R2 <- var_y_pred / ( var_y_pred + var_resid )		
	#--- output
	return(R2)
}
	
