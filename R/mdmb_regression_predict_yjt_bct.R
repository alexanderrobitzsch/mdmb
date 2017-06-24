
mdmb_regression_predict_yjt_bct <- function( object , newdata = NULL, trafo=TRUE, ...)
{
	if ( ! is.null(newdata) ){
		Xdes <- stats::model.matrix( object$formula, newdata )
		offset_values <- offset_values_extract(formula=object$formula, data=newdata )		
	} else {
		Xdes <- object$X
		offset_values <- object$offset_values		
	}
	beta <- coef(object)
	np <- length(beta)
	beta <- beta[ seq(1,np-2) ]
	fitted.values <- Xdes %*% beta + offset_values
	# retransform parameter onto the original metric
	class_regr <- class(object)
	if ( ! trafo ){
		lam0 <- beta[np]
		if ( class_regr == "yjt_regression"){	
			fitted.values <- yj_antitrafo( fitted.values , lambda = lam0 )
		}
		if ( class_regr == "bct_regression"){	
			fitted.values <- bc_antitrafo( fitted.values , lambda = lam0 )
		}		
	}
	return(fitted.values)	
}
