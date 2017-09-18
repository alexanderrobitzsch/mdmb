## File Name: predict.logistic_regression.R
## File Version: 0.13
## File Last Change: 2017-01-23 19:35:14

predict.logistic_regression <- function( object , newdata = NULL , ...)
{	
	if ( ! is.null(newdata) ){
		Xdes <- stats::model.matrix( object$formula , newdata )
		offset_values <- offset_values_extract(formula= object$formula, data=newdata )		
	} else {
		Xdes <- object$X
		offset_values <- object$offset_values		
	}
	beta <- coef(object)
	linear_predictor <- Xdes %*% beta + offset_values
	fitted.values <- stats::plogis( linear_predictor )			
	return(fitted.values)
}
