## File Name: predict.oprobit_regression.R
## File Version: 0.04

predict.oprobit_regression <- function( object , newdata = NULL , ...)
{	
	if ( ! is.null(newdata) ){
		Xdes <- stats::model.matrix( object$formula , newdata )
		offset_values <- offset_values_extract(formula= object$formula, data=newdata )		
	} else {
		Xdes <- object$X
		offset_values <- object$offset_values		
	}
	beta <- object$coefficients[ object$index_beta ]
	linear_predictor <- Xdes %*% beta + offset_values
	return(linear_predictor[,1])
}
