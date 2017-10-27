## File Name: predict.yjt_regression.R
## File Version: 0.07

predict.yjt_regression <- function( object , newdata = NULL, trafo=TRUE, ...)
{	
	fitted.values <- mdmb_regression_predict_yjt_bct( object=object , 
						newdata = newdata , trafo=trafo, ...)
	return(fitted.values)
}
