## File Name: frm_oprobit_density.R
## File Version: 0.13

frm_oprobit_density <- function(model, y, design_matrix=NULL, case=NULL)
{
	if ( is.null(design_matrix) ){
		y_pred <- predict(model)
	} else {
		y_pred <- predict(model, newdata=design_matrix)
	}
	logthresh <- model$coefficients[ model$index_thresh ]
	thresh <- logthresh_2_thresh(x=logthresh)
	d1 <- mdmb_regression_oprobit_density( y=y, ypred=y_pred, thresh=thresh, log=FALSE)	
	d2 <- frm_normalize_posterior( post = d1 , case = case )		
	res <- list( "like" = d1 , "post" = d2 )	
	return(res)
}
