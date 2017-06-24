
frm_linreg_density <- function(model, y, design_matrix=NULL, case=NULL,
		X = NULL , offset = NULL )
{
	if ( is.null(design_matrix) ){
		if (is.null(X) ){
			y_pred <- predict(model)
		} else {
			beta <- model$coefficients
			y_pred <- X %*% beta
			if (! is.null(offset) ){
				y_pred <- y_pred + offset		
			}
		}		
	} else {
		#  y_pred <- predict(model, newdata=design_matrix )
		form <- attr( model$model , "terms")
		Xdes <- stats::model.matrix( form , design_matrix )
		offset_values <- offset_values_extract(formula=form, data= design_matrix )
		beta <- coef(model)
		y_pred <- Xdes %*% beta + offset_values
	}
	w <- model$weights
	yr <- residuals(model)
	if ( is.null(w) ){
		w <- rep( 1 , length( yr ) )
	}
	y_sd <- TAM::weighted_sd( yr , w = w )  
	y_sd0 <- TAM::weighted_sd( y , w = w ) 
	if ( ! is.null(model$sigma) ){	
		y_sd <- model$sigma		
	}
	# R^2
	R2 <- mean( 1 - y_sd^2 / y_sd0^2 )
    d1 <- stats::dnorm( y , mean=y_pred , sd=y_sd )
	d2 <- frm_normalize_posterior( post = d1 , case = case )		
	res <- list( "like" = d1 , "post" = d2 , "sigma" = y_sd , R2 = R2)	
	return(res)
}
