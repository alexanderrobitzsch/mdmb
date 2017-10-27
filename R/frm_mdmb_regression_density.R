## File Name: frm_mdmb_regression_density.R
## File Version: 0.09

frm_mdmb_regression_density <- function(model, y, design_matrix=NULL, case=NULL,
		X = NULL , offset = NULL )
{
	pars <- coef(model)
	class_model <- class(model)
	np <- length(pars)
	beta <- pars[ seq(1,np-2) ]
	if ( is.null(design_matrix) ){
		if (is.null(X) ){
			y_pred <- predict(model)
		} else {
			y_pred <- X %*% beta
			if (! is.null(offset) ){
				y_pred <- y_pred + offset		
			}
		}		
	} else {
		#  y_pred <- predict(model, newdata=design_matrix )
		# form <- attr( model$model , "terms")
		form <- model$formula
		Xdes <- stats::model.matrix( form , design_matrix )
		offset_values <- offset_values_extract(formula=form, data= design_matrix )
		y_pred <- Xdes %*% beta + offset_values
	}
	w <- model$weights	
	if ( is.null(w) ){
		w <- rep( 1 , length( y ) )
	}		
	y_sd <- TAM::weighted_sd( y_pred , w = w )
	#*** y values on the transformed metric
	if (class_model	== "bct_regression"){
		yt <- bc_trafo( y , lambda = pars[np] )
	}
	if (class_model	== "yjt_regression"){
		yt <- yj_trafo( y , lambda = pars[np] )
	}	
	y_sd0 <- TAM::weighted_sd( yt , w = w ) 
	if ( ! is.null(model$sigma) ){	
		y_sd <- model$sigma		
	}
	# R^2
	R2 <- mean( y_sd^2 / y_sd0^2 )
	#****** evaluated density
	if (class_model	== "bct_regression"){
		d1 <- dbct_scaled( y , location=y_pred, shape = pars[np-1] , 
					lambda = pars[np] , df = model$df )
	}	    
	if (class_model	== "yjt_regression"){
		d1 <- dyjt_scaled( y , location=y_pred, shape = pars[np-1] , 
					lambda = pars[np] , df = model$df )
	}	
	d2 <- frm_normalize_posterior( post = d1 , case = case )		
	res <- list( "like" = d1 , "post" = d2 , "sigma" = y_sd , R2 = R2)	
	return(res)
}
