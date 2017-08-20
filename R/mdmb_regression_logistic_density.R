
mdmb_regression_logistic_density <- function( y, ypred , log, eps= 1E-50 )
{
	yp <- stats::plogis( ypred )					
	ll_i <- ifelse( y == 1 , yp , 1 - yp )
	if (log){
		ll_i <- log( ll_i + eps )
	}
	return(ll_i)
}

