## File Name: mdmb_regression_oprobit_density.R
## File Version: 0.09

mdmb_regression_oprobit_density <- function( y, ypred, thresh, log, eps= 1E-50, 
		max_val=99 )
{
	thresh <- c(0, thresh )	
	thresh_low <- c( -max_val, thresh)
	thresh_upp <- c( thresh, max_val )	
	ll_i <- stats::pnorm( thresh_upp[y+1] - ypred ) - stats::pnorm( thresh_low[y+1] - ypred )
	if (log){
		ll_i <- log( ll_i + eps )
	}
	return(ll_i)
}

