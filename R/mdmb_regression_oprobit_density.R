## File Name: mdmb_regression_oprobit_density.R
## File Version: 0.295

mdmb_regression_oprobit_density <- function( y, ypred, thresh, log, eps=1E-50,
        max_val=99, use_rcpp=TRUE )
{
    thresh <- c(0, thresh )
    thresh_low <- c( -max_val, thresh)
    thresh_upp <- c( thresh, max_val )
    if ( ! use_rcpp ){
        ll_i <- stats::pnorm( thresh_upp[y+1] - ypred ) -
                        stats::pnorm( thresh_low[y+1] - ypred )
        if (log){
            ll_i <- log( ll_i + eps )
        }
    } else {
        ll_i <- mdmb_rcpp_oprobit_density( ypred=ypred, thresh_low=thresh_low,
                        thresh_upp=thresh_upp, y=y, use_log=log, eps=eps )
        ll_i <- matrix(ll_i, ncol=1)
    }
    return(ll_i)
}

