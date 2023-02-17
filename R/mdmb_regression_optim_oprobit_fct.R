## File Name: mdmb_regression_optim_oprobit_fct.R
## File Version: 0.04

mdmb_regression_optim_oprobit_fct <- function(x, index_beta, index_thresh, Xdes,
                offset_values, y, eps, weights)
{
    beta <- x[ index_beta ]
    logthresh <- x[ index_thresh ]
    thresh <- logthresh_2_thresh(x=logthresh)
    ypred <- Xdes %*% beta + offset_values
    ll_i <- mdmb_regression_oprobit_density( y=y, ypred=ypred, thresh=thresh,
                        log=TRUE, eps=eps)
    ll <- - sum( weights * ll_i )
    #--- output
    return(ll)
}
