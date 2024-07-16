## File Name: mdmb_regression_optim_oprobit_grad.R
## File Version: 0.065


mdmb_regression_optim_oprobit_grad <- function(x, index_beta, index_thresh, Xdes,
            offset_values, y, eps, h, weights, NT, use_rcpp_deriv_ypred,
            use_rcpp_deriv_logthresh )
{
    beta <- x[ index_beta ]
    logthresh <- x[ index_thresh ]
    thresh <- logthresh_2_thresh(x=logthresh)
    xgrad <- rep(0, length(x) )
    ypred <- Xdes %*% beta + offset_values
    if ( ! use_rcpp_deriv_ypred ){
        ll0 <- mdmb_regression_oprobit_density( y=y, ypred=ypred, thresh=thresh,
                            log=TRUE, eps=eps )
        ll1 <- mdmb_regression_oprobit_density( y=y, ypred=ypred+h, thresh=thresh,
                            log=TRUE, eps=eps )
        der1 <- - mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=h)
    } else {
        res0 <- mdmb_oprobit_extend_thresh(thresh=thresh)
        res <- mdmb_rcpp_oprobit_derivative_ypred( ypred=ypred,
                            thresh_low=res0$thresh_low, thresh_upp=res0$thresh_upp, y=y )
        ll0 <- res$ll0
        der1 <- res$der1
        probs <- res$probs
        dens_upp <- res$dens_upp
        dens_low <- res$dens_low
    }
    if (! is.null(index_beta) ){
        wder1 <- weights * der1[,1]
        xgrad[index_beta] <- colSums( wder1 * Xdes )
    }
    #-- derivatives for thresholds
    if (NT>0){
        thresh <- logthresh_2_thresh(x=logthresh)
        for (ii in 1L:NT){
            logthresh0 <- logthresh
            logthresh0[ii] <- logthresh[ii] + h
            thresh0 <- logthresh_2_thresh(x=logthresh0)
            if ( use_rcpp_deriv_logthresh==0 ){
                ll1 <- mdmb_regression_oprobit_density( y=y, ypred=ypred,
                                    thresh=thresh0, log=TRUE, eps=eps )
                der1 <- - mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=h)
            }
            if ( use_rcpp_deriv_logthresh==1 ){
                res0 <- mdmb_oprobit_extend_thresh(thresh=thresh0)
                der1 <- mdmb_rcpp_oprobit_derivative_logthresh( ypred=ypred,
                                    thresh_low=res0$thresh_low,
                                    thresh_upp=res0$thresh_upp, y=y,
                                    ll0=ll0, eps=eps, h=h, y_value=ii)
            }
            if ( use_rcpp_deriv_logthresh==2 ){
                #*** chain rule derivatives
                thresh_der <- ( thresh0 - thresh ) / h
                l1 <- mdmb_oprobit_extend_thresh( thresh=thresh_der, max_val=0 )
                thresh_der <- c( l1$thresh_low, 0)
                der1 <- mdmb_rcpp_oprobit_derivative_logthresh_chain_rule(
                                thresh_der=thresh_der, probs=probs, dens_upp=dens_upp,
                                dens_low=dens_low, y=y, eps=eps, y_value=ii )
            }
            xgrad[ index_thresh[ii] ] <- sum( weights * der1[,1] )
        }
    }
    #--- output
    return(xgrad)
}
