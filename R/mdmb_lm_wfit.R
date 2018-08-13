## File Name: mdmb_lm_wfit.R
## File Version: 0.07

mdmb_lm_wfit <- function( x, y, w, offset=NULL )
{
    res0 <- mdmb_rcpp_lm_wfit(x=x, y=y, w=w)
    coef <- as.vector(res0$coef)
    residuals <- res0$res
    fitted.values <- res0$fitted
    if ( ! is.null(offset)){
        fitted.values <- fitted.values + offset
    }
    #-- output
    res <- list( coefficients=coef, residuals=residuals, fitted.values=fitted.values, weights=w)
    return(res)
}
