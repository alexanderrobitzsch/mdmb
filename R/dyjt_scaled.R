## File Name: dyjt_scaled.R
## File Version: 0.515

dyjt_scaled <- function( x, location=0, shape=1, lambda=1, df=Inf, log=FALSE,
                    probit=FALSE )
{
    #*** recode lambda
    eps <- 1E-4
    for ( lambda0 in c(0,2) ){
        lambda <- yj_adjust_lambda( lambda=lambda, lambda0=lambda0, eps=eps )
    }
    #* Yeo-Johnson transformation
    res <- mdmb_rcpp_yj_trafo_derivative( y=x, lambda=lambda, probit=probit )
    xt <- res$yt
    yt <- res$dyt
    if ( df==Inf ){
        dy <- mdmb_dnorm(x=xt, mu=location, sigma=shape, log=log)
    } else {
        dy <- dt_scaled( x=xt, location=location, shape=shape, df=df, log=log)
    }

    #---- multiplicative adjustment
    dy <- dyjt_scaled_log_multiplication( dy=dy, yt=yt, use_log=log,
                check_zero=TRUE)
    return(dy)
}

