## File Name: dbct_scaled.R
## File Version: 0.475

dbct_scaled <- function( x, location=0, shape=1, lambda=1, df=Inf, log=FALSE,
                    check_zero=TRUE )
{
    #*** recode lambda
    eps <- 1E-3
    #--- adjustment for derivative of Box-Cox transformation
    res <- bc_trafo_derivative(y=x, lambda=lambda, check_zero=check_zero )
    xt <- res$yt
    yt <- res$dyt

    if ( df==Inf ){
        dy <- mdmb_dnorm(x=xt, mu=location, sigma=shape, log=log)
    } else {
        dy <- dt_scaled( x=xt, location=location, shape=shape, df=df, log=log)
    }

    #---- multiplicative adjustment
    dy <- dyjt_scaled_log_multiplication( dy=dy, yt=yt, use_log=log,
                    check_zero=check_zero )
    #--- output
    return(dy)
}


