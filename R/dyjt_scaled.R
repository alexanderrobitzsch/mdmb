## File Name: dyjt_scaled.R
## File Version: 0.504

dyjt_scaled <- function( x, location=0, shape=1, lambda=1, df=Inf, log=FALSE, probit=FALSE )
{
    #*** recode lambda
    eps <- 1E-4
    for ( lambda0 in c(0,2) ){
        lambda <- yj_adjust_lambda( lambda=lambda, lambda0=lambda0, eps=eps )
    }
    #* Yeo-Johnson transformation
    # xt <- yj_trafo(y=x, lambda=lambda, probit=probit )
    res <- mdmb_rcpp_yj_trafo_derivative( y=x, lambda=lambda, probit=probit )
    xt <- res$yt
    yt <- res$dyt
    if ( df==Inf ){
        # dy <- stats::dnorm( x=xt, mean=location, sd=shape, log=log )
        dy <- mdmb_dnorm(x=xt, mu=location, sigma=shape, log=log)
    } else {
        dy <- dt_scaled( x=xt, location=location, shape=shape, df=df, log=log)
    }

    #--------- adjustment for derivative of Yeo-Johnson transformation
    # y <- x
    # yt <- mdmb_rcpp_yj_trafo_adjustment_derivative(y=y, lambda=lambda)

    #---- multiplicative adjustment
    dy <- dyjt_scaled_log_multiplication( dy=dy, yt=yt, use_log=log )
    return(dy)
}


# zz0 <- Sys.time(); for (bb in 1:B){ ;
#     yt <- mdmb_rcpp_yj_trafo_adjustment_derivative(y=y, lambda=lambda)
# }; cat("\n* trafo2 ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1
