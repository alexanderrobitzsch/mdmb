## File Name: yj_trafo.R
## File Version: 0.462

yj_trafo <- function(y, lambda, use_rcpp=TRUE, probit=FALSE)
{
    #*** recode lambda
    eps <- 1E-5
    for ( lambda0 in c(0,2) ){
        lambda <- yj_adjust_lambda( lambda=lambda, lambda0=lambda0, eps=eps )
    }
    if (probit){
        y <- ( y + eps ) / ( 1 + 2 * eps )
        y <- stats::qnorm(y)
    }
    if (use_rcpp){
        yt <- mdmb_rcpp_yj_trafo( y=y, lambda=lambda )
    } else {
        yt <- y
        #*** y >=0
        yt <- ifelse( y >=0, ( ( yt + 1 )^lambda -1 ) / lambda, yt )
        #*** y <=0
        yt <- ifelse( y < 0, - ( (  - yt + 1 )^(2-lambda) -1 ) / (2-lambda), yt )
    }
    #--- output
    return(yt)
}

