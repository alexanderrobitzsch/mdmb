## File Name: yj_antitrafo.R
## File Version: 0.09

yj_antitrafo <- function(y, lambda, probit=FALSE)
{
    #*** recode lambda
    eps <- 1E-5
    for ( lambda0 in c(0,2) ){
        lambda <- yj_adjust_lambda( lambda=lambda, lambda0=lambda0, eps=eps )
    }
    yt <- y
    #*** y >=0
    yt <- ifelse( y >=0, ( lambda * y + 1 )^(1/lambda ) - 1, yt )
    #*** y <=0
    yt <- ifelse( y < 0, - ( - (2-lambda )*y + 1 )^( 1 / (2-lambda ) ) + 1, yt )
    if (probit){
        yt <- stats::pnorm(yt)
    }
    #--- output
    return(yt)
}
