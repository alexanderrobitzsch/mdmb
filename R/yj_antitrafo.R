## File Name: yj_antitrafo.R
## File Version: 0.07

yj_antitrafo <- function(y, lambda)
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
    #--- output
    return(yt)
}
