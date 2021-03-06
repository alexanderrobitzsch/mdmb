## File Name: bc_trafo.R
## File Version: 1.05


bc_trafo <- function( y, lambda )
{
    eps <- 1E-3
    # lambda <- yj_adjust_lambda( lambda=lambda, lambda0=0, eps=eps )
    if ( abs(lambda) > eps ){
        # yt <- ( y^lambda - 1 ) / lambda
        yt <- log(y)
        yt <- ( exp(lambda*yt) - 1 ) / lambda
    } else {
        yt <- log(y)
    }
    return(yt)
}
