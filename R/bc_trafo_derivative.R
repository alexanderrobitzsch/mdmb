## File Name: bc_trafo_derivative.R
## File Version: 0.05

bc_trafo_derivative <- function(y, lambda, check_zero)
{
    eps <- 1E-3
    if ( abs(lambda) > eps ){
        # yt <- ( y^lambda - 1 ) / lambda
        yt <- log(y)
        y1 <- exp(lambda*yt)
        yt <- ( y1 - 1 ) / lambda
        # derivative
        lam1 <- lambda - 1
        dyt <- y1 / y
        # yt=y^(lambda - 1 )
    } else {
        yt <- log(y)
        dyt <- 1 / y
    }
    if (check_zero){
        dyt <- ifelse( y<=0, 0, dyt )
    }
    #--- output
    res <- list(yt=yt, dyt=dyt)
    return(res)
}

