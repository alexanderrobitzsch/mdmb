## File Name: bc_antitrafo.R
## File Version: 0.05

bc_antitrafo <- function(y, lambda )
{
    eps <- 1E-5
    lambda <- yj_adjust_lambda( lambda=lambda, lambda0=0, eps=eps )
    # inverse Box-Cox transformation
    yt <- ( lambda * y + 1 )^( 1 / lambda )
    return(yt)
}
