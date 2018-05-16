## File Name: mdmb_exp_overflow.R
## File Version: 0.02

mdmb_exp_overflow <- function(x, M = 200)
{
    x <- ifelse( x > M, M , x)
    y <- exp(x)
    return(y)
}
