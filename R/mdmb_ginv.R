## File Name: mdmb_ginv.R
## File Version: 0.131

mdmb_ginv <- function(x, ...)
{
    requireNamespace('MASS')
    y <- MASS::ginv(X=x, ...)
    return(y)
}
