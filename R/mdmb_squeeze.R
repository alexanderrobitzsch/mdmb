## File Name: mdmb_squeeze.R
## File Version: 0.01

mdmb_squeeze <- function(x, lower, upper)
{
    x <- ifelse( x<lower, lower, x)
    x <- ifelse( x>upper, upper, x)
    return(x)
}
