## File Name: mdmb_vcov2se.R
## File Version: 0.01

mdmb_vcov2se <- function(vcov)
{
    return( sqrt( diag( vcov ) ) )
}
