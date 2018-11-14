## File Name: dt_scaled.R
## File Version: 0.23

dt_scaled <- function( x, location=0, shape=1, df=Inf, log=FALSE )
{
    use_log <- log
    if (df==Inf){
        dy <- mdmb_dnorm(x=x, mu=location, sigma=shape, log=log)
    } else {
        x1 <- ( x - location ) / shape
        dy <- mdmb_rcpp_dt( x=x1, df=df, use_log=log )
        if ( use_log ){
            dy <- dy - log(shape)
        } else {
            dy <- dy / shape
        }
    }
    return(dy)
}
