## File Name: rt_scaled.R
## File Version: 0.03

rt_scaled <- function(n , location=0, shape=1, df=Inf)
{
    x <- stats::rt( n , df=df , ncp=0 )
    x <- location + shape * x
    return(x)
}
