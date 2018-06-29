## File Name: ryjt_scaled.R
## File Version: 0.09

ryjt_scaled <- function(n, location=0, shape=1, lambda=1, df=Inf)
{
    if (df==Inf){
        df <- 1E5
    }
    x <- stats::rt( n, df=df, ncp=0 )
    x <- location + shape * x
    x <- yj_antitrafo(y=x, lambda=lambda )
    return(x)
}
