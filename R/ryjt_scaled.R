## File Name: ryjt_scaled.R
## File Version: 0.12

ryjt_scaled <- function(n, location=0, shape=1, lambda=1, df=Inf, probit=FALSE)
{
    if (df==Inf){
        df <- 1E5
    }
    x <- stats::rt( n, df=df, ncp=0 )
    x <- location + shape * x
    x <- yj_antitrafo(y=x, lambda=lambda, probit=probit )
    return(x)
}
