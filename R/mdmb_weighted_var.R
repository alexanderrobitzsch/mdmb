## File Name: mdmb_weighted_var.R
## File Version: 0.06

mdmb_weighted_var <- function(x, w=rep(1,length(x)), unbiased=TRUE, na.rm=TRUE)
{
    m <- stats::weighted.mean(x=x, w=w, na.rm=na.rm)
    e <- x - m
    v1 <- sum(w*e^2, na.rm=na.rm)
    v2 <- sum(w, na.rm=na.rm)
    if (unbiased){
        N <- sum( ! is.na(e) )
        v2 <- (N-1)/N*v2        
    }
    v1 <- v1 / v2
    return(v1)
}
