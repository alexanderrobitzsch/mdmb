## File Name: mdmb_weighted_sd.R
## File Version: 0.01

mdmb_weighted_sd <- function(x, w=rep(1,length(x)), unbiased=TRUE, na.rm=TRUE)
{
    v1 <- mdmb_weighted_var(x=x, w=w, unbiased=unbiased, na.rm=na.rm)
    v1 <- sqrt(v1)
    return(v1)
}
