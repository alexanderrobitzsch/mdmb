## File Name: mdmb_discretize.R
## File Version: 0.02

mdmb_discretize <- function(x, alpha)
{
    K1 <- length(alpha)
    alpha[1] <- -Inf
    alpha[K1] <- Inf
    y <- cut(x, breaks=alpha, labels=FALSE)-1
    return(y)
}
