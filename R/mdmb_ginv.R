## File Name: mdmb_ginv.R
## File Version: 0.08

mdmb_ginv <- function(x, eps=1e-10)
{
    x0 <- x
    x0_diag <- sqrt(diag(x0))
    M <- outer(x0_diag, x0_diag)
    x <- x0 / M
    sx <- svd(x=x)
    ind <- which( sx$d > eps )
    U <- sx$u[,ind]
    # V <- sx$v[,ind]
    V <- t(U)
    D <- diag(1/sx$d[ind])
    U1 <- U %*% D %*% t(V)
    U1 <- U1 / M
    return(U1)
}
