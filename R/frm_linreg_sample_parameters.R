## File Name: frm_linreg_sample_parameters.R
## File Version: 0.23


frm_linreg_sample_parameters <- function(model, design_matrix, y, weights, no_weights, ... )
{
    form <- attr( model$model, "terms")
    Xdes <- stats::model.matrix( object=form, design_matrix )
    offset_values <- offset_values_extract(formula=form, data=design_matrix )
    y <- y - offset_values
    X1 <- Xdes
    y1 <- y
    if ( ! no_weights ){
        wgt <- sqrt(weights)
        X1 <- wgt*X1
        y1 <- wgt*y1
    }
    XtX <- crossprod(X1)
    Xty <- crossprod(X1,y1)
    np <- ncol(X1)
    sample_beta <- ( np > 0 )
    if (sample_beta){
        xtx_inv <- mdmb_ginv(XtX)
        beta0 <- xtx_inv %*% Xty
        e <- y1 - X1 %*% beta0
    } else {
        e <- y1
    }
    N <- length(y1)
    sigma2 <- sum( weights * e^2 ) / sum(weights)
    #--- sample regression parameters
    if (sample_beta){
        requireNamespace("MASS")
        beta_vcov <- sigma2 * xtx_inv
        coef <- MASS::mvrnorm(n=1, mu=beta0[,1], Sigma=beta_vcov)
    } else {
        coef <- NULL
    }
    df <- N - np
    #      df * s2 / sig2 ~ chi2
    #=>   sig2 ~ df * s2 / chi2
    sigma2 <- sigma2 * df / stats::rchisq(1, df=df )
    sigma <- sqrt(sigma2)
    #-- output
    res <- list( coef=coef, sigma=sigma)
    return(res)
}

