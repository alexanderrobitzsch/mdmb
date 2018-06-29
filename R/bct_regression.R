## File Name: bct_regression.R
## File Version: 0.08


bct_regression <- function( formula, data, weights=NULL,
    beta_init=NULL, beta_prior=NULL, df=Inf, lambda_fixed=NULL,
    use_grad=2, h=1E-5, control=NULL )
{
    CALL <- match.call()
    type <- "bct"
    #--- wrapping general regression function
    res <- mdmb_regression( formula=formula, data=data, type=type,
                weights=weights,    beta_init=beta_init,
                beta_prior=beta_prior, df=df, lambda_fixed=lambda_fixed,
                use_grad=use_grad, h=h, control=control )
    res$CALL <- CALL
    #--- additional informations about model type
    class(res) <- "bct_regression"
    return(res)
}
