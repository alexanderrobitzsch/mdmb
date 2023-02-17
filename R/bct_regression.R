## File Name: bct_regression.R
## File Version: 0.172


bct_regression <- function( formula, data, weights=NULL,
    beta_init=NULL, beta_prior=NULL, df=Inf, lambda_fixed=NULL, est_df=FALSE,
    use_grad=2, h=1E-5, optimizer="optim", maxiter=300, control=NULL )
{
    CALL <- match.call()
    type <- "bct"
    #--- wrapping general regression function
    res <- mdmb_regression( formula=formula, data=data, type=type,
                weights=weights, beta_init=beta_init, beta_prior=beta_prior,
                df=df, lambda_fixed=lambda_fixed, est_df=est_df,
                use_grad=use_grad, h=h, optimizer=optimizer, maxiter=maxiter,
                control=control )
    res$CALL <- CALL
    #--- additional informations about model type
    class(res) <- 'bct_regression'
    return(res)
}
