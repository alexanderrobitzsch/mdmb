## File Name: yjt_regression.R
## File Version: 0.33


yjt_regression <- function( formula, data, weights=NULL,
    beta_init=NULL, beta_prior=NULL, df=Inf, lambda_fixed=NULL, probit=FALSE,
    est_df=FALSE, df_min=.5, df_max=100, use_grad=2, h=1E-5, optimizer="optim",
    maxiter=300, control=NULL )
{
    CALL <- match.call()
    type <- "yjt"
    #--- wrapping general regression function
    res <- mdmb_regression( formula=formula, data=data, type=type, weights=weights,
                beta_init=beta_init, beta_prior=beta_prior, df=df, lambda_fixed=lambda_fixed,
                probit=probit, est_df=est_df, df_min=df_min, df_max=df_max,
                use_grad=use_grad, h=h, maxiter=maxiter, control=control, optimizer=optimizer )
    res$CALL <- CALL
    #--- additional informations about model type
    class(res) <- 'yjt_regression'
    return(res)
}
