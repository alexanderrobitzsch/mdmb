## File Name: logistic_regression.R
## File Version: 0.591


logistic_regression <- function( formula, data, weights=NULL,
    beta_init=NULL, beta_prior=NULL, use_grad=2, h=1E-5,
    optimizer="optim", maxiter=300, control=NULL )
{
    CALL <- match.call()
    type <- 'logistic'
    #--- wrapper to general regression function
    res <- mdmb_regression( formula=formula, data=data, type=type,
                weights=weights, beta_init=beta_init,
                use_grad=use_grad, h=h, beta_prior=beta_prior,
                optimizer=optimizer, maxiter=maxiter, control=control )
    res$CALL <- CALL
    #--- additional informations about model type
    class(res) <- 'logistic_regression'
    return(res)
}
