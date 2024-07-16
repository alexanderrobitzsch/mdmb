## File Name: oprobit_regression.R
## File Version: 0.161


oprobit_regression <- function( formula, data, weights=NULL,
    beta_init=NULL, use_grad=2, h=1E-5, optimizer="optim",
    maxiter=300, control=NULL, control_optim_fct=NULL )
{
    CALL <- match.call()
    type <- 'oprobit'
    #--- wrapping general regression function
    res <- mdmb_regression( formula=formula, data=data, type=type,
                weights=weights, beta_init=beta_init, use_grad=use_grad, h=h,
                optimizer=optimizer, maxiter=maxiter, control=control,
                control_optim_fct=control_optim_fct )
    res$CALL <- CALL
    #--- additional informations about model type
    class(res) <- 'oprobit_regression'
    return(res)
}
