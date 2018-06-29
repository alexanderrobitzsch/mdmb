## File Name: oprobit_regression.R
## File Version: 0.05


oprobit_regression <- function( formula, data, weights=NULL,
    beta_init=NULL, use_grad=2, h=1E-5, control=NULL )
{
    CALL <- match.call()
    type <- "oprobit"
    #--- wrapping general regression function
    res <- mdmb_regression( formula=formula, data=data, type=type,
                weights=weights,    beta_init=beta_init,
                use_grad=use_grad, h=h, control=control )
    res$CALL <- CALL
    #--- additional informations about model type
    class(res) <- "oprobit_regression"
    return(res)
}
