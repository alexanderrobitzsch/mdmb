## File Name: frm_mlreg_wrapper_ml_mcmc.R
## File Version: 0.17

frm_mlreg_wrapper_ml_mcmc <- function( data, formula, weights=NULL,
    sample_missings=TRUE, ...)
{
    vars <- all.vars(formula)
    if (sample_missings){
        data <- mdmb_sample_missings(data=data, vars=vars)
    }
    res <- miceadds::ml_mcmc( formula=formula, data=data, ...)
    return(res)
}
