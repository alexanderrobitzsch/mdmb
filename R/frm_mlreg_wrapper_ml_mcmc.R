## File Name: frm_mlreg_wrapper_ml_mcmc.R
## File Version: 0.07

frm_mlreg_wrapper_ml_mcmc <- function( data, formula, weights=NULL,
    sample_missings=TRUE, ...)
{
    vars <- all.vars(formula)
    if (sample_missings){
        data <- mdmb_sample_missings(data=data, vars=vars)
    }
    # res <- miceadds::ml_mcmc( data=data, formula=formula, ...)
    res <- ml_mcmc( data=data, formula=formula, ...)
    return(res)
}
