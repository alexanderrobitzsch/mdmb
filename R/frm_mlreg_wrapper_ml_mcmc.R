## File Name: frm_mlreg_wrapper_ml_mcmc.R
## File Version: 0.282

frm_mlreg_wrapper_ml_mcmc <- function( data, formula, weights=NULL,
    sample_missings=TRUE, ...)
{
    vars <- all.vars(formula)
    if (sample_missings){
        data <- mdmb_sample_missings(data=data, vars=vars)
    }
    args <- list( formula=formula, data=data, ...)
    if ( ! ( "inits_lme4" %in% names(args) ) ){
        args$inits_lme4 <- FALSE
    }
    res <- do.call( what=miceadds::ml_mcmc, args=args)
    return(res)
}
