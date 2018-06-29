## File Name: frm_em_ic.R
## File Version: 0.10

frm_em_ic <- function(ll_new, N, ind0, model_results)
{
    res <- list( deviance=-2*ll_new, N=N, loglike=ll_new )
    NM <- attr( ind0, "NM")
    np_models <- rep(0,NM+1)
    for (mm in seq(1,NM+1)){
        np_models[mm] <- length( model_results[[mm]]$coefficients)
        if ( ind0[[mm]]$model=="linreg"){
            np_models[mm] <- np_models[mm] + 1 - ind0[[mm]]$is_sigma_fixed
        }
    }
    res$np <- sum( np_models )
    res$np_models <- np_models
    return(res)
}

