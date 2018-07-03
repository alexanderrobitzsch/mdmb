## File Name: frm_fb_mh_refresh_imputed_values.R
## File Version: 0.13

frm_fb_mh_refresh_imputed_values <- function( imputations_mcmc, acc_bounds, ind0 )
{
    impute_vars <- imputations_mcmc$impute_vars
    NV <- imputations_mcmc$NV
    mh_imputations_values <- imputations_mcmc$mh_imputations_values

    if (NV > 0){
        for (vv in 1:NV){
# cat("-------", vv, "------\n")
            # vv <- 2
            var_vv <- impute_vars[vv]
            ind0_vv <- ind0[[ var_vv ]]
            mh_imp_vv <- mh_imputations_values[[ var_vv ]]
            mh_adapt <- ( ! is.null( mh_imp_vv) ) &
                            ( ind0_vv$model %in% c("linreg") )
            if ( mh_adapt ){
                acc_pars <- list( mh_imp_vv[,1], mh_imp_vv[,2] )
                res0 <- frm_proposal_refresh_helper( acceptance_parameters=acc_pars,
                            proposal_sd=mh_imp_vv[,3], acceptance_bounds=acc_bounds)
                mh_imp_vv$sd_proposal <- res0$proposal_sd
                mh_imp_vv[,1:2] <- 0 * mh_imp_vv[,1:2]
                mh_imputations_values[[ var_vv ]] <- mh_imp_vv
            }
        }
    }
    #---- arrange output
    imputations_mcmc$mh_imputations_values <- mh_imputations_values
    return(imputations_mcmc)
}
