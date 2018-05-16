## File Name: vcov.mdmb.R
## File Version: 0.12



vcov_mdmb_extract <- function( object , ...)
{
    return( object$vcov )        
}


#-- main functions
vcov.frm_em <- vcov_mdmb_extract
vcov.frm_fb <- vcov_mdmb_extract

#-- mdmb regression
vcov.bct_regression <- vcov_mdmb_extract
vcov.logistic_regression <- vcov_mdmb_extract
vcov.oprobit_regression <- vcov_mdmb_extract
vcov.yjt_regression <- vcov_mdmb_extract

#-- mdmb fit
vcov.fit_bct_scaled <- vcov_mdmb_extract
vcov.fit_oprobit <- vcov_mdmb_extract
vcov.fit_t_scaled <- vcov_mdmb_extract
vcov.fit_yjt_scaled <- vcov_mdmb_extract


