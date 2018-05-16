## File Name: coef.mdmb.R
## File Version: 0.13

coef_mdmb_extract <- function( object , ...)
{
    return(object$coef)
}

#-- mdmb main functions
coef.frm_fb <- coef_mdmb_extract
coef.frm_em <- coef_mdmb_extract

#-- fit mdmb distribution
coef.fit_bct_scaled <- coef_mdmb_extract
coef.fit_oprobit <- coef_mdmb_extract
coef.fit_t_scaled <- coef_mdmb_extract
coef.fit_yjt_scaled <- coef_mdmb_extract

#-- mdmb_regression
coef.bct_regression <- coef_mdmb_extract
coef.logistic_regression <- coef_mdmb_extract
coef.oprobit_regression <- coef_mdmb_extract
coef.yjt_regression <- coef_mdmb_extract
