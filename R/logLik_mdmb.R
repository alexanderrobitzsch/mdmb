## File Name: logLik_mdmb.R
## File Version: 0.12


#--- logLik main functions and mdmb regression

logLik_mdmb_general_extract <- function (object, ...) {
    return( logLik_extract_ic(object=object) )
}

logLik.bct_regression <- logLik_mdmb_general_extract
logLik.frm_em <- logLik_mdmb_general_extract
logLik.oprobit_regression <- logLik_mdmb_general_extract
logLik.logistic_regression <- logLik_mdmb_general_extract
logLik.yjt_regression <- logLik_mdmb_general_extract


#--- logLik fit mdmb distribution
logLik_mdmb_distribution_extract <- function (object, ...) {
    return( fit_mdmb_distribution_logLik_extract( object=object ) )
}

logLik.fit_bct_scaled <- logLik_mdmb_distribution_extract
logLik.fit_oprobit <- logLik_mdmb_distribution_extract
logLik.fit_t_scaled <- logLik_mdmb_distribution_extract
logLik.fit_yjt_scaled <- logLik_mdmb_distribution_extract
