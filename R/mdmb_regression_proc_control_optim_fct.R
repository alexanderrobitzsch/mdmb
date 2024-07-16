## File Name: mdmb_regression_proc_control_optim_fct.R
## File Version: 0.121

mdmb_regression_proc_control_optim_fct <- function(control_optim_fct)
{
    if ( is.null(control_optim_fct) ){
        control_optim_fct <- list()
    }
    control_optim_fct[[ '__da__' ]] <- NA
    defaults <- list( use_rcpp_deriv_ypred=FALSE, use_rcpp_deriv_logthresh=2)
    for (nn in names(defaults) ){
        if ( ! ( nn %in% names(control_optim_fct) ) ){
            control_optim_fct[[ nn ]] <- defaults[[ nn ]]
        }
    }
    c1 <- ! control_optim_fct$use_rcpp_deriv_ypred
    control_optim_fct$use_rcpp_deriv_logthresh[ c1 ] <- 0
    return(control_optim_fct)
}
