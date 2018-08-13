## File Name: mdmb_regression_proc_control_optim_fct.R
## File Version: 0.09

mdmb_regression_proc_control_optim_fct <- function(control_optim_fct)
{
    if ( is.null(control_optim_fct) ){
        control_optim_fct <- list()
    }
    control_optim_fct[[ "__da__" ]] <- NA
    defaults <- list( use_rcpp_deriv_ypred=FALSE, use_rcpp_deriv_logthresh=2)
    nms <- names(defaults)
    for (nn in names(defaults) ){
        if ( ! ( nn %in% names(control_optim_fct) ) ){
            control_optim_fct[[ nn ]] <- defaults[[ nn ]]
        }    
    }
    control_optim_fct$use_rcpp_deriv_logthresh[ ! control_optim_fct$use_rcpp_deriv_ypred ] <- 0
    return(control_optim_fct)
}
