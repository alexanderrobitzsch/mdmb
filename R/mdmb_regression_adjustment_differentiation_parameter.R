## File Name: mdmb_regression_adjustment_differentiation_parameter.R
## File Version: 0.02

mdmb_regression_adjustment_differentiation_parameter <- function(h, par )
{
    ## adjustment of differentiation parameter in CDM::numerical_Hessian
    abs_par <- abs(par)
    hvec <- h * ifelse(abs_par > 1, abs_par, 1)
    return(hvec)
}
