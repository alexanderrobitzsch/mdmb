## File Name: frm_fb_initial_parameters_se_sd_proposal.R
## File Version: 0.03


frm_fb_initial_parameters_se_sd_proposal <- function(mod)
{
    if ( "hessian" %in% names(mod) ){
        hess_diag <- abs( diag( mod$hessian ) )
        M <- 10000 # maximum value
        hess_diag[ hess_diag > M ] <- M
        se_mod <- sqrt( 1 / hess_diag )
    } else {
        se_mod <- mdmb_vcov2se(vcov=vcov(mod))
    }
    return(se_mod)
}
