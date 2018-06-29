## File Name: frm_fb_sample_parameters_mh_acceptance_step.R
## File Version: 0.07


frm_fb_sample_parameters_mh_acceptance_step <- function(ll0, ll1)
{
    logprob_cc <- ll1 - ll0
    prob_cc <- mdmb_exp_overflow(logprob_cc)
    rn0 <- stats::runif(1)
    accept <- 1 * ( rn0 < prob_cc )
    if ( ! is.finite(accept) ){
        accept <- FALSE
    }
    return(accept)
}
