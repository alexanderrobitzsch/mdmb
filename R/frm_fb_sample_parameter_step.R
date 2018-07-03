## File Name: frm_fb_sample_parameter_step.R
## File Version: 0.364


frm_fb_sample_parameter_step <- function( ind_mm, dat, weights,
            mod, coef, sigma )
{
    #--- compute likelihood
    mod$coefficients <- coef
    mod$sigma <- sigma
    args <- list(model=mod, y=dat[, ind_mm$dv_vars ], case=dat$case)
    args$design_matrix <- dat
    if ( ind_mm$use_gibbs_model ){
        args$no_weights <- ind_mm$no_weights
        args$weights <- weights
        if ( ind_mm$model=="mlreg" ){
            args$R_args <- mod$R_args
        }
        res <- do.call( what=ind_mm$R_sampling_fct, args=args )
    } else {
        dmod <- do.call( what=ind_mm$R_density_fct, args=args )
        # log-likelihood
        ll <- sum( log( dmod$like ) * weights )
        res <- list( like=dmod$like, ll=ll)
    }
    return(res)
}
