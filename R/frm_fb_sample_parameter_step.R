## File Name: frm_fb_sample_parameter_step.R
## File Version: 0.383


frm_fb_sample_parameter_step <- function( ind_mm, dat, weights,
            mod, coef, sigma )
{
    #--- compute likelihood
    mod$coefficients <- mod$coef <- coef
    use_variable_level <- ind_mm$use_variable_level
    mod$sigma <- sigma
    #* adapt for sampling parameters for models with variables at higher levels
    if (use_variable_level){
        id_variable_level_unique <- ind_mm$id_variable_level_unique
        dat <- dat[ id_variable_level_unique, ]
        weights <- weights[ id_variable_level_unique ]
    }
    #* create list of arguments
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
        eps <- 1e-100
        ll <- sum( log( dmod$like + eps) * weights )
        res <- list( like=dmod$like, ll=ll)
    }
    return(res)
}
