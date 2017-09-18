## File Name: frm_fb_sample_parameter_step.R
## File Version: 0.14
## File Last Change: 2017-01-23 19:48:18


frm_fb_sample_parameter_step <- function( ind_mm , dat , weights ,
			mod , coef , sigma )
{
	#--- compute likelihood		
	# R_args <- list( formula = ind_mm$formula, data = dat, weights = weights,
	#				design_matrix = dat )
	mod$coefficients <- coef
	mod$sigma <- sigma		
	args <- list(model = mod , y = dat[ , ind_mm$dv_vars ],  case=dat$case )
	args$design_matrix <- dat	
	dmod <- do.call( what=ind_mm$R_density_fct , args = args )			
	# log-likelihood
	ll <- sum( log( dmod$like ) * weights )
	#--- output
	res <- list( "like" = dmod$like , "ll" = ll)
	return(res)
}	
