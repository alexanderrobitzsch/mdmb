

frm_fb_sample_imputed_values_eval_likelihood <- function(mm, model_results, ind0,
		dat_vv )
{
	ind_mm <- ind0[[mm]]	
	mod <- model_results[[mm]]
	args <- list(model = mod , y = dat_vv[ , ind_mm$dv_vars ],  
					case = dat_vv$case, design_matrix = dat_vv )
	dmod <- do.call( what=ind_mm$R_density_fct , args = args )
	like <- dmod$like
	return(like)
}
