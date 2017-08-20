
frm_em_calc_likelihood_estimate_model <- function( ind_mm, dat, weights )
{ 
	R_args <- list( formula = ind_mm$formula, data = dat, weights = weights)
	R_args <- frm_append_list(list1=R_args, list2=ind_mm$R_args)		
	R_fct <- ind_mm$R_fct
	#--- switch for model = "linreg" from stats::lm to stats::lm.wfit
	switch_linreg <- ind_mm$model == "linreg"	
	# switch_linreg <- FALSE		
	if ( switch_linreg ){
		R_args <- ind_mm$R_des
		R_args[["w"]] <- weights
		R_fct <- "lm.wfit"				
	}		
	mod <- do.call( what=R_fct , args = R_args )	
	if ( switch_linreg ){
		mod$sigma <- TAM::weighted_sd( mod$residuals , weights )
	}
	return(mod)
}
