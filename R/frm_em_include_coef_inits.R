## File Name: frm_em_include_coef_inits.R
## File Version: 0.05

frm_em_include_coef_inits <- function( ind , mm , model_results , iter)
{
	if (iter==0){
		mod <- model_results[[mm]]
		cm <- coef(mod)
		ind_mm <- ind[[mm]]
		coef_inits <- ind_mm$coef_inits
		if (length(coef_inits) > 0 ){
			ii <- which( ! is.na(coef_inits) )
			if ( length(ii) > 0 ){
				cm[ii] <- coef_inits[ii]
			}		
		}
		model_results[[mm]]$coefficients <- cm	
	}		
	return(model_results)
}
