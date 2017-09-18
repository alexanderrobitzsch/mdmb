## File Name: frm_prepare_models_sigma_fixed.R
## File Version: 0.06
## File Last Change: 2017-01-23 19:35:13

frm_prepare_models_sigma_fixed <- function( ind0 , NM, dat0, dat)
{
	NM1 <- NM + 1
	N <- nrow(dat0)	
	for (mm in 1:NM1){
		# mm <- 1
		ind_mm <- ind0[[mm]]		
		sigma_fixed <- ind_mm$sigma_fixed
		NS <- length(sigma_fixed)
		ind_mm$is_sigma_fixed <- FALSE
		if ( NS == 1){		
			sigma_fixed <- rep( sigma_fixed , N )		
		}
		if ( NS > 0){
			sigma_fixed <- sigma_fixed[ dat$case ]
			ind_mm$sigma_fixed <- sigma_fixed
			ind_mm$is_sigma_fixed <- TRUE
		}
		ind0[[mm]] <- ind_mm
	}	
	return(ind0)		
}
