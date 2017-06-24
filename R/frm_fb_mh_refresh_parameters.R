
frm_fb_mh_refresh_parameters <- function( ind0 , acc_bounds )
{
	NM <- attr( ind0 , "NM")
	NM1 <- NM + 1
	for (mm in 1:NM1){
		# mm <- 1		
		ind0 <- frm_fb_refresh_parameters_step( mm=mm, ind0=ind0, name_MH="coef_MH", 
						name_sd_proposal="coef_sd_proposal", acc_bounds= acc_bounds  )	
		if ( ind0[[mm]]$sample_sigma ){					
			ind0 <- frm_fb_refresh_parameters_step( mm=mm, ind0=ind0, name_MH="sigma_MH", 
							name_sd_proposal="sigma_sd_proposal", acc_bounds= acc_bounds )	
		}
	}		
	return(ind0)
}
