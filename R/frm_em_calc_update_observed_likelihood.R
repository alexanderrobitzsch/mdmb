## File Name: frm_em_calc_update_observed_likelihood.R
## File Version: 0.03

frm_em_calc_update_observed_likelihood <- function(like_obs, post_miss,
	dmod , mm , ind_resp , ind_miss )
{
	ind_resp_mm <- ind_resp[[mm]]		
	if ( length(ind_resp_mm) > 0 ){
		like_obs[ind_resp_mm] <- like_obs[ind_resp_mm] * dmod$like[ind_resp_mm]
	}
	ind_miss_mm <- ind_miss[[mm]]
	if ( length(ind_miss_mm) > 0 ){
		post_miss[ind_miss_mm] <- post_miss[ind_miss_mm] * dmod$post[ind_miss_mm]
	}	
	res <- list( like_obs = like_obs , post_miss = post_miss)
	return(res)
}
