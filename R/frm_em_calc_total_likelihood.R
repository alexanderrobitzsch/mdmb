
frm_em_calc_total_likelihood <- function(dat, weights0,
	like_obs, post_miss)
{
	eps <- 1E-50
	# calculate total likelihood per case
	like_case <- like_obs * post_miss	
	like_case <- rowsum( like_case , dat$case )[,1]
	ll <- sum( log( like_case + eps ) * weights0 )
	return(ll)
}
