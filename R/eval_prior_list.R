
eval_prior_list <- function( par , par_prior )
{
	NP <- length(par_prior)
	prior_val <- rep(NA,NP)
	for (pp in 1:NP){
		# pp <- 1
		pp_args <- as.list( par_prior[[pp]][[2]] )
		pp_args[["x"]] <- par[pp]
		prior_val[pp] <- do.call( what= par_prior[[pp]][[1]] , args = pp_args )
	}
	return(prior_val)
}
