## File Name: eval_prior_list.R
## File Version: 0.08
## File Last Change: 2017-08-20 14:06:27

eval_prior_list <- function( par , par_prior, log = FALSE, eps=1E-50 )
{
	NP <- min( length(par), length(par_prior) )
	prior_val <- rep(NA,NP)
	for (pp in 1:NP){
		# pp <- 1
		pp_args <- as.list( par_prior[[pp]][[2]] )
		pp_args[["x"]] <- par[pp]
		prior_val[pp] <- do.call( what= par_prior[[pp]][[1]] , args = pp_args )
		if (log){
			prior_val[[pp]] <- log( prior_val[[pp]] + eps)
		}						
	}
	return(prior_val)
}
