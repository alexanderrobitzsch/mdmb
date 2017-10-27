## File Name: eval_prior_list_gradient_log.R
## File Version: 0.09

eval_prior_list_gradient_log <- function( par , par_prior, h )
{
	eps <- 1e-50
	# ll0 <- log( eval_prior_list( par=par , par_prior=par_prior ) + eps )
	hvec <- mdmb_regression_adjustment_differentiation_parameter(h=h , par=par )
	# ll1 <- log( eval_prior_list( par=par+hvec , par_prior=par_prior ) + eps )
	# der1 <- mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=hvec)	
	NP <- length(par)
	der2 <- rep(NA, NP )
	densities_derivatives <- c( "dnorm" )
	for (pp in 1:NP){	
		par_prior_pp <- par_prior[[pp]][[1]] 				
		if ( par_prior_pp %in% densities_derivatives){
			pp_args <- as.list( par_prior[[pp]][[2]] )
			pp_args[["x"]] <- par[pp]
			p0 <- log( do.call( what= par_prior_pp , args = pp_args ) + eps )
			h_pp <- hvec[pp]
			pp_args[["x"]] <- par[pp] + h_pp
			p1 <- log( do.call( what= par_prior_pp , args = pp_args ) + eps )
			der2[pp] <- mdmb_diff_quotient(ll0=p0, ll1=p1, h=h_pp )
		}
		if ( par_prior_pp == "dnorm"){
			# d0 <- - 0.5 * log(2*pi) - log( pp_args$sd ) - 0.5 * ( pp_args$x - pp_args$mean )^2 / pp_args$sd^2
			der2[pp] <-  - ( pp_args$x - pp_args$mean ) / pp_args$sd^2
		}
	}
	return(der2)	
}	
