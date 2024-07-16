## File Name: eval_prior_list_sumlog.R
## File Version: 0.136


eval_prior_list_sumlog <- function( par, par_prior, use_grad=1 )
{
    eps <- 1E-50
    if ( use_grad < 2){
        v1 <- eval_prior_list( par=par, par_prior=par_prior )
        v1 <- log(v1+eps)
    }
    if ( use_grad==2){
        NP <- length(par)
        v2 <- rep(NA, NP)
        densities_derivatives <- c( 'dnorm' )
        for (pp in 1L:NP){
            par_prior_pp <- par_prior[[pp]][[1]]
            if ( par_prior_pp %in% densities_derivatives){
                pp_args <- as.list( par_prior[[pp]][[2]] )
                pp_args[['x']] <- par[pp]
                v2[pp] <- log( do.call( what=par_prior_pp, args=pp_args ) + eps )
            }
            if ( par_prior_pp=='dnorm'){
                quadform <- 0.5 * ( pp_args$x - pp_args$mean )^2 / pp_args$sd^2
                v2[pp] <- - 0.5 * log(2*pi) - log( pp_args$sd ) - quadform
            }
        }
        v1 <- v2
    }
    res <- sum( v1 )
    return(res)
}
