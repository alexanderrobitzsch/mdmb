## File Name: frm_fb_init_matrices_saved_parameters.R
## File Version: 0.18

frm_fb_init_matrices_saved_parameters <- function( iter, burnin,
        Nsave, Nimp, npars, parms, parms_index, predictorMatrix )
{
    if (burnin > iter){
        burnin <- round(iter/2)
    }
    # number of iterations
    iter_main <- iter - burnin
    Nsave0 <- min( iter_main, Nsave )
    steps <- floor( iter_main / Nsave0 )
    iter_save <- seq( burnin + 1, iter, steps )
    iter <- max(iter_save)
    NI <- length(iter_save)
    values <- matrix( NA, nrow=NI, ncol=npars, byrow=TRUE )
    rownames(values) <- iter_save
    parms_unlist <- unlist( parms )
    parms_unlist <- parms_unlist[ ! is.na( parms_unlist) ]
    colnames(values) <- parms_unlist
    iter_save_temp <- iter_save[1]
    iter_save_index <- 1
    # M and SD
    vars <- rownames(predictorMatrix)
    IV <- length(vars)
    M_mcmc <- matrix( NA, nrow=NI, ncol=IV, byrow=TRUE )
    rownames(M_mcmc) <- iter_save
    colnames(M_mcmc) <- vars
    SD_mcmc <- M_mcmc
    #--- output
    res <- list( values=values, NI=NI, iter=iter, iter_save=iter_save,
                parms=parms, parms_index=parms_index,
                parms_unlist=parms_unlist, iter_save_temp=iter_save_temp,
                iter_save_index=iter_save_index, M_mcmc=M_mcmc,
                SD_mcmc=SD_mcmc, vars_descriptives=vars, burnin=burnin )
    return(res)
}
