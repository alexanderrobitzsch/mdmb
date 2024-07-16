## File Name: frm_proposal_refresh_helper.R
## File Version: 0.343


#****** refreshing the proposal SD
frm_proposal_refresh_helper <- function( acceptance_parameters, proposal_sd,
        acceptance_bounds )
{
    acc1 <- acceptance_parameters[[1]] / acceptance_parameters[[2]]
    proposal_sd_names <- names(proposal_sd)
    if ( is.null(proposal_sd_names) ){
        proposal_sd_names <- names(acc1)
    }
    if ( is.null(proposal_sd_names) ){
        proposal_sd_names <- seq_len(length.out=length(acc1))
    }
    if (length(acc1)==1){
        proposal_sd_names <- c(1)
    }
    acc <- acc1[ proposal_sd_names ]
    #*** compute new proposal SD
    proposal_sd <- mdmb_refresh_proposal_sd(acc=acc, acceptance_bounds=acceptance_bounds,
                        proposal_sd=proposal_sd)
    #*** clean table for storing acceptance rates
    accept_parms <- acceptance_parameters
    for (uu in 1L:2){
        accept_parms[[uu]] <- 0*accept_parms[[uu]]
    }
    acc1[ proposal_sd_names ] <- acc

    #--- output
    res0 <- list( proposal_sd=proposal_sd, acceptance_parameters=accept_parms, acc=acc1 )
    return(res0)
}
