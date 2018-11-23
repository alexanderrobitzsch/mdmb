## File Name: frm_proposal_refresh_helper.R
## File Version: 0.331


#****** refreshing the proposal SD
frm_proposal_refresh_helper <- function( acceptance_parameters, proposal_sd,
        acceptance_bounds )
{
    target <- mean(acceptance_bounds)
    acc1 <- acceptance_parameters[[1]] / acceptance_parameters[[2]]
    SD.pp <- proposal_sd
    proposal_sd_names <- names(proposal_sd)
    if ( is.null(proposal_sd_names) ){
        proposal_sd_names <- names(acc1)
    }
    if (length(acc1)==1){
        proposal_sd_names <- c(1)
    }
    acc <- acc1[ proposal_sd_names ]
    #*** compute new proposal SD
    SD.pp <- ifelse( acc < acceptance_bounds[1],
                    SD.pp / ( 2 - acc / target ), SD.pp )
    SD.pp <- ifelse( acc > acceptance_bounds[2],
                    SD.pp * ( 2 - (1-acc)/(1-target) ), SD.pp )
    accept_parms <- acceptance_parameters
    for (uu in 1:2){
        accept_parms[[uu]] <- 0*accept_parms[[uu]]
    }
    acc1[ proposal_sd_names ] <- acc

    #--- output
    res0 <- list( proposal_sd=SD.pp, acceptance_parameters=accept_parms, acc=acc1 )
    return(res0)
}
