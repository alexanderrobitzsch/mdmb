## File Name: frm_proposal_refresh_helper.R
## File Version: 0.11


#################################################################
# refreshing the proposal SD
frm_proposal_refresh_helper <- function( acceptance_parameters, proposal_sd,
        acceptance_bounds ){
    target <- mean(acceptance_bounds)
    acc <- acceptance_parameters[[1]] / acceptance_parameters[[2]]
    SD.pp <- proposal_sd
    #*** compute new proposal SD
    SD.pp <- ifelse( acc < acceptance_bounds[1],
                    SD.pp / ( 2 - acc / target ), SD.pp )
    SD.pp <- ifelse( acc > acceptance_bounds[2],
                    SD.pp * ( 2 - (1-acc)/(1-target) ), SD.pp )
    accept_parms <- acceptance_parameters
    for (uu in 1:2){
        accept_parms[[uu]] <- 0*accept_parms[[uu]]
    }
    #--- output
    res0 <- list( proposal_sd=SD.pp,
                    acceptance_parameters=accept_parms, acc=acc )
    return(res0)
}
#################################################################
