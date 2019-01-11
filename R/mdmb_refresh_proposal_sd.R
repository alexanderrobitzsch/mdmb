## File Name: mdmb_refresh_proposal_sd.R
## File Version: 0.02

mdmb_refresh_proposal_sd <- function(acc, acceptance_bounds, proposal_sd)
{
    target <- mean(acceptance_bounds)
    proposal_sd <- ifelse( acc < acceptance_bounds[1],
                    proposal_sd / ( 2 - acc / target ), proposal_sd )
    proposal_sd <- ifelse( acc > acceptance_bounds[2],
                    proposal_sd * ( 2 - (1-acc)/(1-target) ), proposal_sd )
    return(proposal_sd)
}
