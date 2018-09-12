## File Name: frm_fb_refresh_parameters_step.R
## File Version: 0.14

frm_fb_refresh_parameters_step <- function( mm, ind0, name_MH, name_sd_proposal,
        acc_bounds, proposal_sd_min )
{
    c_mh_mm <- ind0[[mm]][[name_MH]]
    c_sd_prop <- ind0[[mm]][[name_sd_proposal]]
    res2 <- frm_proposal_refresh_helper( acceptance_parameters=c_mh_mm,
                    proposal_sd=c_sd_prop, acceptance_bounds=acc_bounds)
    ind0[[mm]][[name_MH]] <- res2$acceptance_parameters
    ind0[[mm]][[name_sd_proposal]] <- res2$proposal_sd
    return(ind0)
}
