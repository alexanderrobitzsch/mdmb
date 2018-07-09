## File Name: frm_estimate_model_create_R_args.R
## File Version: 0.02

frm_estimate_model_create_R_args <- function(dat, weights, ind_mm)
{
    dat11 <- dat
    weights11 <- weights
    id_variable_level_unique <- ind_mm$id_variable_level_unique
    use_variable_level <- ind_mm$use_variable_level
    if (use_variable_level){
        dat11 <- dat[ id_variable_level_unique, ]
        weights11 <- weights[ id_variable_level_unique ]
    }
    R_args <- list( formula=ind_mm$formula, data=dat11, weights=weights11)
    return(R_args)
}
