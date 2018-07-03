## File Name: frm_fb_sample_imputed_values_eval_likelihood.R
## File Version: 0.23


frm_fb_sample_imputed_values_eval_likelihood <- function(mm, model_results, ind0,
        dat_vv, aggregation=FALSE, dat=NULL, ind_miss_vv=NULL,
        sampling_level_vv=NULL, use_sampling_level_vv=FALSE )
{
    ind_mm <- ind0[[mm]]
    mod <- model_results[[mm]]
    model <- mod
    if (use_sampling_level_vv){
        aggregation <- TRUE
    }
    if (aggregation){
        dat_sel <- dat
        dat_sel[ ind_miss_vv, colnames(dat_vv) ] <- dat_vv
    } else {
        dat_sel <- dat_vv
    }
    y <- dat_sel[, ind_mm$dv_vars ]
    case <- dat_sel$case
    design_matrix <- dat_sel
    args <- list(model=model, y=y, case=case, design_matrix=design_matrix )
    dmod <- do.call( what=ind_mm$R_density_fct, args=args )
    like <- dmod$like
    if (aggregation & ( ! use_sampling_level_vv ) ){
        like <- like[ ind_miss_vv ]
    }
    return(like)
}
