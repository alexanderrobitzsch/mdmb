## File Name: frm_fb_sample_imputed_values_eval_likelihood.R
## File Version: 0.448


frm_fb_sample_imputed_values_eval_likelihood <- function(mm, model_results, ind0,
        dat_vv, aggregation=FALSE, dat=NULL, ind_miss_vv=NULL,
        sampling_level_vv=NULL, use_sampling_level_vv=FALSE )
{
    ind_mm <- ind0[[mm]]
    mod <- model_results[[mm]]
    use_variable_level_mm <- ind_mm$use_variable_level
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

    # dat_sel <- dat_sel[ ind_mm$id_variable_level_unique, ]
    y <- dat_sel[, ind_mm$dv_vars ]
    case <- dat_sel$case
    design_matrix <- dat_sel
    args <- list(model=model, y=y, case=case, design_matrix=design_matrix )
    if (ind_mm$R_density_fct=="frm_linreg_density"){
        args$use_in_frm_fb <- TRUE
    }
    if (ind_mm$R_density_fct=="frm_mlreg_density"){
        args$id_variable_level_unique <- ind_mm$id_variable_level_unique
    }
    dmod <- do.call( what=ind_mm$R_density_fct, args=args )
    like <- dmod$like
    #- adjust likelihood for cluster-level variables
    if (use_variable_level_mm){
        variable_level <- ind_mm$variable_level
        idcluster <- dat_sel[, variable_level]
        freq_cluster <- rowsum( 1+0*idcluster, idcluster)
        freq_cluster <- freq_cluster[ match( idcluster, rownames(freq_cluster) ), 1]
        like <- like^( 1 / freq_cluster )
    }
    if (aggregation & ( ! use_sampling_level_vv ) ){
        like <- like[ ind_miss_vv ]
    }
    return(like)
}
