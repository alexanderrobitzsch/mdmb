## File Name: frm_fb_sample_imputed_values_evaluate_mh_ratio.R
## File Version: 0.12


frm_fb_sample_imputed_values_evaluate_mh_ratio <- function( like, like1,
    use_sampling_level_vv=FALSE, cluster_index=NULL, ind_miss_vv=NULL, eps=1E-30,
    ind_vv=NULL )
{
    like1 <- rowSums( log( like1 + eps ) )
    like <- rowSums( log( like + eps ) )
    like_diff <- like1 - like
    use_variable_level <- ind_vv$use_variable_level
    if (use_variable_level){
        variable_info <- ind_vv$variable_info
        cluster_index <- variable_info$id
        use_sampling_level_vv <- FALSE
    }
    if (use_sampling_level_vv | use_variable_level){
        like_diff <- rowsum( like_diff, cluster_index )
        if (use_variable_level){
            replace_miss_id <- match(cluster_index, rownames(like_diff) )
        }
        like_diff <- like_diff[,1]
    }
    prob_nn <- mdmb_exp_overflow( like_diff )
    pr_vv <- length(prob_nn)
    rn_nn <- stats::runif(pr_vv)
    accept <- rn_nn < prob_nn
    accept[ is.na(accept) ] <- FALSE
    if (use_sampling_level_vv){
        accept <- accept[ cluster_index ]
        accept <- accept[ ind_miss_vv ]
    }
    if (use_variable_level){
        accept <- accept[ replace_miss_id ]
    }
    return(accept)
}
