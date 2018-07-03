## File Name: frm_fb_sample_imputed_values_evaluate_mh_ratio.R
## File Version: 0.02


frm_fb_sample_imputed_values_evaluate_mh_ratio <- function( like, like1,
    use_sampling_level_vv=FALSE, cluster_index=NULL, ind_miss_vv=NULL, eps=1E-30 )
{
    like1 <- rowSums( log( like1 + eps ) )
    like <- rowSums( log( like + eps ) )
    like_diff <- like1 - like
    if (use_sampling_level_vv){
        like_diff <- rowsum( like_diff, cluster_index )[,1]
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
    return(accept)
}
