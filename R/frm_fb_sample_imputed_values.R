## File Name: frm_fb_sample_imputed_values.R
## File Version: 0.698


frm_fb_sample_imputed_values <- function( imputations_mcmc, model_results,
        ind0, iter, dat, aggregation, eps=1E-30 )
{
    NV <- imputations_mcmc$NV
    NM <- attr(ind0,"NM")
    NM1 <- NM + 1
    nrow_dat <- nrow(dat)
    for (vv in 1:NV){
        var_vv <- imputations_mcmc$impute_vars[vv]
        index_vv <- imputations_mcmc$impute_vars_index[vv]
        ind_miss_vv <- imputations_mcmc$ind_miss[[ var_vv ]]
        like_nrow <- N_vv <- length( ind_miss_vv )
        sampling_level_vv <- ind0[[ index_vv ]]$sampling_level
        use_sampling_level_vv <- imputations_mcmc$use_sampling_level[[ vv ]]
        if (use_sampling_level_vv){
            like_nrow <- nrow_dat
            cluster_index <- imputations_mcmc$cluster_index[[ vv ]]
        }
        # dat_vv is a reduced data frame which has missing values in variable vv
        dat_vv <- dat[ ind_miss_vv,, drop=FALSE ]
        imp <- dat_vv[, var_vv ]
        #--- sample new proposed values
        dat1_vv <- dat_vv
        res <- frm_fb_sample_imputed_values_proposal( var_vv=var_vv, index_vv=index_vv,
                    ind0=ind0, imp=imp,    imputations_mcmc=imputations_mcmc, N_vv=N_vv, dat_vv=dat_vv )
        dat1_vv[, var_vv ] <- imp1 <- res$imp1
        do_mh <- res$do_mh
        NG <- res$NG
        gibbs_values <- res$gibbs_values
        like <- matrix( NA, nrow=like_nrow, ncol=NM1 )
        like1 <- like

        #******************************************
        #**** evaluate probabilities in case of Gibbs sampling
        if ( ! do_mh){
            probs_vv <- matrix( NA, nrow=N_vv, ncol=NG)
            for (gg in 1:NG){
                like_temp <- like
                pdat_vv <- dat_vv
                pdat_vv[, var_vv] <- gibbs_values[gg]
                for (mm in 1:NM1){
                    like_temp[,mm] <- frm_fb_sample_imputed_values_eval_likelihood(mm=mm,
                                    model_results=model_results, ind0=ind0, dat_vv=pdat_vv )
                }
                probs_vv[,gg] <- exp( rowSums( log(like_temp + eps ) ))
            }

            ### include cluster level sampling
            if (use_sampling_level_vv){
                stop( paste0( "Imputation step at cluster sampling level is not implemented\n",
                            "for Gibbs sampling.") )
            }
            probs_vv <- frm_normalize_matrix_row(matr=probs_vv)
            imp1 <- gibbs_values[ mdmb_sample_probabilities(matr=probs_vv) ]
            dat_vv[,var_vv] <- dat1_vv[,var_vv] <- imp1
        }
        #******************************************
        #**** evaluate models under old value and new proposed value
        if (do_mh){
            for (mm in 1:NM1){
                args_like <- list(mm=mm, model_results=model_results, ind0=ind0, dat_vv=dat_vv,
                            aggregation=aggregation, dat=dat, ind_miss_vv=ind_miss_vv,
                            sampling_level_vv=sampling_level_vv, use_sampling_level_vv=use_sampling_level_vv )
                if (do_mh){
                    like[,mm] <- do.call( what=frm_fb_sample_imputed_values_eval_likelihood, args=args_like)
                }
                args_like$dat_vv <- dat1_vv
                like1[,mm] <- do.call( what=frm_fb_sample_imputed_values_eval_likelihood, args=args_like)
            }  # end mm
        }
        #**** evaluation proposal in Metropolis-Hastings sampling
        if (do_mh){
            accept <- frm_fb_sample_imputed_values_evaluate_mh_ratio( like=like, like1=like1,
                            use_sampling_level_vv=use_sampling_level_vv, cluster_index=cluster_index,
                            ind_miss_vv=ind_miss_vv, eps=eps )
            if ( sum(accept) > 0){
                dat_vv[ accept, var_vv ] <- dat1_vv[ accept, var_vv ]
            }
            mh_vv <- imputations_mcmc$mh_imputations_values[[ var_vv ]]
            mh_vv$iter <- mh_vv$iter + 1
            mh_vv$accepted <- mh_vv$accepted + accept
            imputations_mcmc$mh_imputations_values[[ var_vv ]] <- mh_vv
        } else {  # no MH sampling
            dat_vv[, var_vv ] <- dat1_vv[, var_vv ]
        }

        dat[ ind_miss_vv, var_vv ] <- dat_vv[, var_vv ]
        ind_save <- match( iter, imputations_mcmc$imp_save )
        save_values <- ( ! is.na( ind_save ) )
        if ( save_values ){
            imputations_mcmc$values[[var_vv]][, ind_save ] <- dat_vv[, var_vv ]
        }

    }  # end vv
    #--------------------------------
    #--- output
    res <- list( imputations_mcmc=imputations_mcmc, dat=dat)
    return(res)
}

