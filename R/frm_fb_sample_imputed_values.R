## File Name: frm_fb_sample_imputed_values.R
## File Version: 0.29
## File Last Change: 2017-02-07 12:08:47


frm_fb_sample_imputed_values <- function( imputations_mcmc, model_results,
		ind0 , iter, dat , eps=1E-30 )
{
	NV <- imputations_mcmc$NV
	NM <- attr(ind0,"NM")
	NM1 <- NM + 1
	for (vv in 1:NV){
		# vv <- 2
		var_vv <- imputations_mcmc$impute_vars[vv]
#  cat("\n---------------\n")		
#  Revalpr("var_vv")		
		index_vv <- imputations_mcmc$impute_vars_index[vv]
		ind_miss_vv <- imputations_mcmc$ind_miss[[vv]]
		N_vv <- length( ind_miss_vv )
		# dat_vv is a reduced data frame which has missing values in variable vv
		dat_vv <- dat[ ind_miss_vv , , drop=FALSE ]
		imp <- dat_vv[ , var_vv ]
		#--- sample new proposed values
		dat1_vv <- dat_vv
 		res <- frm_fb_sample_imputed_values_proposal( var_vv=var_vv , 
					index_vv=index_vv ,	ind0=ind0, imp=imp , 
					imputations_mcmc=imputations_mcmc, N_vv=N_vv,
					dat_vv = dat_vv )
		dat1_vv[ , var_vv ] <- imp1 <- res$imp1		
		do_mh <- res$do_mh	
		NG <- res$NG
		gibbs_values <- res$gibbs_values	
		like <- matrix( NA , nrow=N_vv , ncol=NM1 )
		like1 <- like
		#******************************************
		#**** evaluate probabilities in case of Gibbs sampling
		if ( ! do_mh){
			probs_vv <- matrix( NA , nrow=N_vv , ncol=NG)
			for (gg in 1:NG){
				# gg <- 1
				like_temp <- like
				pdat_vv <- dat_vv
				pdat_vv[ , var_vv] <- gibbs_values[gg]
				for (mm in 1:NM1){
					like_temp[,mm] <- frm_fb_sample_imputed_values_eval_likelihood(mm=mm, 
									model_results=model_results, ind0=ind0, dat_vv=pdat_vv )
				}		
				probs_vv[,gg] <- exp( rowSums( log(like_temp + eps ) ))
			}
			probs_vv <- frm_normalize_matrix_row(matr=probs_vv)
			imp1 <- gibbs_values[ mdmb_sample_probabilities(matr=probs_vv) ]
			dat_vv[,var_vv] <- dat1_vv[,var_vv] <- imp1
		}
		#******************************************		
		#**** evaluate models under old value and new proposed value
		if (do_mh){
			for (mm in 1:NM1){
				# mm <- 1
# cat("\n ------- mm = " , mm , " ----\n")				
				if (do_mh){
					like[,mm] <- frm_fb_sample_imputed_values_eval_likelihood(mm=mm, 
							model_results=model_results, ind0=ind0, dat_vv=dat_vv )
				}
				like1[,mm] <- frm_fb_sample_imputed_values_eval_likelihood(mm=mm, 
						model_results=model_results, ind0=ind0, dat_vv=dat1_vv )			
			}
		}
		#**** evaluation proposal in Metropolis-Hastings sampling
		if (do_mh){
			like1 <- rowSums( log( like1 + eps ) )
			like <- rowSums( log( like + eps ) )
			prob_nn <- exp( like1 - like )
			rn_nn <- stats::runif(N_vv)
			accept <- rn_nn < prob_nn
			accept[ ! is.finite(accept)	] <- FALSE
			LA <- sum(accept)
			if (LA > 0){
				dat_vv[ accept , var_vv ] <- dat1_vv[ accept , var_vv ]
			}
			mh_vv <- imputations_mcmc$mh_imputations_values[[ var_vv ]]
			mh_vv$iter <- mh_vv$iter + 1	
			mh_vv$accepted <- mh_vv$accepted + accept
			imputations_mcmc$mh_imputations_values[[ var_vv ]] <- mh_vv	
		} else {  # no MH sampling
			dat_vv[ , var_vv ] <- dat1_vv[ , var_vv ]
		}
		
		dat[ ind_miss_vv , var_vv ] <- dat_vv[ , var_vv ]
		ind_save <- match( iter, imputations_mcmc$imp_save )
		save_values <- ( ! is.na( ind_save ) )
		if ( save_values ){
			imputations_mcmc$values[[var_vv]][, ind_save ] <- dat_vv[ , var_vv ]
		}
	}
	#--------------------------------
	#--- output
	res <- list( imputations_mcmc=imputations_mcmc, dat = dat)
	return(res)			
}			   
	
