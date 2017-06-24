

frm_fb_sample_parameters <- function( dat, ind0 , NM, eps =1E-30, iter = NULL ,
		weights0=NULL, dat_resp, ind_resp, ind_miss, model_results,
		parms_mcmc )
{
	weights <- dat$weights	
	N2 <- nrow(dat)
	NM1 <- NM + 1	
	for (mm in 1:NM1 ){	
		ind_mm <- ind0[[mm]]
# cat("\n---------------------- mm =" , mm , " --------\n")			
# Revalpr("ind_mm$formula")		
		#--- likelihood evaluated at old parameter
		mod <- model_results[[mm]]
		coef0 <- mod$coef		
		sigma0 <- ind_mm$sigma
		mod$sigma <- sigma0	
		NC <- ind_mm$N_coef	
		res0 <- frm_fb_sample_parameter_step( ind_mm=ind_mm , dat=dat , 
					weights=weights , mod = mod ,
					coef = coef0 , sigma = sigma0)			
		if (NC > 0){
			for (cc in 1:NC){		
				coef1 <- coef0
				coef1[cc] <- stats::rnorm(1, mean=coef0[cc], 
								sd = ind_mm$coef_sd_proposal[cc])	
				res1 <- frm_fb_sample_parameter_step( ind_mm=ind_mm , dat=dat , 
								weights=weights , mod = mod ,
								coef = coef1 , sigma = sigma0)					
				logprob_cc <- res1$ll - res0$ll
				prob_cc <- exp( logprob_cc)
				# simulate random number
				rn0 <- stats::runif(1)
				accept <- 1 * ( rn0 < prob_cc )
				if ( ! is.finite(accept) ){
					accept <- FALSE
				}
				ind_mm$coef_MH$iter[cc] <- ind_mm$coef_MH$iter[cc] + 1
				if (accept){
					coef0 <- coef1
					res0 <- res1
					ind_mm$coef_MH$accepted[cc] <- ind_mm$coef_MH$accepted[cc] + 1
				}			
			}
		}
		#--- sample sigma
		sample_sigma <- ind_mm$sample_sigma
		if ( sample_sigma){
			sigma1 <- stats::rnorm(1, mean= sigma0 , 
							sd = ind_mm$sigma_sd_proposal)	
			res1 <- frm_fb_sample_parameter_step( ind_mm=ind_mm , dat=dat , 
							weights=weights , mod = mod ,
							coef = coef0 , sigma = sigma1)					
			logprob_cc <- res1$ll - res0$ll
			prob_cc <- exp( logprob_cc)
			rn0 <- stats::runif(1)
			accept <- 1 * ( rn0 < prob_cc )	
			if ( ! is.finite(accept) ){
				accept <- FALSE
			}			
			ind_mm$sigma_MH$iter <- ind_mm$sigma_MH$iter + 1
			if (accept){
				sigma0 <- sigma1
				res0 <- res1
				ind_mm$sigma_MH$accepted <- ind_mm$sigma_MH$accepted + 1
			}
		}
		mod$coefficients <- coef0
		ind_mm$coef0 <- coef0
		mod$sigma <- sigma0
		ind_mm$sigma <- sigma0
		model_results[[mm]] <- mod
		ind0[[mm]] <- ind_mm
	}
	#------------
	# save sampled parameters
	iter_save_temp <- parms_mcmc$iter_save_temp
	if (iter==iter_save_temp){
		ii <- parms_mcmc$iter_save_index
		parms_mcmc$iter_save_index <- parms_mcmc$iter_save_index + 1
		parms_mcmc$iter_save_temp <- parms_mcmc$iter_save[ parms_mcmc$iter_save_index ]		
		for (mm in 1:NM1){
			# mm <- 1
			i0 <- parms_mcmc$parms_index[[mm]][[1]]
			nc_mm <- ind0[[mm]]$N_coef
			if ( nc_mm > 0 ){
				parms_mcmc$values[ ii , i0 ] <- coef(model_results[[mm]])
			}
			i0 <- parms_mcmc$parms_index[[mm]][[2]]
			sample_sigma <- ind0[[mm]]$sample_sigma			
			if ( sample_sigma ){
				parms_mcmc$values[ ii , i0 ] <- model_results[[mm]]$sigma
			}		
		}
		vars_d <- parms_mcmc$vars_descriptives
		parms_mcmc$M_mcmc[ii,] <- colMeans( dat[,vars_d ] )
		parms_mcmc$SD_mcmc[ii,] <- apply( dat[,vars_d ] , 2 , stats::sd )
	}
	#--------------------------------
	#--- output
	res <- list( model_results = model_results , ind0 = ind0,
					parms_mcmc = parms_mcmc  )
	return(res)			
}			   
	
