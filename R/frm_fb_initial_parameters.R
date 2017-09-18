## File Name: frm_fb_initial_parameters.R
## File Version: 0.22
## File Last Change: 2017-02-07 12:09:06

frm_fb_initial_parameters <- function(dat, ind0, data_init )
{
	weights <- dat$weights
	NM <- attr(ind0 , "NM")
	NM1 <- NM + 1
	if ( ! is.null(data_init) ){
		dat <- data_init
	}	
	model_results <- list()	
	parms <- list()	
	parms00 <- list( NA , NA )
	for (mm in 1:NM1){
		# mm <- 1	
#  cat("\n------ mm = " , mm , "----- \n")		
		ind_mm <- ind0[[mm]]
		var_mm <- ind_mm$dv_vars
		model_mm <- ind_mm$model
		parms0 <- parms00
		#--- estimate model with weights
		R_args <- list( formula = ind_mm$formula, data = dat, weights = weights)
		R_args <- frm_append_list(list1=R_args, list2=ind_mm$R_args)		
		mod <- do.call( what=ind_mm$R_fct , args = R_args )		
		model_results[[mm]] <- mod	
		se_mod <- sqrt( diag( vcov(mod) ) )
		ind_mm$N_coef <- length(se_mod)		
		ind_mm$coef <- coef(mod)	
		ind_mm$coef_sd_proposal <- se_mod
		ind_mm$coef_parnames <- NA
		if ( ind_mm$N_coef > 0 ){
			names_mm <- names(ind_mm$coef)
			NM <- length(names_mm)
			on1 <- rep(" ON " , NM )
			if (ind_mm$model %in% c("bctreg","yjtreg") ){
				on1[ (NM-1):NM ] <- " "
			}
			ind_mm$coef_parnames <- paste0( var_mm , on1 , names_mm )
		}
		parms0[[1]] <- ind_mm$coef_parnames		
		v1 <- 0*coef(mod)
		ind_mm$coef_MH$accepted <- v1
		ind_mm$coef_MH$iter <- v1

		ind_mm$N_sigma <- 0						
		ind_mm$sigma_parnames <- NULL
		ind_mm$sample_sigma <- FALSE
		# residual standard deviation
		est_sigma <- model_mm %in% c( "linreg")
		if ( ! is.null( ind_mm$sigma_fixed ) ){
			est_sigma <- FALSE
		}
		if ( est_sigma ){
			sigma <- TAM::weighted_sd( residuals(mod) , weights )
			ind_mm$N_sigma <- 1
			ind_mm$sample_sigma <- TRUE
			ind_mm$sigma <- sigma
			ind_mm$sigma_sd_proposal <- sigma / 20
			ind_mm$sigma_parnames <- paste0( var_mm , " sigma" )
			parms0[[2]] <- ind_mm$sigma_parnames
			ind_mm$sigma_MH$accepted <- 0
			ind_mm$sigma_MH$iter <- 0
		}
		parms[[mm]] <- parms0
		ind0[[mm]] <- ind_mm	
	}
	#--- indices for parameters to be saved
	parms_index <- parms
	N0 <- 0
	for (mm in 1:NM1){
		for (vv in 1:2){
			p_m_vv <- parms_index[[mm]][[vv]]
			p_m_vv <- p_m_vv[ ! is.na( p_m_vv ) ]
			NC <- length( p_m_vv )
			if ( NC > 0 ){
				vec0 <- N0 + seq(1,NC)				
				N0 <- N0 + NC				
				parms_index[[mm]][[vv]] <- vec0
			}
		}	
	}		
	#--- output
	res <- list(ind0=ind0, parms = parms, parms_index = parms_index ,
				npars = N0 , model_results = model_results )
	return(res)
}	

	
