## File Name: frm_prepare_data_fb.R
## File Version: 0.10

frm_prepare_data_fb <- function(dat, dep, ind, weights0, dat0, data_init )
{
	NM <- attr(ind,"NM")
	NM1 <- NM + 1
	N <- nrow(dat)		
	dat$case <- 1:N
	dat$weights0 <- weights0
	dat$weights <- 1
	dat$resp_all <- 1
	# vector of dependent variables
	dv_vars <- c()
	ind00 <- ind
	ind[[ dep$dv_vars ]] <- dep
	
	for (mm in 1:NM1){	
		# mm <- 1
		ind_mm <- ind[[mm]]		
		var_mm <- ind_mm$dv_vars
		dv_vars <- c( dv_vars , var_mm)
		r_mm <- paste0("resp_" , var_mm)
		dat[ , r_mm] <- 1 * ( 1 - is.na( dat[,var_mm] )	)	
		dat$resp_all <- dat$resp_all * dat[, r_mm ]
		# include initial values for dat[,var_mm]
		M_r_mm <- mean( dat[,r_mm] )
		N1 <- sum( dat[,r_mm] )
		N0 <- sum( 1 - dat[,r_mm] )
		y <- dat[,var_mm]
		ind1_r_mm <- which( dat[,r_mm] == 1 )
		ind0_r_mm <- which( dat[,r_mm] == 0 )
		if ( M_r_mm > 0 ){	# some observations			
			y <- y[ ind1_r_mm ]
			y0 <- sample(y , N0 , replace=TRUE )
			dat[ ind0_r_mm , var_mm] <- y0
		}
		if ( M_r_mm == 0 ){	# some observations			
			y0 <- data_init[ , var_mm]
			if ( length(y0) == 0 ){
				stop_mess <- paste0("Provide initial values for variable " ,
						var_mm , " in argument 'data_init'!\n")			
				stop( stop_mess )
			}
		}			
	}	
	#** prepare dependent variables
	dat$weights <- dat$weights * dat$weights0	
	
	#--- include data_init if it is not NULL
	if ( ! is.null( data_init ) ){
		cn <- colnames(data_init)
		dat[ , cn ] <- data_init
	}		
	
	dep <- ind[[ dep$dv_vars ]]
	ind[[ dep$dv_vars ]] <- NULL
	
	# extract matrix with response indicators
	dat_resp <- dat[ , paste0( "resp_" , dv_vars ) ]
	ind_resp <- as.list( dv_vars )
	names(ind_resp) <- dv_vars
	ind_miss <- ind_resp
	freq_miss_values <- rep(0,NM+1)
	names(freq_miss_values) <- dv_vars
	for (mm in 1:(NM+1) ){
		ind_resp[[mm]] <- which( dat_resp[,mm] == 1)
		ind_miss[[mm]] <- which( dat_resp[,mm] == 0)
		freq_miss_values[mm] <- sum( is.na( dat0[ , dv_vars[mm] ] ) )
	}	
	#*** variables to be imputed
	impute_vars <- names(freq_miss_values)[ freq_miss_values > 0]
	impute_vars_index <- match( impute_vars , dv_vars )
	#--- output
	res <- list( dat=dat , dv_vars = dv_vars , dat_resp = dat_resp ,
				ind_resp = ind_resp, ind_miss=ind_miss,
				freq_miss_values=freq_miss_values, impute_vars=impute_vars ,
				impute_vars_index=impute_vars_index)
	return(res)
}
