## File Name: frm_prepare_data_include_latent_data.R
## File Version: 0.15


##################################################################
# include latent data according nodes for 
# calculation in EM algorithm
frm_prepare_data_include_latent_data <- function( dat , var_mm,
	nodes_mm , ind_mm )
{	
	resp_varname <- paste0("resp_" , var_mm) 
	dat[ , resp_varname ] <- 1 - is.na(dat[,var_mm] )
	dat$resp_all <- dat$resp_all * dat[ , resp_varname ]
	dat1 <- dat[ dat[ , resp_varname ] == 1 , ]
	dat0 <- dat[ dat[ , resp_varname ] == 0 , ]
	N0 <- nrow(dat0)
	NX <- length(nodes_mm)	
	dat12 <- dat1
	# if (N0>0){
	#	for (xx in 1:NX){
	#		# xx <- 1
	#		dat11 <- dat0
	#		dat11[,var_mm] <- nodes_mm[xx]
	#		dat11$weights <- dat11$weights * ind_mm$nodes_weights[xx]	
	#		dat12 <- rbind( dat12 , dat11 )
	#	}
	# }
	if (N0>0){
		rep_ind <- rep( 1:N0 , NX )
		dat11 <- dat0[ rep_ind ,  ]
		dat11[,var_mm] <- rep( nodes_mm , each = N0 )
		dat11$weights <- dat11$weights * rep( ind_mm$nodes_weights , each = N0 )
		dat12 <- rbind( dat12 , dat11 )	
	}		
	dat12 <- dat12[ order(dat12$case) , ]
	return(dat12)
}
