
remove_NA_data_frame <- function( formula , data , weights=NULL)
{
	#-- include weights
	N <- nrow(data)
	if ( is.null(weights) ){
		weights <- rep(1,N)	
	}			
	#--- remove missings
	data_vars <- stats::get_all_vars(formula=formula, data = data )
	ind <- which( rowMeans( 1 - is.na(data_vars) ) == 1 )
	data <- data[ ind , ]
	weights <- weights[ ind ]
	#--- output
	res <- list( data = data , weights = weights )
	return(res)		
}	
