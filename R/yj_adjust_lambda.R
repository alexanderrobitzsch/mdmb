
yj_adjust_lambda <- function( lambda , lambda0 , eps = 1E-3 )
{
	if ( abs( lambda - lambda0 ) < eps ){
		sign_lambda <- 1 * ( sign(lambda) >= 0  )
		lambda <- lambda0 +  sign_lambda * eps
	}
	return(lambda)
}
