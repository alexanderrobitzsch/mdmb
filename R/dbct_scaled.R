
dbct_scaled <- function( x , location=0, shape=1, lambda = 1 , df = Inf, log = FALSE )
{
	#*** recode lambda
	eps <- 1E-3
	# lambda <- yj_adjust_lambda( lambda = lambda , lambda0 = 0 , eps = eps )	
	xt <- bc_trafo(y=x , lambda =lambda )
	if ( df==Inf ){
		dy <- stats::dnorm( x = xt , mean = location , sd = shape , log = log )
	} else {
		dy <- dt_scaled( x = xt , location = location , shape = shape , df = df , log = log)
	}		
	#--------- adjustment for derivative of Box-Cox transformation
	y <- x
	if ( abs(lambda) > eps ){
		yt <- y^( lambda - 1 )
	} else {
		yt <- 1 / y
	}
	yt <- ifelse( x <= 0 , 0 , yt )	
	#---- multiplicative adjustment	
	dy <- dyjt_scaled_log_multiplication( dy=dy , yt=yt , use_log=log )		
	return(dy)
}
