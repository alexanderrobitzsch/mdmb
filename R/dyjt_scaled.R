## File Name: dyjt_scaled.R
## File Version: 0.14

dyjt_scaled <- function( x , location=0, shape=1, lambda = 1 , df = Inf, log = FALSE )
{
	#*** recode lambda
	eps <- 1E-3
	for ( lambda0 in c(0,2) ){
		lambda <- yj_adjust_lambda( lambda = lambda , lambda0 = lambda0 , eps = eps )
	}
	xt <- yj_trafo(y=x , lambda =lambda )
	if ( df==Inf ){
		dy <- stats::dnorm( x = xt , mean = location , sd = shape, log=log )
	} else {
		dy <- dt_scaled( x = xt , location = location , shape = shape , df = df , log=log)
	}
	#--------- adjustment for derivative of Yeo-Johnson transformation
	yt <- y <- x
	#*** y >= 0
	yt <- ifelse( y >= 0 ,  ( yt + 1 )^(lambda-1) , yt )
	#*** y <= 0
	yt <- ifelse( y < 0 , ( - yt + 1 )^(1-lambda)  , yt )
	#---- multiplicative adjustment	
	dy <- dyjt_scaled_log_multiplication( dy=dy , yt=yt , use_log=log )					
	return(dy)
}
