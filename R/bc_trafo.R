## File Name: bc_trafo.R
## File Version: 0.03
## File Last Change: 2017-01-23 19:35:11

bc_trafo <- function( y , lambda )
{
	eps <- 1E-3
	# lambda <- yj_adjust_lambda( lambda = lambda , lambda0 = 0 , eps = eps )
	if ( abs(lambda) > eps ){
		yt <- ( y^lambda - 1 ) / lambda
	} else {
		yt <- log(y)
	}
	return(yt)
}
