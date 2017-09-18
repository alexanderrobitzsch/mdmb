## File Name: rbct_scaled.R
## File Version: 0.02
## File Last Change: 2017-01-23 19:35:14

rbct_scaled <- function(n , location=0, shape=1, lambda = 1, df=Inf)
{
	if (df == Inf){
		df <- 1E5
	}
	x <- stats::rt( n , df=df , ncp=0 )
	x <- location + shape * x
	x <- bc_antitrafo(y=x, lambda =lambda )
	return(x)
}
