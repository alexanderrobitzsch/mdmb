## File Name: doprobit.R
## File Version: 0.04

doprobit <- function(x, thresh, max_val=99)
{
	thresh <- thresh[ order(thresh) ]
	thresh_upp <- c( thresh , max_val )
	thresh_low <- c( - max_val , thresh )
	res <- stats::pnorm( thresh_upp[ x + 1 ]) - stats::pnorm( thresh_low[ x + 1 ] )
	return(res)
}
