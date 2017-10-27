## File Name: dt_scaled.R
## File Version: 0.07

dt_scaled <- function( x , location=0, shape=1, df = Inf, log=FALSE )
{
	use_log <- log
	if (df==Inf){
		dy <- stats::dnorm( x , mean = location , sd = shape , log = log)
	} else {
		dy <- stats::dt( ( x - location ) / shape , df = df, log=log ) 
		if ( use_log ){
			dy <- dy - log(shape)
		} else {
			dy <- dy / shape
		}
	}
	return(dy)
}
