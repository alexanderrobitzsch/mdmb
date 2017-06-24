
dt_scaled <- function( x , location=0, shape=1, df = Inf )
{
	if (df==Inf){
		dy <- stats::dnorm( x , mean = location , sd = shape )
	} else {
		dy <- stats::dt( ( x - location ) / shape , df = df ) * 1 / shape
	}
	return(dy)
}
