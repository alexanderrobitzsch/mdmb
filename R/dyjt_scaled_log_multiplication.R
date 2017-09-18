## File Name: dyjt_scaled_log_multiplication.R
## File Version: 0.03
## File Last Change: 2017-08-19 12:32:58

dyjt_scaled_log_multiplication <- function( dy , yt , use_log )
{
	if (use_log){
		dy <- dy + log(yt)
	} else {
		dy <- dy * yt		
	}
	dy[ is.na(dy) ] <- 0	
	return(dy)
}
	
