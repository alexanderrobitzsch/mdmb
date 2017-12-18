## File Name: logthresh_2_thresh.R
## File Version: 0.01

logthresh_2_thresh <- function(x)
{
	cumsum( exp(x) )
}
