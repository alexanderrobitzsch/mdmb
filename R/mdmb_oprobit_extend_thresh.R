## File Name: mdmb_oprobit_extend_thresh.R
## File Version: 0.01

mdmb_oprobit_extend_thresh <- function(thresh, max_val=99)
{
    thresh <- c(0, thresh )
    thresh_low <- c( -max_val, thresh)
    thresh_upp <- c( thresh, max_val )
    res <- list(thresh_low=thresh_low, thresh_upp=thresh_upp)
    return(res)
}
