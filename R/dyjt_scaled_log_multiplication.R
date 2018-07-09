## File Name: dyjt_scaled_log_multiplication.R
## File Version: 0.08

dyjt_scaled_log_multiplication <- function( dy, yt, use_log, check_zero=TRUE )
{
    if (use_log){
        dy <- dy + log(yt)
    } else {
        dy <- dy * yt
    }
    if (check_zero){
        dy[ is.na(dy) ] <- 0
    }
    return(dy)
}

