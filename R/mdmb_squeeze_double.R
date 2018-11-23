## File Name: mdmb_squeeze_double.R
## File Version: 0.02

mdmb_squeeze_double <- function(val, min=-Inf, max=Inf)
{
    if( val > max ){
        val <- max
    } else {
        if (val < min){
            val <- min
        }
    }
    return(val)
}
