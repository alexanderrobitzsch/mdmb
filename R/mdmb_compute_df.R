## File Name: mdmb_compute_df.R
## File Version: 0.11

mdmb_compute_df <- function(x, df=Inf, est_df=FALSE, maxval=6)
{
    if (est_df){
        logdf <- x['logdf']
        if (logdf<maxval){
            df <- exp(logdf)
        } else {
            df <- Inf
        }
    }
    return(df)
}
