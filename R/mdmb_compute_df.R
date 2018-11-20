## File Name: mdmb_compute_df.R
## File Version: 0.08

mdmb_compute_df <- function(x, df=Inf, est_df=FALSE)
{
    if (est_df){
        logdf <- x["logdf"]
        if (logdf<6){
            df <- exp(logdf)
        } else {
            df <- Inf
        }
    }
    return(df)
}
