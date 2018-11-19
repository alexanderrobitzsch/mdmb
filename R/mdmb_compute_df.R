## File Name: mdmb_compute_df.R
## File Version: 0.03

mdmb_compute_df <- function(x, df=Inf, est_df=FALSE)
{
    if (est_df){
        df <- exp(x["logdf"])
    }
    return(df)
}
