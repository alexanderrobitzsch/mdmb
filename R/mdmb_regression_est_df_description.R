## File Name: mdmb_regression_est_df_description.R
## File Version: 0.03

mdmb_regression_est_df_description <- function(description, df, est_df=FALSE)
{
    if (est_df){
        des1 <- strsplit(description, split="(df=", fixed=TRUE)[[1]][1]
        description <- paste0( des1, "(df=", round(df,2), ")")
    }
    return(description)
}
