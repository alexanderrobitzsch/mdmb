## File Name: mdmb_dnorm.R
## File Version: 0.02

mdmb_dnorm <- function(x, mean, sd, log)
{
    if (log){
        dy <- mdmb_rcpp_log_dnorm( x=x, mu=mean, sigma=sd )
    } else {
        dy <- mdmb_rcpp_dnorm( x=x, mu=mean, sigma=sd )
    }
    return(dy)
}
