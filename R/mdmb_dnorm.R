## File Name: mdmb_dnorm.R
## File Version: 0.16

mdmb_dnorm <- function(x, mu, sigma, log)
{
    if (is.matrix(mu) ){
        mu <- mu[,1]
    }
    if ( length(mu) > 1){
        if (log){
            dy <- mdmb_rcpp_log_dnorm( x=x, mu=mu, sigma=sigma )
        } else {
            dy <- mdmb_rcpp_dnorm( x=x, mu=mu, sigma=sigma )
        }
    } else {
        if (log){
            dy <- mdmb_rcpp_log_dnorm_double( x=x, mu=mu, sigma=sigma )
        } else {
            dy <- mdmb_rcpp_dnorm_double( x=x, mu=mu, sigma=sigma )
        }
    }
    return(dy)
}
