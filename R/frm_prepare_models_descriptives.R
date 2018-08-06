## File Name: frm_prepare_models_descriptives.R
## File Version: 0.01

frm_prepare_models_descriptives <- function(y)
{
    y <- y[ ! is.na(y) ]
    if ( length(y) > 0 ){
        m0 <- mean( y, na.rm=TRUE )
        sd0 <- stats::sd( y, na.rm=TRUE )
    } else {
        m0 <- 0
        sd0 <- 1
    }
    #--- output
    res <- list(m0=m0, sd0=sd0)
    return(res)
}
