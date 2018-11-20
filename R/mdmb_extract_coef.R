## File Name: mdmb_extract_coef.R
## File Version: 0.02

mdmb_extract_coef <- function(mod)
{
    if ("coef" %in% names(mod) ){
        c1 <- mod$coef
    } else {
        c1 <- mod$coefficients
    }
    return(c1)
}
