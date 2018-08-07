## File Name: mdmb_regression_extract_parameters.R
## File Version: 0.12

mdmb_regression_extract_parameters <- function( mod, parnames, type,
    is_lambda_fixed, lambda_fixed)
{
    beta <- mod$par
    if ( ( type %in% c("yjt","bct") ) & ( is_lambda_fixed ) ){
        beta <- c( beta, lambda_fixed )
        parnames <- c(parnames, "lambda" )
    }
    names(beta) <- parnames
    return(beta)
}
