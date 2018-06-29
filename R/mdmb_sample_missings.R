## File Name: mdmb_sample_missings.R
## File Version: 0.01

mdmb_sample_missings <- function(data, vars=NULL)
{
    if (is.null(vars)){
        vars <- colnames(data)
    }
    VV <- length(vars)
    for (vv in 1:VV){
        var_vv <- vars[vv]
        dvv <- data[, var_vv]
        ind_vv <- is.na(dvv)
        svv <- sum(ind_vv)
        if ( svv > 0 ){
            data[ ind_vv ,var_vv] <- sample( dvv[ ! ind_vv ], size = svv, replace=TRUE )
        }        
    }
    return(data)
}
