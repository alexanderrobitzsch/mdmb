## File Name: mdmb_regression_predict.R
## File Version: 0.221

#**** evaluate individual likelihood
mdmb_regression_predict <- function( Xdes, beta, offset_values, type, y,
        index_beta )
{
    np <- length(beta)
    beta1 <- beta[ index_beta ]
    if (type %in% c('oprobit','logistic') ){
        linear.predictor <- Xdes %*% beta1 + offset_values
    }
    if (type %in% c('yjt','bct') ){
        linear.predictor <- Xdes %*% beta1 + offset_values
    }
    fitted.values <- linear.predictor

    #**** logistic regression
    if (type=='logistic'){
        fitted.values <- stats::plogis(linear.predictor)
    }
    #*** include transformed values in yj transformation
    if (type=='yjt'){
        lam0 <- beta['lambda']
        fitted.values <- yj_antitrafo( y=linear.predictor, lambda=lam0)
    }
    if (type=='bct'){
        lam0 <- beta['lambda']
        fitted.values <- bc_antitrafo( y=linear.predictor, lambda=lam0)
    }
    #--- output
    res <- list( linear.predictor=linear.predictor,
                fitted.values=fitted.values)
    return(res)
}

