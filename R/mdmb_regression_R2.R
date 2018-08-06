## File Name: mdmb_regression_R2.R
## File Version: 0.23

#**** evaluate individual likelihood
mdmb_regression_R2 <- function( linear.predictor, y, type, beta, index_sigma, probit=FALSE )
{
    R2 <- NULL
    np <- length(beta)
    var_y_pred <- stats::var(linear.predictor)

    #***** logistic regression
    #  R2 form McKelvey and Zavoina
    if (type=="logistic"){
        var_resid <- 3.141593^2 / 3
    }
    if (type=="oprobit"){
        var_resid <- 1
    }

    #***** yjt regression
    if (type %in% c("yjt","bct") ){
        sigma <- beta[index_sigma]
        if (type=="yjt"){
            yt <- yj_trafo( y=y, lambda=beta[np], probit=probit )
        }
        if (type=="bct"){
            yt <- bc_trafo( y=y, lambda=beta[np] )
        }
        var_y <- stats::var( yt    )
        var_resid <- var_y - var_y_pred
    }
    #*** R2 formula
    R2 <- var_y_pred / ( var_y_pred + var_resid )
    #--- output
    return(R2)
}

