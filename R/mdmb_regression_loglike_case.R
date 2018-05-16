## File Name: mdmb_regression_loglike_case.R
## File Version: 0.14

#**** evaluate individual likelihood
mdmb_regression_loglike_case <- function(y, linear.predictor, 
    fitted.values, type , beta, df, index_beta=NULL , index_thresh=NULL )
{
    np <- length(beta)
    #**********************
    # logistic regression
    if (type=="logistic"){
        loglike_case <- ifelse( y == 1 , fitted.values , 1 - fitted.values )
    }
    #**********************
    # ordinal probit model
    if (type=="oprobit"){    
        thresh <- logthresh_2_thresh(x=beta[ index_thresh ])
        loglike_case <- mdmb_regression_oprobit_density( y=y, ypred=linear.predictor, 
                            thresh=thresh, log=FALSE )    
    }    
    
    #**********************
    # yjt regression
    if (type=="yjt"){
        sigma <- beta[ np-1 ]
        lambda <- beta[ np ]
        loglike_case <- dyjt_scaled( y , location=linear.predictor , shape = sigma ,
                            lambda = lambda , df = df )
    }
    #**********************
    # bct regression
    if (type=="bct"){
        sigma <- beta[ np-1 ]
        lambda <- beta[ np ]
        loglike_case <- dbct_scaled( y , location=linear.predictor , shape = sigma ,
                            lambda = lambda , df = df )
    }    
    #------------------------------------
    #--- log-likelihood
    eps <- 1E-50
    loglike_case <- log( loglike_case + eps )
    #------------------------------------
    return(loglike_case)
}
    
