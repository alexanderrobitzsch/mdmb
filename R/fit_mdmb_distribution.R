## File Name: fit_mdmb_distribution.R
## File Version: 0.43

fit_mdmb_distribution <- function(x, type, df=Inf, lambda_fixed=NULL, par_init=NULL,
    weights=NULL)
{
    CALL <- match.call()
    s1 <- Sys.time()
    #--- remove missings
    res0 <- fit_mdmb_distribution_remove_NA( x=x, weights=weights )
    x <- res0$x
    weights <- res0$weights
    is_lambda_fixed <- TRUE

    y <- x
    eps <- 1E-50

    #--- some descriptive statistics for inits
    m0 <- mean(x)
    sd0 <- stats::sd(x)

    #***************************************************
    #******* scaled t distribution
    if ( type=="t_scaled"){
        # initial values for parameters
        x0 <- c( m0, sd0 )
        parnames <- c("location","scale")
        description <- paste0( "Scaled t distribution (df=", df, ")")
        class_type <- "fit_t_scaled"
        loglik_fit <- function(x){
            dx <- dt_scaled( x=y, location=x[1], shape=x[2], df=df )
            fn <- - sum( weights * log(dx + eps ) )
            return(fn)
        }
    }
    #***************************************************
    #******* scaled t distribution with Yeo-Johnson transformation
    if ( type=="yjt_scaled"){
        if ( is.null(lambda_fixed) ){
            is_lambda_fixed <- FALSE
        }
        # initial values for parameters
        x0 <- c( m0, sd0, 1)
        if (is_lambda_fixed){
            x0 <- x0[1:2]
        }
        parnames <- c("location","scale","lambda")
        description <- paste0(
            "Scaled t distribution with Yeo-Johnson transformation (df=", df, ")")
        class_type <- "fit_yjt_scaled"
        loglik_fit <- function(x){
            if ( is_lambda_fixed ){
                lambda1 <- lambda_fixed
            } else {
                lambda1 <- x[3]
            }
            dx <- dyjt_scaled( x=y, location=x[1], shape=x[2],
                    lambda=lambda1, df=df )
            fn <- - sum( weights * log(dx + eps ) )
            return(fn)
        }
    }
    #***************************************************
    #******* scaled t distribution with Box-Cox transformation
    if ( type=="bct_scaled"){
        if ( is.null(lambda_fixed) ){
            is_lambda_fixed <- FALSE
        }
        # initial values for parameters
        x0 <- c( m0, sd0, 1)
        if (is_lambda_fixed){
            x0 <- x0[1:2]
        }
        parnames <- c("location","scale","lambda")
        description <- paste0(
            "Scaled t distribution with Box-Cox transformation (df=", df, ")")
        class_type <- "fit_bct_scaled"
        loglik_fit <- function(x){
            if ( is_lambda_fixed ){
                lambda1 <- lambda_fixed
            } else {
                lambda1 <- x[3]
            }
            dx <- dbct_scaled( x=y, location=x[1], shape=x[2],
                    lambda=lambda1, df=df )
            fn <- - sum( weights * log(dx + eps ) )
            return(fn)
        }
    }
    #***************************************************
    #******* ordinal probit model
    if ( type=="oprobit"){
        # initial values for parameters
        freq <- cumsum( table(y) ) / length(y)
        x0 <- stats::qnorm( freq[ - length(freq) ] )
        K <- length(x0)
        df <- NULL
        parnames <- paste0( "thresh", 1:K)
        description <- paste0( "Ordinal Probit Model")
        class_type <- "fit_oprobit"
        loglik_fit <- function(x){
            dx <- doprobit( x=y, thresh=x )
            fn <- - sum( weights * log(dx + eps ) )
            return(fn)
        }
    }
    #***************************************************

    if ( ! is.null(par_init) ){
        x0 <- par_init
    }
    np <- length(x0)

    #--- optimization
    res0 <- stats::optim( par=x0, fn=loglik_fit, hessian=TRUE )

    #--- collect results
    res0 <- fit_mdmb_distribution_extract_results(res0=res0, lambda_fixed=lambda_fixed,
                is_lambda_fixed=is_lambda_fixed, parnames=parnames)
    coefs <- res0$coefs
    vcovs <- res0$vcovs
    loglike <- res0$loglike
    deviance <- res0$deviance
    N <- length(y)
    partable <- fit_mdmb_distribution_summary_table( beta=coefs, vcov1=vcovs )
    s2 <- Sys.time()
    #--- output
    res <- list( coef=coefs, vcov=vcovs, loglike=loglike,
                deviance=deviance,
                partable=partable, np=np, N=N, lambda_fixed=lambda_fixed,
                is_lambda_fixed=is_lambda_fixed,
                df=df, x=y, weights=weights, CALL=CALL,
                description=description, type=type,
                s1=s1, s2=s2 )
    class(res) <- class_type
    return(res)
}
