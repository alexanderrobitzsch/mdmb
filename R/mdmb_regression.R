## File Name: mdmb_regression.R
## File Version: 1.923


mdmb_regression <- function( formula, data, type, weights=NULL,
    beta_init=NULL, beta_prior=NULL, df=Inf, lambda_fixed=NULL, probit=FALSE,
    est_df=FALSE, use_grad=2, h=1E-4, control=NULL, control_optim_fct=NULL )
{
    CALL <- match.call()
    s1 <- Sys.time()
    res <- remove_NA_data_frame( formula=formula, data=data, weights=weights)
    data <- res$data
    weights <- res$weights
    Xdes <- stats::model.matrix( object=formula, data=data )
    N <- nrow(Xdes)

    # control arguments control_optim_fct
    control_optim_fct <- mdmb_regression_proc_control_optim_fct(control_optim_fct=control_optim_fct)

    index_beta <- NULL
    index_thresh <- NULL

    offset_values <- offset_values_extract(formula=formula, data=data )
    Ndes <- ncol(Xdes)
    parnames <- colnames(Xdes)
    if (type %in% c("yjt","bct") ){
        parnames <- c( parnames, "sigma", "lambda" )
    }

    dv_var <- as.character( formula[[2]] )
    y <- data[,dv_var]
    #*** lambda fixed
    is_lambda_fixed <- TRUE
    if (  ( type %in% c("yjt","bct") ) & ( is.null( lambda_fixed ) ) ){
        is_lambda_fixed <- FALSE
    }
    #-- probit model
    if ( type %in% "oprobit"){
        K <- max(y, na.rm=TRUE)
        t1 <- cumsum( prop.table(table(y)) )
        thresh_init <- stats::qnorm( t1[ - c(K+1) ] )
        thresh_init <- thresh_init - thresh_init[1]
        thresh_init <- thresh_init[-1]
        thresh_init_diff <- c(thresh_init[1], diff(thresh_init) )
        thresh_init <- log( thresh_init_diff )
        if (K>=2){
            names(thresh_init) <- paste0("difflogthresh",2:K)
        } else {
            thresh_init <- NULL
        }
    }

    #--- starting values parameters
    if ( is.null(beta_init) ){
        par <- rep(0,Ndes)
        y1 <- y
        if (probit){
            y1 <- stats::qlogis(y1)
        }
        if (use_grad %in% c(1,2) ){
            mod <- stats::lm.wfit( y=y1, x=Xdes, w=weights)
            par0 <- mod$coefficients
            sd_y <- mdmb_weighted_sd( x=mod$residuals, w=weights )
            par <- par0
        } else {
            sd_y <- mdmb_weighted_sd( x=y1, w=weights )
        }
        #** starting values yjt and bct regression
        if ( type %in% c("yjt","bct") ){
            if ( is_lambda_fixed ){
                par <- c( par, sigma=sd_y  )
            } else {
                par <- c( par, sigma=sd_y, lambda=1 )
            }
            if (est_df){
                df0 <- df
                if (df0==Inf){
                    df0 <- 30
                }
                par <- c(par, logdf=log(df0) )
            }
        }
        #** ordinal probit model
        if ( type %in% c("oprobit") ){
            par <- c( par0, thresh_init )
        }
    }

    if (type %in% c("oprobit") ){
        if (Ndes > 0){
            index_beta <- 1:Ndes
        } else {
            index_beta <- NULL
        }
        if (K>=2){
            index_thresh <- Ndes + seq(1,length(thresh_init))
        } else {
            index_thresh <- NULL
        }
    }

    if ( ! is.null(beta_init) ){
        par <- beta_init
    }

    is_prior <- FALSE
    if ( ! is.null( beta_prior ) ){
        is_prior <- TRUE
    }

    # define upper bound for df
    upper <- rep(Inf, length(par))
    lower <- rep(-Inf, length(par))
    names(upper) <- names(lower) <- names(par)
    if (est_df){
        upper[length(par)] <- 6.000
    }

    x <- par
    parnames <- names(par)
    np <- length(x)
    eps <- 1E-50
    index_sigma <- NULL
    index_lambda <- NULL
    index_df <- NULL

    #-----------------------------------------
    #---- define optimization function

    #***********************************
    #*** logistic regression
    if ( type=="logistic"){
        description <- "Logistic regression"
        index_beta <- 1:np
        #--- optimization function
        fct_optim <- function(x){
            ypred <- Xdes %*% x + offset_values
            ll_i <- mdmb_regression_logistic_density( y=y, ypred=ypred, log=TRUE, eps=eps )
            ll <- - sum( weights * ll_i )
            #--- include prior distributions
            if (is_prior){
                ll <- ll - eval_prior_list_sumlog( par=x, par_prior=beta_prior, use_grad=use_grad )
            }
            #--- output
            return(ll)
        }
        #--- gradient
        grad_optim2 <- function(x){
            ypred <- Xdes %*% x + offset_values
            ll0 <- mdmb_regression_logistic_density( y=y, ypred=ypred, log=TRUE, eps=eps )
            ll1 <- mdmb_regression_logistic_density( y=y, ypred=ypred+h, log=TRUE, eps=eps )
            der1 <- - mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=h)
            wder1 <- weights * der1
            xgrad <- colSums( wder1 * Xdes )
            if ( is_prior ){
                xgrad <- xgrad - eval_prior_list_gradient_log( par=par, par_prior=beta_prior, h=h )
            }
            return(xgrad)
        }
    }
    #***********************************
    #*** yjt regression
    if ( type %in% c("yjt","bct") ){
        np1 <- np
        if (est_df){
            np1 <- np - 1
            index_df <- np
        }
        index_beta <- seq(1,np1-2 + is_lambda_fixed)
        index_sigma <- np1 - 1 + is_lambda_fixed
        if ( ! is_lambda_fixed){
            index_lambda <- np1
        }
        eps_shape <- .01
        if (type=="yjt"){
            if (probit){
                description <- paste0("Scaled t regression with Probit Yeo-Johnson transformation (df=", df, ")")
            } else {
                description <- paste0("Scaled t regression with Yeo-Johnson transformation (df=", df, ")")
            }
            dens_fct <- dyjt_scaled
        }
        if (type=="bct"){
            description <- paste0( "Scaled t regression with Box-Cox transformation (df=", df, ")")
            dens_fct <- dbct_scaled_mdmb_regression_wrapper
        }

        #--- define optimization function
        fct_optim <- function(x){
            ll <- mdmb_regression_optim_yjt_fct( x=x, index_beta=index_beta, eps_shape=eps_shape,
                        index_sigma=index_sigma, lambda_fixed=lambda_fixed, is_lambda_fixed=is_lambda_fixed,
                        index_lambda=index_lambda, index_df=index_df, est_df=est_df,
                        Xdes=Xdes, offset_values=offset_values, y=y, df=df,
                        probit=probit, weights=weights, is_prior=is_prior, beta_prior=beta_prior,
                        use_grad=use_grad, dens_fct=dens_fct )
            return(ll)
        }
        #--- gradient
        grad_optim2 <- function(x){
            xgrad <- mdmb_regression_optim_yjt_grad( x=x, index_beta=index_beta,
                            eps_shape=eps_shape, index_sigma=index_sigma, lambda_fixed=lambda_fixed,
                            is_lambda_fixed=is_lambda_fixed, index_lambda=index_lambda,
                            index_df=index_df, est_df=est_df, Xdes=Xdes,
                            offset_values=offset_values, y=y, df=df, probit=probit, weights=weights, is_prior=is_prior,
                            beta_prior=beta_prior, use_grad=use_grad, dens_fct=dens_fct, np=np, h=h )
            return(xgrad)
        }
    }
    #***********************************
    #*** ordinal probit model
    if ( type=="oprobit"){
        description <- "Ordinal Probit Regression"
        NT <- length(index_thresh)

        #--- optimization function
        fct_optim <- function(x){
            ll <- mdmb_regression_optim_oprobit_fct( x=x, index_beta=index_beta,
                        index_thresh=index_thresh, Xdes=Xdes, offset_values=offset_values, y=y,
                        eps=eps, weights=weights )
            return(ll)
        }
        #--- gradient
        grad_optim2 <- function(x){
            xgrad <- mdmb_regression_optim_oprobit_grad( x=x, index_beta=index_beta,
                        index_thresh=index_thresh, Xdes=Xdes, offset_values=offset_values, y=y, eps=eps, h=h,
                        weights=weights, NT=NT, use_rcpp_deriv_ypred=control_optim_fct$use_rcpp_deriv_ypred,
                        use_rcpp_deriv_logthresh=control_optim_fct$use_rcpp_deriv_logthresh )
            return(xgrad)
        }
    }
    #--- compute gradient
    grad_optim <- function(x){
        xh <- CDM::numerical_Hessian( par=x, FUN=fct_optim, gradient=TRUE, hessian=FALSE)
        return(xh)
    }

    if ( use_grad==0){
        grad_optim <- NULL
    }
    if ( use_grad==2){
        grad_optim <- grad_optim2
    }

    coef_init <- par
    #* lower bounds
    if ( type %in% c("yjt","bct") ){
        lower[index_sigma] <- 0
        if (est_df){
            lower[index_df] <- log(1.5)
        }
        if (! is.null(index_lambda) ){
            upper["lambda"] <- 5
        }
    }

    #-------------------------------------------
    #---- optimization using optim
    mod1 <- stats::optim( par=par, fn=fct_optim, gr=grad_optim, method="L-BFGS-B",
                hessian=TRUE, lower=lower, upper=upper, control=control)
    result_optim <- mod1

    #--- extract parameters
    beta <- mdmb_regression_extract_parameters( mod=mod1, parnames=parnames,
                type=type, is_lambda_fixed=is_lambda_fixed, lambda_fixed=lambda_fixed )

    #--- degrees of freedom
    df <- mdmb_compute_df(x=beta, est_df=est_df, df=df)

    #--- thresholds
    thresh <- NULL
    if (type=="oprobit"){
        thresh <- c( 0, logthresh_2_thresh(x=beta[index_thresh] ) )
        names(thresh) <- paste0("thresh", 1:K)
    }

    #--- extract log-likelihood, log prior and log-posterior
    res0 <- mdmb_regression_loglike_logpost(mod=mod1, beta=beta,
                beta_prior=beta_prior, is_prior=is_prior, type=type,
                is_lambda_fixed=is_lambda_fixed)
    loglike <- res0$loglike
    logprior <- res0$logprior
    logpost <- res0$logpost
    hessian <- res0$hessian
    vcov1 <- res0$vcov1

    #--- predictions
    res0 <- mdmb_regression_predict( Xdes=Xdes, beta=beta, offset_values=offset_values, type=type,
                    y=y, index_beta=index_beta )
    linear.predictor <- res0$linear.predictor
    fitted.values <- res0$fitted.values

    #---- individual likelihood
    loglike_case <- mdmb_regression_loglike_case(y=y, linear.predictor=linear.predictor,
                        fitted.values=fitted.values, type=type, beta=beta, df=df,
                        index_beta=index_beta, index_thresh=index_thresh)

    #---- information criteria
    deviance <- - 2 * loglike
    ic <- mdmb_regression_ic( N=N, beta=beta, deviance=deviance, type=type,
                index_beta=index_beta, index_sigma=index_sigma, index_lambda=index_lambda,
                index_thresh=index_thresh, index_df=index_df )

    #---- summary table
    partable <- mdmb_regression_summary_table( beta=beta, vcov1=vcov1 )

    #--- description
    description <- mdmb_regression_est_df_description(description=description, df=df,
                        est_df=est_df)

    #---- calculate R^2
    R2 <- mdmb_regression_R2( linear.predictor=linear.predictor, y=y,
                type=type, beta=beta, index_sigma=index_sigma,
                index_lambda=index_lambda, probit=probit )
    s2 <- Sys.time()

    #--- output
    res <- list(coefficients=beta, vcov=vcov1, partable=partable, y=y, X=Xdes, weights=weights,
            fitted.values=fitted.values, linear.predictor=linear.predictor,
            hessian=hessian, coef_init=coef_init, loglike=loglike, deviance=deviance, logprior=logprior, logpost=logpost,
            like_case=loglike_case, ic=ic, formula=formula, offset_values=offset_values,
            thresh=thresh, R2=R2, parnames=parnames, beta_prior=beta_prior, df=df,
            index_beta=index_beta, index_sigma=index_sigma, index_lambda=index_lambda,
            index_thresh=index_thresh, index_df=index_df, est_df=est_df,
            is_prior=is_prior, fct_optim=fct_optim, type=type,
            CALL=CALL, converged=mod1$converged, result_optim=result_optim, probit=probit,
            coef=beta, iter=mod1$counts['function'], description=description, s1=s1, s2=s2, diff_time=s2-s1
            )
    class(res) <- "mdmb_regression"
    return(res)
}
