## File Name: frm_fb_sample_imputed_values_proposal.R
## File Version: 0.32


frm_fb_sample_imputed_values_proposal <- function( var_vv, index_vv,
        ind0, imputations_mcmc, N_vv, imp, dat_vv )
{
    mh_vv <- imputations_mcmc$mh_imputations_values[[ var_vv ]]
    ind0_vv <- ind0[[index_vv]]
    model_vv <- ind0[[ index_vv ]]$model
    imp1 <- NULL
    do_mh <- FALSE
    gibbs_values <- NULL
    NG <- NULL
    #*** sample new values for linear regression
    if ( model_vv %in% c( "linreg" ) ){
        imp1 <- stats::rnorm( N_vv, mean=imp, sd=mh_vv$sd_proposal )
        do_mh <- TRUE
    }
    #*** sample new values for logistic regression
    if ( model_vv %in% c( "logistic" ) ){
        gibbs_values <- 0:1
        imp1 <- dat_vv[,var_vv]
        NG <- length(gibbs_values)
    }
    #*** sample new values for logistic regression
    if ( model_vv %in% c( "oprobit" ) ){
        imp1 <- dat_vv[,var_vv]
        gibbs_values <- as.numeric( names(table(imp1) ) )
        NG <- length(gibbs_values)
    }
    #*** sample new values for yjtreg and bctreg regression
    if ( model_vv %in% c( "bctreg", "yjtreg" ) ){
        imp1 <- stats::rnorm( N_vv, mean=imp, sd=mh_vv$sd_proposal )
        if ( model_vv=="bctreg"){
            imp1 <- ifelse( imp1 < 0, imp, imp1 )
        }
        do_mh <- TRUE
    }
    #*** sample new values for linear regression
    if ( model_vv %in% c( "mlreg" ) ){
        outcome <- ind0_vv$R_args$outcome
        if (outcome=="probit"){
            imp1 <- dat_vv[,var_vv]
            gibbs_values <- seq(0, max(imp1) )
            NG <- length(gibbs_values)
        }
        if (outcome=="normal"){
            imp1 <- stats::rnorm( N_vv, mean=imp, sd=mh_vv$sd_proposal )
            do_mh <- TRUE
        }
    }
    res <- list( imp1=imp1, do_mh=do_mh, gibbs_values=gibbs_values, NG=NG )
    return(res)
}
