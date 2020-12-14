## File Name: frm_fb_sample_imputed_values_proposal.R
## File Version: 0.577


frm_fb_sample_imputed_values_proposal <- function( var_vv, index_vv,
        ind0, imputations_mcmc, N_vv, imp, dat_vv, model_results, ind_miss_vv )
{
    mh_vv <- imputations_mcmc$mh_imputations_values[[ var_vv ]]
    ind0_vv <- ind0[[index_vv]]
    model_vv <- ind0[[ index_vv ]]$model
    imp1 <- NULL
    do_mh <- FALSE
    gibbs_values <- NULL
    NG <- NULL
    changed1 <- TRUE
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
        eps <- 1E-3
        if ( model_vv=="bctreg"){
            imp1 <- ifelse( imp1 < 0, imp, imp1 )
            changed1 <- ( imp1 !=imp )
        }
        if ( model_vv=="yjtreg"){
            if ( ind0[[index_vv]]$R_args$probit){
                imp1 <- ifelse( imp1 < 0, imp, imp1 )
                imp1 <- ifelse( imp1 > 1, imp, imp1 )
            }
            changed1 <- ( imp1 !=imp )
        }
        do_mh <- TRUE
    }
    #*** sample new values for linear regression
    if ( model_vv %in% c( "mlreg" ) ){
        outcome <- ind0_vv$R_args$outcome
        if (outcome=="probit"){
            imp <- model_results$y[ ind_miss_vv,1]
        }
        imp1 <- stats::rnorm( N_vv, mean=imp, sd=mh_vv$sd_proposal )
        if (outcome=="probit"){
            alpha <- model_results$alpha[,1]
            imp1 <- mdmb_discretize(x=imp, alpha=alpha )
        }
        do_mh <- TRUE
    }
    #--- set missing values equal within a cluster in case of imputation at higher level
    ind0_vv <- ind0[[ index_vv ]]
    if ( ind0_vv$use_variable_level ){
        variable_info <- ind0_vv$variable_info
        imp1 <- imp1[ variable_info$replace_miss_id ]
    }
    #--- output
    res <- list( imp1=imp1, do_mh=do_mh, gibbs_values=gibbs_values, NG=NG,
                    changed1=changed1)
    return(res)
}
