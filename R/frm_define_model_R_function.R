## File Name: frm_define_model_R_function.R
## File Version: 0.696

frm_define_model_R_function <- function(model, use_grad=2, use_gibbs=FALSE,
    R_args=NULL, sampling_level=NULL, variable_level=NULL, maxiter=8, dat0=NULL )
{
    R_fct <- NULL
    R_density_fct <- NULL
    R_sampling_fct <- NULL
    use_gibbs_model <- FALSE
    if (is.null(sampling_level)){
        sampling_level <- NULL
    }
    if (is.null(variable_level)){
        variable_level <- NULL
    }
    if (is.null(R_args) ){
        R_args <- list()
    }
    if (! ( model$model %in% c('yjtreg') ) ){
        R_args$probit <- FALSE
    }
    #--- linear regression normal distribution
    if (model$model=='linreg'){
        R_fct <- stats::lm
        R_fct_name <- 'stats::lm'
        R_density_fct <- 'frm_linreg_density'
        R_sampling_fct <- 'frm_linreg_sample_parameters'
        use_gibbs_model <- use_gibbs
    }
    #--- logistic regression
    if (model$model=='logistic'){
        R_fct <- logistic_regression
        R_fct_name <- 'mdmb::logistic_regression'
        # R_args <- list('family'='binomial')
        R_args$use_grad <- use_grad
        R_density_fct <- 'frm_logistic_density'
        R_args$probit <- NULL
        R_args <- frm_define_model_R_function_include_maxiter(R_args=R_args,
                            maxiter=maxiter)
    }
    #--- ordinal probit regression
    if (model$model=='oprobit'){
        R_fct <- oprobit_regression
        R_fct_name <- 'mdmb::oprobit_regression'
        R_args$use_grad <- use_grad
        R_density_fct <- 'frm_oprobit_density'
        R_args$probit <- NULL
        R_args <- frm_define_model_R_function_include_maxiter(R_args=R_args,
                            maxiter=maxiter)
    }
    #--- linear regression with Box-Cox Transformation
    if (model$model %in% c('bctreg') ){
        R_fct <- bct_regression
        R_fct_name <- 'mdmb::bct_regression'
        R_args$use_grad <- use_grad
        R_density_fct <- 'frm_mdmb_regression_density'
        R_args$probit <- NULL
        R_args <- frm_define_model_R_function_include_maxiter(R_args=R_args,
                            maxiter=maxiter)
    }
    #--- linear regression with Yeo-Johnson Transformation
    if (model$model %in% c('yjtreg') ){
        R_fct <- yjt_regression
        R_fct_name <- 'mdmb::yjt_regression'
        R_args$use_grad <- use_grad
        R_density_fct <- 'frm_mdmb_regression_density'
        R_args <- frm_define_model_R_function_include_maxiter(R_args=R_args,
                            maxiter=maxiter)
        if (is.null(R_args$probit)){
            R_args$probit <- FALSE
        }
    }
    if (model$model %in% c('yjtreg', 'bctreg') ){
        if (is.null(R_args$est_df)){
            R_args$est_df <- FALSE
        }
    }
    #--- linear regression with multilevel regression
    if (model$model %in% c('mlreg') ){
        R_fct <- frm_mlreg_wrapper_ml_mcmc
        R_fct_name <- 'miceadds::ml_mcmc'
        R_args$probit <- NULL
        args <- list(outcome='normal', iter=5, burnin=1, inits_lme4=TRUE,
                        thresh_fac=5.8 )
        R_args <- frm_append_list( list1=R_args, list2=args, overwrite=FALSE)
        R_density_fct <- 'frm_mlreg_density'
        R_sampling_fct <- 'frm_mlreg_sample_parameters'
        use_gibbs_model <- TRUE
        if (!is.null(sampling_level)){
            cn <- colnames(dat0)
            if (!is.null(cn)){
                sl0 <- setdiff(sampling_level, cn)
                if (length(sl0) > 0){
                    stop(paste0('Sampling level \'', sl0, '\' not in data!\n') )
                }
            }
        }
    }
    #--- output
    res <- list( R_fct=R_fct, R_args=R_args, R_density_fct=R_density_fct,
                    R_fct_name=R_fct_name, use_gibbs_model=use_gibbs_model,
                    R_sampling_fct=R_sampling_fct, sampling_level=sampling_level,
                    variable_level=variable_level)
    return(res)
}
