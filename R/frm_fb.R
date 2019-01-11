## File Name: frm_fb.R
## File Version: 0.7938

### Factored regression model
### Fully Bayesian estimation
frm_fb <- function(dat, dep, ind, weights=NULL, verbose=TRUE,
            data_init=NULL, iter=500, burnin=100, Nimp=10,
            Nsave=3000, refresh=25, acc_bounds=c(.45,.50),
            print_iter=10, use_gibbs=TRUE, aggregation=TRUE )
{
    CALL <- match.call()
    s1 <- Sys.time()

    #*****************----------------
    # adaptation of function for including new model classes
    #
    # @ frm_prepare_models()
    #        * subfunction frm_define_model_R_function()
    #            - define sampling functions for parameters and evaluation
    #              function for density in Metropolis-Hastings step
    #                o R_fct <- stats::lm
    #                o R_fct_name <- "stats::lm"
    #                o R_density_fct <- "frm_linreg_density"
    #                o R_sampling_fct <- "frm_linreg_sample_parameters"
    #
    # @ frm_prepare_data_fb()
    #        * no adaptation necessary, these are just processing functions
    #         for missing dat
    #
    # @ frm_fb_initial_parameters()
    #        * definition of standard deviations for proposal distribution
    #         * definition of coef of sampled parameters
    #            o ind_mm$coef <- coef(mod)
    #            o ind_mm$coef_sd_proposal <- se_mod
    #            o ind_mm$coef_parnames <- ...
    #        * in this step, the model is estimated. R_args for function arguments
    #             R_args <- list( formula=ind_mm$formula, data=dat, weights=weights)
    #            R_args <- frm_append_list(list1=R_args, list2=ind_mm$R_args)
    #            mod <- do.call( what=ind_mm$R_fct, args=R_args )
    #        * parms and parms_index contain parameter names and parameter indices
    #
    #
    #
    # @ frm_fb_init_matrices_saved_parameters()
    #        * no adaptation is necessary, matrices are created based on
    #         parms and parms_index
    #
    # @ frm_fb_init_imputations()
    #        * no adaptations are needed
    #
    #*****************----------------

    #*** prepare models
    res <- frm_prepare_models(dep=dep, ind=ind, dat0=dat, nodes_weights=FALSE, use_gibbs=use_gibbs,
                weights=weights)
    dep <- res$dep
    ind <- res$ind
    predictorMatrix <- res$predictorMatrix
    variablesMatrix <- res$variablesMatrix
    weights0 <- weights
    N <- nrow(dat)
    if ( is.null(weights0)){
        weights0 <- rep(1,N)
    }
    dat0 <- dat

    #*** prepare data
    res2 <- frm_prepare_data_fb(dat=dat, dep=dep, ind=ind, weights0=weights0,
                    dat0=dat0, data_init=data_init )
    dat <- res2$dat
    dat_resp <- res2$dat_resp
    dv_vars <- res2$dv_vars
    ind_resp <- res2$ind_resp
    ind_miss <- res2$ind_miss
    freq_miss_values <- res2$freq_miss_values
    impute_vars <- res2$impute_vars
    impute_vars_index <- res2$impute_vars_index
    N2 <- nrow(dat)

    #*** prepare list of models
    NM <- attr(ind,"NM")
    ind0 <- ind
    ind0[[ dep$dv_vars ]] <- dep

    #*** fixed standard deviations
    ind0 <- frm_prepare_models_sigma_fixed( ind0=ind0, NM=NM, dat0=dat0, dat=dat )

    #*** initial estimation of models
    res3 <- frm_fb_initial_parameters(dat=dat, ind0=ind0, data_init=data_init,
                ind_miss=ind_miss)
    ind0 <- res3$ind0
    parms <- res3$parms
    parms_index <- res3$parms_index
    model_results <- res3$model_results
    npars <- res3$npars
    dat <- res3$dat

    #**** allocate matrices with sampled values for parameters
    parms_mcmc <- frm_fb_init_matrices_saved_parameters( iter=iter, burnin=burnin,
                        Nsave=Nsave, Nimp=Nimp, npars=npars, parms=parms,
                        parms_index=parms_index, predictorMatrix=predictorMatrix )
    iter <- parms_mcmc$iter
    burnin <- parms_mcmc$burnin

    #**** inits objects for imputations
    imputations_mcmc <- frm_fb_init_imputations( Nimp=Nimp, model_results=model_results,
                            iter=iter, burnin=burnin, impute_vars=impute_vars,
                            impute_vars_index=impute_vars_index, ind_miss=ind_miss,
                            dv_vars=dv_vars, ind0=ind0, variablesMatrix=variablesMatrix,
                            dat=dat)

    #*** add additional arguments for regression functions
    # ind0 <- frm_prepare_models_design_matrices( ind0=ind0, dat=dat, NM=NM)
    maxiter <- iter
    iter <- 1
    iterate <- TRUE
    mcmc_start_time <- Sys.time()

zz0 <- Sys.time()

    #**** MCMC algorithm
    while( iterate ){

        #*** sample model parameters
        res <- frm_fb_sample_parameters( dat=dat, ind0=ind0, NM=NM, iter=iter,
                    weights0=weights0, dat_resp=dat_resp, ind_resp=ind_resp,
                    ind_miss=ind_miss, model_results=model_results,
                    parms_mcmc=parms_mcmc )
        ind0 <- res$ind0
        model_results <- res$model_results
        parms_mcmc <- res$parms_mcmc

        #*** imputation of missing values
        res <- frm_fb_sample_imputed_values( imputations_mcmc=imputations_mcmc,
                    model_results=model_results, ind0=ind0, iter=iter, dat=dat,
                    aggregation=aggregation )
        imputations_mcmc <- res$imputations_mcmc
        dat <- res$dat

        #*** refreshing proposal SD for parameters and imputed values
        if ( ( iter %% refresh==0 ) & ( iter <=burnin ) ){
            ind0 <- frm_fb_mh_refresh_parameters( ind0=ind0, acc_bounds=acc_bounds )
            imputations_mcmc <- frm_fb_mh_refresh_imputed_values( imputations_mcmc=imputations_mcmc,
                                    acc_bounds=acc_bounds, ind0=ind0 )
            res4 <- frm_fb_verbose_mh_refresh( verbose=verbose, iter=iter )
        }

        #**** print progress
        res4 <- frm_fb_verbose_iterations( verbose=verbose, iter=iter,
                        print_iter=print_iter, maxiter=maxiter, mcmc_start_time=mcmc_start_time )
        if (iter >=maxiter){ iterate <- FALSE }
        iter <- iter + 1

    }
    #***************************************

# cat("\n* MCMC algorithm ") ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

    if (verbose){
        cat("\n")
        utils::flush.console()
    }

    #--- create parameter table
    res <- frm_fb_partable( ind0=ind0, parms_mcmc=parms_mcmc )
    partable <- res$partable
    tech_summary <- res$tech_summary
    coefs <- res$coef
    vcovs <- res$vcov
    values_coda <- res$values_coda

    #--- information criteria
    ic <- list()
    ic$N <- nrow(dat0)
    ic$np <- nrow(partable)
    #--- descriptive statistics
    desc_vars <- frm_fb_descriptives_variables(dat=dat, predictorMatrix=predictorMatrix,
                        freq_miss_values=freq_miss_values, dat0=dat0, parms_mcmc=parms_mcmc )

    #--- output
    s2 <- Sys.time()
    res <- list( coef=coefs, vcov=vcovs, partable=partable, tech_summary=tech_summary,
                values_coda=values_coda, ic=ic, ind0=ind0, parms_mcmc=parms_mcmc,
                imputations_mcmc=imputations_mcmc, predictorMatrix=predictorMatrix,
                variablesMatrix=variablesMatrix, desc_vars=desc_vars, model_results=model_results,
                ind0=ind0, dat=dat0, freq_miss_values=freq_miss_values,    iter=maxiter, burnin=burnin,
                CALL=CALL, s1=s1, s2=s2    , diff_time=s2-s1 )
    class(res) <- "frm_fb"
    return(res)
}
