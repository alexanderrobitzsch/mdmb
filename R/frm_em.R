## File Name: frm_em.R
## File Version: 0.972


frm_em <- function(dat, dep, ind, weights=NULL, verbose=TRUE,
    maxiter=500, conv_dev=1E-8, conv_parm=1E-5,
    nodes_control=c(11,5), h=1E-4, use_grad=2, update_model=NULL )
{
    CALL <- match.call()
    s1 <- Sys.time()

    #*** prepare models
    res <- frm_prepare_models(dep=dep, ind=ind, dat0=dat, nodes_control=nodes_control,
                    use_grad=use_grad)
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
    res2 <- frm_prepare_data_em(dat=dat, dep=dep, ind=ind, weights0=weights0,
                    dat0=dat0, update_model=update_model)
    dat <- res2$dat
    dat_resp <- res2$dat_resp
    dv_vars <- res2$dv_vars
    ind_resp <- res2$ind_resp
    ind_miss <- res2$ind_miss
    freq_miss_values <- res2$freq_miss_values
    # dat$weights <- dat$weights0 * dat$resp_all
    N2 <- nrow(dat)

    #*** prepare list of models
    NM <- attr(ind,'NM')
    if (is.null(update_model)){
        ind0 <- ind
        ind0[[ dep$dv_vars ]] <- dep
        ind0 <- frm_prepare_models_sigma_fixed( ind0=ind0, NM=NM, dat0=dat0, dat=dat )
        #*** add additional arguments for regression functions
        ind0 <- frm_prepare_models_design_matrices( ind0=ind0, dat=dat, NM=NM)
    } else {
        ind0 <- update_model$ind0
        weights0 <- update_model$weights0
    }

    iter <- 0
    conv <- FALSE
    ll_new <- 1E-500
    beta_new <- 0
    iterate <- TRUE
    conv1 <- conv2 <- FALSE

    #**** EM algorithm
    while( iterate ){
        ll_old <- ll_new
        beta_old <- beta_new
        res <- frm_em_calc_likelihood( dat=dat, ind0=ind0, NM=NM, iter=iter,
                    weights0=weights0, dat_resp=dat_resp, ind_resp=ind_resp,
                    ind_miss=ind_miss )
        ind0 <- res$ind0
        coefs <- res$coefs
        dat$weights <- res$post * dat$weights0
        like <- res$like
        beta_new <- res$coefs[[NM+1]]
        ll_new <- res$ll
        iter <- iter + 1

        #**** changes in parameters
        ll_change0 <- ( ll_new - ll_old ) / abs( ll_new)
        ll_change <- abs(ll_change0)
        ll_change_disp <- ll_new - ll_old

        if (iter==1 ){ ll_change_disp <- NA }
        beta_change <- max( abs( beta_new - beta_old ) )
        if(verbose){
            p1 <- paste0('*** Iter. ', iter, ' | ', 'LL ','=', ' ',
                    round(ll_new,3),
                    ' | LL change ', '=', ' ', round(ll_change_disp,6),
                    ' | Parm. change ', '=', ' ', round(beta_change,6),'\n'    )
            cat(p1)
            utils::flush.console()
        }
        if (iter >=maxiter){ iterate <- FALSE }
        if ( (ll_change < conv_dev) & (beta_change < conv_parm) ){
            iterate <- FALSE
        }
        if (ll_change < conv_dev){
            conv1 <- TRUE
            iterate <- FALSE
        }
        if (beta_change < conv_parm){ conv2 <- TRUE}
        conv <- conv1 & conv2

    }
    #***************************************
#    cat('\n* EM algorithm ') ; zz1 <- Sys.time(); print(zz1-zz0) ; zz0 <- zz1

    if (verbose){
        cat('--- Compute asymptotic covariance matrix')
        utils::flush.console()
    }

    #--- computation asymptotic covariance matrix
    res0 <- frm_em_avcov(res=res, dat=dat, ind0=ind0, NM=NM, h=h)

    if (verbose){
        cat('\n')
        utils::flush.console()
    }

    model_results <- res$model_results
    coefs <- res0$partable$est
    names(coefs) <- res0$partable$parm
    vcovs <- res0$avcov
    rownames(vcovs) <- colnames(vcovs) <- names(coefs)
    ses <- 0*coefs + res0$partable$se

    #--- information criteria
    ic <- frm_em_ic( ll_new=ll_new, N=N, ind0=ind0,
                    model_results=model_results )

    #--- descriptive statistics
    desc_vars <- frm_descriptives_variables(dat=dat, predictorMatrix=predictorMatrix,
                        freq_miss_values=freq_miss_values, dat0=dat0)

    #--- output
    s2 <- Sys.time()
    res <- list( coef=coefs, vcov=vcovs, partable=res0$partable,
                all_coefs=res$coefs, ll=ll_new, like=like, dat=dat, se=ses,
                info=res0$info, conv=conv, iter=iter, ic=ic,
                ind0=ind0, predictorMatrix=predictorMatrix,
                variablesMatrix=variablesMatrix, desc_vars=desc_vars,
                model_results=res$model_results, like0=res$like0,
                freq_miss_values=freq_miss_values, weights0=weights0,
                CALL=CALL, s1=s1, s2=s2    , diff_time=s2-s1    )
    class(res) <- 'frm_em'
    return(res)
}

# z0 <- TAM:::tamcat('calc_likelihood',z0,TRUE)
