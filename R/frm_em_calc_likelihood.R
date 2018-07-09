## File Name: frm_em_calc_likelihood.R
## File Version: 1.357

#--- loop over models and predictions
frm_em_calc_likelihood <- function( dat, ind0, NM, eps=1E-30, iter=NULL,
        weights0=NULL, dat_resp, ind_resp, ind_miss )
{
    weights <- dat$weights
    N2 <- nrow(dat)
    loglike <- matrix(NA, nrow=N2, ncol=NM+1)
    like0 <- loglike
    post0 <- loglike
    model_results <- NULL
    post <- 1 + 0*dat$weights
    like <- post
    like_obs <- post
    post_miss <- post
    coefs <- as.list( 1:(NM+1) )
    for (mm in 1:(NM+1)){
        ind_mm <- ind0[[mm]]
        #--- estimate model with weights
        mod <- frm_em_calc_likelihood_estimate_model( ind_mm=ind_mm, dat=dat,
                    weights=weights )
        model_results[[mm]] <- mod
        model_results <- frm_em_include_coef_inits( ind=ind0, mm=mm,
                model_results=model_results, iter=iter)
        model_results[[mm]]$est_sigma <- FALSE
        if ( ! is.null(ind_mm$sigma_fixed)){
            model_results[[mm]]$sigma <- ind_mm$sigma_fixed
        }
        mod <- model_results[[mm]]

        #--- compute likelihood (evaluation of density)
        args <- list(model=mod, y=dat[, ind_mm$dv_vars ],  case=dat$case)
        args <- frm_em_linreg_density_extend_args(args=args, ind_mm=ind_mm)
        dmod <- do.call( what=ind_mm$R_density_fct, args=args )

        #*** arrange coefficients
        mod <- model_results[[mm]]
        cm <- coef(mod)
        if (ind_mm$model=="linreg"){
            model_results[[mm]]$NC <- length(cm)
            if ( is.null(ind_mm$sigma_fixed) ){
                cm["sigma"] <- dmod$sigma
                model_results[[mm]]$sigma <- dmod$sigma
                model_results[[mm]]$est_sigma <- TRUE
            }
            cm["R2"] <- dmod$R2
        }
        # define initial coefficients for successive iterations
        if (ind_mm$model=="logistic"){
            ind0[[mm]]$R_args$beta_init <- coef(mod)
            cm["R2"] <- mod$R2
        }
        if (ind_mm$model=="bctreg"){
            ind0[[mm]]$R_args$beta_init <- coef(mod)
            cm["R2"] <- mod$R2
        }
        if (ind_mm$model=="oprobit"){
            if (iter>1){
                ind0[[mm]]$R_args$beta_init <- coef(mod)
            }
            cm["R2"] <- mod$R2
        }
        coefs[[mm]] <- cm
        like0[,mm] <- dmod$like
        loglike[,mm] <- log( dmod$like + eps )
        post0[,mm] <- dmod$post
        post <- post * dmod$post
        like <- like * dmod$like
        # update observed likelihood
        res3 <- frm_em_calc_update_observed_likelihood(like_obs=like_obs,
                    post_miss=post_miss, dmod=dmod, mm=mm,
                    ind_resp=ind_resp, ind_miss=ind_miss)
        like_obs <- res3$like_obs
        post_miss <- res3$post_miss
    }    #* end mm
    #-------

    post <- frm_normalize_posterior( post=post, case=dat$case)
    dat$weights <- dat$weights0 * post
    #--- compute total log likelihood
    ll <- frm_em_calc_total_likelihood(dat=dat, weights0=weights0,
                like_obs=like_obs, post_miss=post_miss)
    res <- list( loglike=loglike, post=post, coefs=coefs,
                model_results=model_results, ll=ll, post0=post0,
                ind0=ind0, like=like, like0=like0  )
    return(res)
}

# z0 <- TAM:::tamcat(" -- like/post_calculations",z0,TRUE)
