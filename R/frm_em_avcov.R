## File Name: frm_em_avcov.R
## File Version: 0.962

frm_em_avcov <- function(res, dat, ind0, NM, h=h)
{

    #------ standard error calculation
    model_results <- res$model_results
    loglike0 <- res$loglike
    post0 <- res$post0
    coefs <- res$coefs
    dfr <- NULL
    for (mm in 1:(NM+1)){
        # mm <- 1
        coef_mm <- coef(model_results[[mm]] )
        NC <- length(coef_mm)
        names_mm <- names(coef_mm)
        model_mm <- ind0[[mm]]$model
        dv_mm <- ind0[[mm]]$dv_vars
        c1 <- paste0( dv_mm, " ON ", names_mm )
        if (NC > 0){
            dfr1 <- data.frame( "model"=mm, "dv"=dv_mm, "parm"=c1,
                "ON"=1, "est"=coef_mm )
        } else {
            dfr1 <- NULL
        }

        #*** include standard deviation if model=="linreg"
        include_sigma <- ( ind0[[mm]]$model=="linreg" )
        is_sigma_fixed <- ( ! is.null( ind0[[mm]]$sigma_fixed ) )
        include_sigma <- include_sigma & ( ! is_sigma_fixed    )
        if ( include_sigma ){
            sigma <- model_results[[mm]]$sigma
            dfr1b <- data.frame( "model"=mm, "dv"=dv_mm,
                        "parm"=paste0( dv_mm, " sigma" ), "ON"=0, "est"=sigma )
            dfr1 <- rbind( dfr1, dfr1b )
        }
        dfr1$parm <- paste(dfr1$parm)

        dfr <- rbind( dfr1, dfr )

    }

    NP <- nrow(dfr)
    dfr <- data.frame("index"=1:NP, dfr )
    rownames(dfr) <- NULL
    x <- dfr$est
    index_coefs_vec <- as.list(1:(NM+1))
    index_sigma_vec <- as.list(1:(NM+1))
    for (mm in 1:(NM+1)){
        index_coefs_vec[[mm]] <- which( ( dfr$model==mm ) & ( dfr$ON==1 ) )
        index_sigma_vec[[mm]] <- which( ( dfr$model==mm ) & ( dfr$ON==0 ) )
    }

    #----------------------- define Q-function in expected log likelihood
    f <- function(x, y, update=seq(1,NM+1), return_like=FALSE,
            update_y=TRUE ){

        #*** function Q( x, y )

        N2 <- nrow(dat)
        # loglike <- matrix(NA, nrow=N2, ncol=NM+1)
        loglike <- loglike0
        post <- 1 + 0*dat$weights
        eps <- 1e-30
        post0a <- post0

        for (mm in update ){
            ind_mm <- ind0[[mm]]
            l1_mm <- index_coefs_vec[[mm]]
            l2_mm <- index_sigma_vec[[mm]]
            #*** x first argument
            x1 <- x[ l1_mm ]
            model_results[[mm]]$coefficients <- x1
            if ( length(l2_mm) > 0 ){
                model_results[[mm]]$sigma <- x[ l2_mm ]
            }
            args <- list(model=model_results[[mm]], y=dat[, ind_mm$dv_vars ],
                          case=dat$case )
            dmod <- do.call( what=ind_mm$R_density_fct, args=args )
            loglike[,mm] <- log( dmod$like + eps )
            #*** y second argument
            x1 <- y[ l1_mm ]
            testdiff <- any( y[ l1_mm ] !=x[ l1_mm ] )
            if ( testdiff & update_y){
                model_results[[mm]]$coefficients <- x1
                args <- list(model=model_results[[mm]], y=dat[, ind_mm$dv_vars ],
                                case=dat$case )
                args <- frm_em_linreg_density_extend_args(args=args, ind_mm=ind_mm)
                dmod <- do.call( what=ind_mm$R_density_fct, args=args )
            }
            post0a[,mm] <- dmod$post
            # post <- post * dmod$post
        }
        for (mm in seq(1,NM+1)){
            post <- post * post0a[,mm]
        }
        post <- frm_normalize_posterior( post=post, case=dat$case )
        post <- dat$weights0 * post
        ll <- sum(loglike * post)
        res <- list( ll=ll, loglike=loglike, post0a=post0a )
        if ( ! return_like ){
            res <- ll
        }
        return(res)
    }
    #-------------------------------------------
    FUN <- f
    par1 <- par <- x0 <- x
    # compute score function with respect to theta (all coordinates)
    f0 <- FUN(x=par, y=par, update=NULL )
    abs_par <- abs( par )
    hvec <- h * ifelse(abs_par > 1, abs_par, 1)
    #-----------------------------------------------------
    #-----------------------------------------------------
    # define score function
    score_f1 <- function(par){
        NP <- length(par)
        abs_par <- abs(par)
#        hvec <- h * ifelse(abs_par > 1, abs_par, 1)
        x <- par
        y <- par
        #*** function Q( x, y )
        N2 <- nrow(dat)
        # loglike <- matrix(NA, nrow=N2, ncol=NM+1)
        loglike <- loglike0
        post <- 1 + 0*dat$weights
        eps <- 1E-30
        post0a <- post0
        for (mm in 1:(NM+1)){
            ind_mm <- ind0[[mm]]
            l1_mm <- index_coefs_vec[[mm]]
            l2_mm <- index_sigma_vec[[mm]]
            dmod <- frm_em_score_function_prepare_model(mm=mm,
                        model_results=model_results, x=x, index_coefs_vec=index_coefs_vec,
                        index_sigma_vec=index_sigma_vec, dat=dat, ind_mm=ind_mm )
            loglike[,mm] <- log( dmod$like + eps )
            #*** y second argument
            x1 <- y[ l1_mm ]
            testdiff <- any( y[ l1_mm ] !=x[ l1_mm ] )
            if ( testdiff ){
                dmod <- frm_em_score_function_prepare_model(mm=mm,
                            model_results=model_results, x=y, index_coefs_vec=index_coefs_vec,
                            index_sigma_vec=index_sigma_vec, dat=dat, ind_mm=ind_mm )
            }
            post0a[,mm] <- dmod$post
        }
        for (mm in seq(1,NM+1)){
            post <- post * post0a[,mm]
        }
        post <- frm_normalize_posterior( post=post, case=dat$case )
        post <- dat$weights0 * post
        ll <- sum(loglike * post)
        res <- list( ll=ll, loglike=loglike, post0a=post0a )
        f0a <- res
        f0 <- f0a$ll
        #--------------------------------
        #----- derivatives
        # f0a <- FUN(x=par, y=par, return_like=TRUE )
        loglike0 <- f0a$loglike
        post0 <- f0a$post0a
        score_fct0 <- rep(NA,NP)

        for (ii in 1:NP){
            par1 <- par
            par1[ii] <- par[ii] + hvec[ii]

            loglike <- f0a$loglike
            post <- 1 + 0*dat$weights
            post0a <- f0a$post0a
            ####   x=par1
            ####   y=par
            x <- par1
            y <- par
            for (mm in 1:(NM+1)){
                ind_mm <- ind0[[mm]]
                l1_mm <- index_coefs_vec[[mm]]
                l2_mm <- index_sigma_vec[[mm]]
                l3_mm <- unique( c( l1_mm, l2_mm )    )
                if ( length(l3_mm) > 0 ){
                    update_model <- ( max( abs( par1[ l3_mm ] - par[l3_mm ]    ) ) > 1E-10 )
                } else {
                    update_model <- FALSE
                }
                # update_model <- TRUE
                if ( update_model ){
                    dmod <- frm_em_score_function_prepare_model(mm=mm,
                                model_results=model_results, x=x, index_coefs_vec=index_coefs_vec,
                                index_sigma_vec=index_sigma_vec, dat=dat, ind_mm=ind_mm )
                    loglike[,mm] <- log( dmod$like + eps )

                    #*** y second argument
                    #    x1 <- y[ l1_mm ]
                    #    testdiff <- any( y[ l1_mm ] !=x[ l1_mm ] )
                    #    if ( testdiff ){
                    #        dmod <- frm_em_score_function_prepare_model(mm=mm,
                    #                    model_results=model_results, x=y, index_coefs_vec=index_coefs_vec,
                    #                    index_sigma_vec=index_sigma_vec, dat=dat, ind_mm=ind_mm )
                    #    }
                    #    post0a[,mm] <- dmod$post
                }
            }

            for (mm in seq(1,NM+1)){
                post <- post * post0a[,mm]
            }
            post <- frm_normalize_posterior( post=post, case=dat$case )
            post <- dat$weights0 * post
            score_fct0[ii] <- sum(loglike * post)

        }  # end parameter ii
        score_fct0 <- ( score_fct0 - f0 ) / hvec
        return(score_fct0)
    }
    #----------------------------------------------------
    #-----------------------------------------------------

    score_fct1 <- score_f1( par=par )
    infomat <- matrix(NA,nrow=NP,ncol=NP)
    m1 <- paste0( rep("*",NP), collapse="")
    cat( paste0("\n|",m1,"|","\n|") )

    for (ii in 1:NP){
        cat("-")
        utils::flush.console()
        par1 <- par
        par1[ii] <- par[ii] + hvec[ii]
        sc2 <- score_f1( par=par1 )
        infomat[ii,] <- (sc2 - score_fct1) / hvec[ii]
    }
    cat("|\n")
    #-- modify parameter labels
    dfr <- frm_modify_parameter_labels( dfr=dfr, ind0=ind0, NM=NM )

    #-- parameter labels thresholds
    dfr <- frm_partable_thresholds(partable=dfr)

    infomat <- - infomat
    avcov <- mdmb_ginv( x=infomat )
    se <- sqrt( diag(avcov) )
    dfr$se <- se

    dfr <- sirt::parmsummary_extend(dfr=dfr)
    res <- list( avcov=avcov, se=se, partable=dfr, info=infomat )
    return(res)
}
