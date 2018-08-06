## File Name: frm_fb_partable.R
## File Version: 0.455

frm_fb_partable <- function( ind0, parms_mcmc )
{
    parms <- parms_mcmc$parms
    parms_index <- parms_mcmc$parms_index
    NM <- attr( ind0, "NM" )
    dfr <- NULL
    # rename_index <- c(1,5)
    rename_index <- c(1,4)
    rename_names <- c("idparm","parm")
    accrate <- NULL

    for (mm in 1:(NM+1)){
        # mm <- 1
        names_mm <- parms[[mm]][1]
        NC <- length(names_mm)
        # NC <- ind0[[mm]]$N_coef

        model_mm <- ind0[[mm]]$model
        dv_mm <- ind0[[mm]]$dv_vars
        c1 <- names_mm
        if (NC > 0){
            dfr1 <- data.frame(
                        "index"=parms_index[[mm]][1],
                        "model"=mm,
                        "dv"=dv_mm, "parm"=c1,
                        "ON"=1, "est"=NA )
            c2 <- ind0[[mm]]$coef_MH
            c2 <- c2$accepted / c2$iter
            accrate <- c( accrate, c2)
        } else {
            dfr1 <- NULL
        }
        colnames(dfr1)[ rename_index ] <- rename_names

        #*** include standard deviation if model=="linreg"

        include_sigma <- ( ind0[[mm]]$model=="linreg" )
        is_sigma_fixed <- ( ! is.null( ind0[[mm]]$sigma_fixed ) )
        include_sigma <- include_sigma & ( ! is_sigma_fixed    )
        names_mm <- parms[[mm]][2]
        if ( include_sigma ){
            dfr1b <- data.frame(
                        "index"=parms_index[[mm]][2],
                        "model"=mm,
                        # "type"=model_mm,
                        "dv"=dv_mm,
                        "parm"=names_mm, "ON"=0,
                        "est"=NA )
            colnames(dfr1b)[ rename_index ] <- rename_names
            dfr1 <- rbind( dfr1, dfr1b )
            c2 <- ind0[[mm]]$sigma_MH
            c2 <- c2$accepted / c2$iter
            accrate <- c( accrate, c2)
        }
        dfr <- rbind( dfr1, dfr )
    }

    NP <- nrow(dfr)
    dfr <- data.frame("index"=1:NP, dfr )
    rownames(dfr) <- NULL
    dfr <- dfr[ ! is.na( dfr$idparm ), ]

    #-- include labels bctreg or yjtreg
    dfr <- frm_modify_parameter_labels( dfr=dfr, ind0=ind0, NM=NM )
    #-- include labels thresholds
    dfr <- frm_partable_thresholds(partable=dfr)

    #--- compute statistics
    values <- parms_mcmc$values
    est <- colMeans( values )
    ind1 <- dfr$idparm
    dfr[, "est" ] <- est[ind1]
    dfr[, "se" ] <- apply( values, 2, stats::sd )[ind1]

    p1 <- colMeans( values > 0 )
    p2 <- colMeans( values < 0 )
    y0 <- 2 * ifelse( p1 < p2, p1, p2 )
    dfr[, "p" ] <- y0[ind1]
    dfr[, "lower95"] <- apply( values, 2, stats::quantile, probs=.025 )[ind1]
    dfr[, "upper95"] <- apply( values, 2, stats::quantile, probs=.975 )[ind1]

    #--- convert parameter values into coda object
    NV <- ncol(values)
    values <- values[, seq(NV,1,-1) ]
    dfr11 <- dfr[ order(dfr$idparm), ]
    dfr11 <- dfr11[ seq(NV,1,-1), ]
    colnames(values) <- paste(dfr$parm)

    values_coda <- coda::mcmc(data=values, start=min(parms_mcmc$iter_save),
                                end=max(parms_mcmc$iter_save),
                                thin=diff(parms_mcmc$iter_save)[1] )
#    colnames(values_coda) <- paste( dfr$parm )

    #--- technical summary MCMC algorithm
    dfr2 <- dfr[, 1:5]
    dfr2[,"Nsampled"] <- nrow(values)

    colnames(values_coda) <- gsub( "ON difflogthresh", "difflogthresh", colnames(values_coda))
    es <- coda::effectiveSize(values_coda)

    parnames <- paste(dfr2$parm)
    dfr2[,"effsize"] <- es[parnames]
    dfr2[,"accrate"] <- accrate[ dfr2$idparm ]

    #* compute Rhat function
    r1 <- sirt::mcmc_Rhat( mcmc_object=values_coda, n_splits=3 )
    dfr2[,"Rhat"] <- r1[ parnames ]

    #--- covariance matrix of parameters
    coefs <- dfr$est
    names(coefs) <- dfr$parm
    colnames(values) <- gsub( "ON difflogthresh", "difflogthresh", colnames(values))

    vcovs <- stats::cov( values[, parnames]  )
    dfr$index <- NULL
    dfr2$index <- NULL

    #--- output
    res <- list( partable=dfr, tech_summary=dfr2, vcov=vcovs,
                    coef=coefs, values_coda=values_coda )
    return(res)
}
