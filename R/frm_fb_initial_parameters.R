## File Name: frm_fb_initial_parameters.R
## File Version: 0.452

frm_fb_initial_parameters <- function(dat, ind0, data_init, ind_miss=NULL )
{
    weights <- dat$weights
    NM <- attr(ind0, "NM")
    NM1 <- NM + 1
    if ( ! is.null(data_init) ){
        subs <- intersect( colnames(data_init), colnames(dat) )
        dat[,subs] <- data_init[, subs]
    }
    model_results <- list()
    parms <- list()
    parms00 <- list( NA, NA )

    for (mm in 1:NM1){
        ind_mm <- ind0[[mm]]
        var_mm <- ind_mm$dv_vars
        ind_miss_mm <- ind_miss[[ var_mm ]]
        model_mm <- ind_mm$model
        parms0 <- parms00
        #--- estimate model with weights
        use_variable_level <- FALSE
        id_variable_level <- NULL
        id_variable_level_unique <- NULL
        variable_info <- NULL
        if ( ! is.null( ind_mm$variable_level) ){
            use_variable_level <- TRUE
            id <- dat[, ind_mm$variable_level]
            id_variable_level <- match( id, unique(id) )
            id_variable_level_unique <- which( ! duplicated(id_variable_level) )
            v1 <- dat[ id_variable_level_unique, var_mm ]
            dat[, var_mm ] <- v1[ id_variable_level ]
            #-- data frame with informations
            N <- nrow(dat)
            variable_info <- data.frame( case=1:N, id=id_variable_level)
            variable_info$miss <- variable_info$case %in% ind_miss_mm
            variable_info$unique <- variable_info$case %in% id_variable_level_unique
            variable_info <- variable_info[ variable_info$miss, ]
            variable_info$miss_id <- 1:nrow(variable_info)
            variable_info$replace_miss_id <- variable_info$miss_id
            # unique_cases <- unique(variable_info$id[ variable_info$unique ] )
            var1 <- variable_info[ variable_info$unique, ]
            var2 <- variable_info[ ! variable_info$unique, ]
            var2[, "replace_miss_id"] <- var1[ match( var2$id, var1$id), "miss_id" ]
            variable_info <- rbind( var1, var2)
            variable_info <- variable_info[ order(variable_info$miss_id), ]
        }
        ind_mm$use_variable_level <- use_variable_level
        ind_mm$id_variable_level <- id_variable_level
        ind_mm$id_variable_level_unique <- id_variable_level_unique
        ind_mm$variable_info <- variable_info
        #*** estimate model
        R_args <- frm_estimate_model_create_R_args(dat=dat, weights=weights, ind_mm=ind_mm)
        R_args <- frm_append_list(list1=R_args, list2=ind_mm$R_args)
        if (model_mm %in% c("linreg","oprobit")){
            R_args$probit <- NULL
        }

        if (model_mm %in% c("mlreg")){
            # R_args$inits_lme4 <- FALSE
        }
        
        mod <- do.call( what=ind_mm$R_fct, args=R_args )
        model_results[[mm]] <- mod
        # se_mod <- mdmb_vcov2se(vcov=vcov(mod))
        se_mod <- frm_fb_initial_parameters_se_sd_proposal(mod=mod)
        ind_mm$N_coef <- length(se_mod)
        ind_mm$coef <- coef(mod)
        ind_mm$coef_sd_proposal <- se_mod
        ind_mm$coef_parnames <- NA
        if ( ind_mm$N_coef > 0 ){
            names_mm <- names(ind_mm$coef)
            NM <- length(names_mm)
            on1 <- rep(" ON ", NM )
            if (ind_mm$model %in% c("bctreg","yjtreg") ){
                on1[ (NM-1):NM ] <- " "
                ind_mm$index_lambda <- mod$index_lambda
                ind_mm$index_df <- mod$index_df
                ind_mm$est_df <- mod$est_df
                ind_mm$df_min <- mod$df_min
                ind_mm$df_max <- mod$df_max
                ind_mm$logdf_min <- log(mod$df_min)
                ind_mm$logdf_max <- log(mod$df_max)
            }
            ind_mm$coef_parnames <- paste0( var_mm, on1, names_mm )
        }
        if ( model_mm=="mlreg"){
            ind_mm$coef_parnames <- paste0( var_mm, "_", names_mm )
        }
        parms0[[1]] <- ind_mm$coef_parnames
        v1 <- 0*coef(mod)
        ind_mm$coef_MH$accepted <- v1
        ind_mm$coef_MH$iter <- v1
        ind_mm$N_sigma <- 0
        ind_mm$sigma_parnames <- NULL
        ind_mm$sample_sigma <- FALSE
        # residual standard deviation
        est_sigma <- model_mm %in% c( "linreg")
        if ( ! is.null( ind_mm$sigma_fixed ) ){
            est_sigma <- FALSE
        }
        if ( est_sigma ){
            sigma <- mdmb_weighted_sd(x=residuals(mod), w=weights,
                        unbiased=TRUE, na.rm=TRUE)
            ind_mm$N_sigma <- 1
            ind_mm$sample_sigma <- TRUE
            ind_mm$sigma <- sigma
            ind_mm$sigma_sd_proposal <- sigma / sqrt( nrow(dat) )
            ind_mm$sigma_parnames <- paste0( var_mm, " sigma" )
            parms0[[2]] <- ind_mm$sigma_parnames
            ind_mm$sigma_MH$accepted <- 0
            ind_mm$sigma_MH$iter <- 0
        }
        parms[[mm]] <- parms0
        ind0[[mm]] <- ind_mm

    }
    #--- indices for parameters to be saved
    parms_index <- parms
    N0 <- 0
    for (mm in 1:NM1){
        for (vv in 1:2){
            p_m_vv <- parms_index[[mm]][[vv]]
            p_m_vv <- p_m_vv[ ! is.na( p_m_vv ) ]
            NC <- length( p_m_vv )
            if ( NC > 0 ){
                vec0 <- N0 + seq(1,NC)
                N0 <- N0 + NC
                parms_index[[mm]][[vv]] <- vec0
            }
        }
    }
    #--- output
    res <- list(ind0=ind0, parms=parms, parms_index=parms_index,
                npars=N0, model_results=model_results, dat=dat )
    return(res)
}


