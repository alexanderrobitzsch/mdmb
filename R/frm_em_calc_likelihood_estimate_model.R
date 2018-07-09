## File Name: frm_em_calc_likelihood_estimate_model.R
## File Version: 0.444

frm_em_calc_likelihood_estimate_model <- function( ind_mm, dat, weights )
{
    R_args <- list( formula=ind_mm$formula, data=dat, weights=weights)
    R_args <- frm_append_list(list1=R_args, list2=ind_mm$R_args)
    R_fct <- ind_mm$R_fct
    #--- linear regression
    is_linreg <- ind_mm$model=="linreg"
    if ( is_linreg ){
        R_args <- ind_mm$R_des
        R_args[["w"]] <- weights
        R_fct <- "mdmb_lm_wfit"
    }
    #-- estimate regression model
    mod <- do.call( what=R_fct, args=R_args )
    if ( is_linreg ){
        yres <- mod$residuals
        mod$sigma <- mdmb_rcpp_weighted_sd_centered( x=yres, w=weights )
        names(mod$coefficients) <- colnames(ind_mm$R_des$x)
    }
    return(mod)
}

# z0 <- Sys.time()
# z0 <- TAM:::tamcat(" ** weighted_sd",z0,TRUE)
