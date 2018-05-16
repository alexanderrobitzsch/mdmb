## File Name: frm_em_score_function_prepare_model.R
## File Version: 0.10


frm_em_score_function_prepare_model <- function(mm, model_results,
        x , index_coefs_vec , index_sigma_vec, dat , ind_mm )
{
    l1_mm <- index_coefs_vec[[mm]]            
    l2_mm <- index_sigma_vec[[mm]]
    #*** x first argument
    x1 <- x[ l1_mm ]  
    model_results[[mm]]$coefficients <- x1  
    if ( length(l2_mm) > 0 ){
        model_results[[mm]]$sigma <- x[ l2_mm ]
    }                
    args <- list(model = model_results[[mm]] , y = dat[ , ind_mm$dv_vars ],
                          case=dat$case )
    args <- frm_em_linreg_density_extend_args(args=args, ind_mm=ind_mm)
    res <- do.call( what=ind_mm$R_density_fct, args=args )
    return(res)
}

