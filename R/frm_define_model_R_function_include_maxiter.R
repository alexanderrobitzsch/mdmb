## File Name: frm_define_model_R_function_include_maxiter.R
## File Version: 0.02


frm_define_model_R_function_include_maxiter <- function(R_args, maxiter)
{
    if ( is.null(R_args$maxiter) ){
        R_args$maxiter <- maxiter
    }
    return(R_args)
}
