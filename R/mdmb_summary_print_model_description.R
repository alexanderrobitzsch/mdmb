## File Name: mdmb_summary_print_model_description.R
## File Version: 0.02

mdmb_summary_print_model_description <- function(object, pack)
{
    # print package and R information
    sirt::sirt_summary_print_package_rsession(pack=pack)
    # computation time
    mdmb_summary_print_computation_time(object=object)
    # CALL
    CALL <- object$CALL
    if ( ! is.null(CALL) ){
        sirt::sirt_summary_print_call(CALL=CALL)
    }
}
