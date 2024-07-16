## File Name: mdmb_summary_print_computation_time.R
## File Version: 0.03

mdmb_summary_print_computation_time <- function(object)
{
    cat('Date of Analysis:', paste(object$s2), '\n' )
    cat('Computation Time:', print(object$s2 - object$s1), '\n\n')
}
