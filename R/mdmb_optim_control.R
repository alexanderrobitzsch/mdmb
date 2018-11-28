## File Name: mdmb_optim_control.R
## File Version: 0.03

mdmb_optim_control <- function(optimizer, control, maxiter)
{
    if (is.null(control)){
        control <- list()
    }
    #*** stats::optim
    if (optimizer=="optim"){ maxit_label <- "maxit" }
    #*** stats::nlminb
    if (optimizer=="nlminb"){ maxit_label <- "iter.max" }
    #*** maximum number of iterations
    if (is.null(control[[maxit_label]]) ){
        control[[maxit_label]] <- maxiter
    }
    #--- output
    return(control)
}
