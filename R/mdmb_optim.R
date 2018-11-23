## File Name: mdmb_optim.R
## File Version: 0.12

mdmb_optim <- function(optimizer, par, fn, gr=NULL, method="L-BFGS-B",
        lower=NULL, upper=NULL, control=NULL, h=1e-5)
{
    #*** stats::optim
    if (optimizer=="optim"){
        mod1 <- stats::optim( par=par, fn=fn, gr=gr, method=method,
                    hessian=TRUE, lower=lower, upper=upper, control=control)
        mod1$iter <- mod1$counts['function']
    }
    #*** stats::nlminb
    if (optimizer=="nlminb"){
        mod1 <- stats::nlminb(start=par, objective=fn, gradient=gr, lower=lower,
                        upper=upper, control=control)
        mod1$value <- mod1$objective
        mod1$hessian <- CDM::numerical_gradient(par=mod1$par, FUN=gr, h=h)
        mod1$iter <- mod1$iterations
    }

    #--- arrange output
    mod1$optimizer <- optimizer
    mod1$converged <- mod1$convergence==0
    return(mod1)
}
