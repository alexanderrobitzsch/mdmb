## File Name: frm_mlreg_sample_parameters.R
## File Version: 0.14


frm_mlreg_sample_parameters <- function(model, y, design_matrix, 
        R_args, ...)
{
    #*** create new design matrices
    formula_terms <- model$formula_terms
    data <- design_matrix
    res <- frm_mlreg_create_design_matrices(data=data, formula_terms=formula_terms )                                
    y <- res$y
    X <- res$X
    Z_list <- res$Z_list    
    #*** define arguments
    beta <- model$beta
    Psi_list <- model$Psi_list
    sigma2 <- model$sigma2
    alpha <- model$alpha
    NR <- model$NR
    u_list <- model$u_list
    N <- model$N
    idcluster_list <- model$idcluster_list
    sd_proposal <- model$sd_proposal
    onlyintercept_list <- model$onlyintercept_list
    parameter_index <- model$parameter_index
    est_parameter <- model$est_parameter
    npar <- model$npar
    thresh_fac <- model$thresh_fac
    parnames <- model$parnames
    outcome <- model$outcome
    ncluster_list <- model$ncluster_list
    sigma2_nu0 <- model$sigma2_nu0
    sigma2_sigma2_0 <- model$sigma2_sigma2_0
    psi_nu0_list <- model$psi_nu0_list
    psi_S0_list <- model$psi_S0_list
    est_sigma2 <- model$est_sigma2
    est_probit <- model$est_probit
    est_thresh <- model$est_thresh
    iter <- model$iter
    save_iter <- model$save_iter
    verbose <- FALSE
    print_iter <- 9999
    parnames0 <- model$parnames0
    K <- model$K
    #*** apply sampling function
    ml_mcmc_fit_args <- list( y=y, X=X, Z_list=Z_list, beta=beta, Psi_list=Psi_list,
            sigma2=sigma2, alpha=alpha, u_list=u_list, idcluster_list=idcluster_list,
            onlyintercept_list=onlyintercept_list, ncluster_list=ncluster_list,
            sigma2_nu0=sigma2_nu0, sigma2_sigma2_0=sigma2_sigma2_0, psi_nu0_list=psi_nu0_list,
            psi_S0_list=psi_S0_list, est_sigma2=est_sigma2, est_probit=est_probit,
            parameter_index=parameter_index, est_parameter=est_parameter, npar=npar,
            iter=iter, save_iter=save_iter, verbose=verbose, print_iter=print_iter,
            parnames0=parnames0, K=K, est_thresh=est_thresh, thresh_fac=thresh_fac,
            parm_summary=FALSE)    
    res <- do.call( miceadds::ml_mcmc_fit, args=ml_mcmc_fit_args)
    sampled_values <- res$sampled_values    
    coef <- sampled_values[ nrow(sampled_values), ]
    names(coef) <- colnames(sampled_values)
    res$coef <- coef
    return(res)
}
