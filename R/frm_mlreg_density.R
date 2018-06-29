## File Name: frm_mlreg_density.R
## File Version: 0.06


frm_mlreg_density <- function( model, y, case, design_matrix)
{
    outcome <- model$outcome
    data <- design_matrix
    formula_terms <- model$formula_terms
    res <- frm_mlreg_create_design_matrices(data=data, formula_terms=formula_terms )                                
    y <- res$y
    X <- res$X
    Z_list <- res$Z_list
    #-- extract model parameters
    beta <- model$beta
    Psi_list <- model$Psi_list
    sigma2 <- model$sigma2
    alpha <- model$alpha
    NR <- model$NR
    u_list <- model$u_list
    N <- model$N
    idcluster_list <- model$idcluster_list
    onlyintercept_list <- model$onlyintercept_list
    parameter_index <- model$parameter_index
    est_parameter <- model$est_parameter
    npar <- model$npar
    parnames <- model$parnames
    outcome <- model$outcome
    ncluster_list <- model$ncluster_list
    est_sigma2 <- model$est_sigma2
    est_probit <- model$est_probit
    est_thresh <- model$est_thresh
    parnames0 <- model$parnames0
    K <- model$K
    
    #- evaluate normal density
    if (outcome == "normal"){
        ypred <- miceadds::miceadds_rcpp_ml_mcmc_predict_fixed_random( X=X, beta=beta, Z_list=Z_list, 
                        u_list=u_list, idcluster_list=idcluster_list, NR=NR ) 
        sigma <- sqrt(sigma2)
        like <- mdmb_rcpp_dnorm( x=y, mu=ypred[,1], sigma=sigma )
    }        
    res <- list(like=like)
    return(res)
}
