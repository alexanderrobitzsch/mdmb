## File Name: frm_mlreg_density.R
## File Version: 0.171


frm_mlreg_density <- function( model, y, case, design_matrix,
                        id_variable_level_unique=NULL)
{
    outcome <- model$outcome
    data <- design_matrix
    formula_terms <- model$formula_terms
    if ( ! is.null(id_variable_level_unique) ){
        data <- data[ id_variable_level_unique, ]
    }
    res <- frm_mlreg_create_design_matrices(data=data, formula_terms=formula_terms )
    y <- res$y
    X <- res$X
    Z_list <- res$Z_list
    #-- extract model parameters
    beta <- model$beta
    sigma2 <- model$sigma2
    alpha <- model$alpha
    NR <- model$NR
    u_list <- model$u_list
    idcluster_list <- model$idcluster_list
    outcome <- model$outcome
    K <- model$K
    #- evaluate densities
    ypred <- miceadds::miceadds_rcpp_ml_mcmc_predict_fixed_random( X=X, beta=beta,
                            Z_list=Z_list, u_list=u_list, idcluster_list=idcluster_list,
                            NR=NR )
    ypred <- ypred[,1]
    if (outcome=='normal'){
        sigma <- sqrt(sigma2)
        like <- mdmb_rcpp_dnorm( x=y, mu=ypred, sigma=sigma )
    }
    if (outcome=='probit'){
        like <- miceadds::miceadds_rcpp_ml_mcmc_probit_category_prob( y_int=y,
                    alpha=alpha, mu1=ypred, use_log=FALSE )
    }
    res <- list(like=like)
    return(res)
}
