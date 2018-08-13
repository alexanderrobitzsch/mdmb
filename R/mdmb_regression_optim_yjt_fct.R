## File Name: mdmb_regression_optim_yjt_fct.R
## File Version: 0.01


mdmb_regression_optim_yjt_fct <- function(x, index_beta, eps_shape, index_sigma, lambda_fixed,
        is_lambda_fixed, index_lambda, Xdes, offset_values, y, df, probit, weights,
        is_prior, beta_prior, use_grad, dens_fct )
{
    res <- mdmd_regression_optim_yjt_extract( x=x, index_beta=index_beta, eps_shape=eps_shape, index_sigma=index_sigma,
                        lambda_fixed=lambda_fixed, is_lambda_fixed=is_lambda_fixed, index_lambda=index_lambda )
    beta <- res$beta
    shape <- res$shape
    lambda <- res$lambda
    y_pred <- Xdes %*% beta + offset_values
    ll_i <- dens_fct( y, location=y_pred, shape=shape, lambda=lambda, df=df, log=TRUE, probit=probit )
    ll <- - sum( weights * ll_i )
    #--- include prior distributions
    if (is_prior){
        ll <- ll - eval_prior_list_sumlog( par=x, par_prior=beta_prior, use_grad=use_grad )
    }
    #--- output
    return(ll)
}
