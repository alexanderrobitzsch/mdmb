## File Name: mdmb_regression_optim_yjt_grad.R
## File Version: 0.04

mdmb_regression_optim_yjt_grad <- function(x, index_beta, eps_shape, index_sigma, lambda_fixed,
        is_lambda_fixed, index_lambda, Xdes, offset_values, y, df, probit, weights,
        is_prior, beta_prior, use_grad, dens_fct, np, h )
{
    res <- mdmd_regression_optim_yjt_extract( x=x, index_beta=index_beta, eps_shape=eps_shape, index_sigma=index_sigma,
                        lambda_fixed=lambda_fixed, is_lambda_fixed=is_lambda_fixed, index_lambda=index_lambda )
    beta <- res$beta
    shape <- res$shape
    lambda <- res$lambda
    y_pred <- Xdes %*% beta + offset_values
    x_grad <- rep(NA, np )
    ll0 <- ll_i <- dens_fct( y, location=y_pred, shape=shape, lambda=lambda, df=df, log=TRUE, probit=probit )
    hvec <- mdmb_regression_adjustment_differentiation_parameter(h=h, par=x )
    #*** derivative with respect to beta
    h0 <- hvec[ index_sigma ]
    # derivative with respect to mu (apply chain rule)
    ll1 <- dens_fct( y, location=y_pred+h0, shape=shape, lambda=lambda, df=df, log=TRUE, probit=probit )
    der1 <- - mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=h0)
    wder1 <- weights * der1
    wder1 <- as.vector(wder1)
    x_grad[index_beta] <- colSums( wder1 * Xdes )
    #*** derivative with respect to sigma
    ll1 <- dens_fct( y, location=y_pred, shape=shape+h0, lambda=lambda, df=df, log=TRUE, probit=probit )
    der1 <- - mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=h0)
    x_grad[index_sigma] <- sum( der1 * weights )
    #*** derivative with respect to lambda
    if ( ! is_lambda_fixed ){
        ll1 <- dens_fct( y, location=y_pred, shape=shape, lambda=lambda+h0, df=df, log=TRUE, probit=probit )
        der1 <- - mdmb_diff_quotient(ll0=ll0, ll1=ll1, h=h0)
        x_grad[index_lambda] <- sum( der1 * weights )
    }
    if ( is_prior ){
        xgrad[index_beta] <- xgrad[ index_beta ] - eval_prior_list_gradient_log( par=par, par_prior=beta_prior, h=h )
    }
    return(x_grad)
}
