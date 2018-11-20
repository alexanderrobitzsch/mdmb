## File Name: frm_mdmb_regression_density.R
## File Version: 0.412

frm_mdmb_regression_density <- function(model, y, design_matrix=NULL, case=NULL,
        X=NULL, offset=NULL )
{
    # pars <- coef(model)
    pars <- mdmb_extract_coef(mod=model)
    class_model <- class(model)
    np <- length(pars)
    beta <- pars[ model$index_beta ]
    if ( is.null(design_matrix) ){
        if (is.null(X) ){
            y_pred <- predict(model)
        } else {
            y_pred <- X %*% beta
            if (! is.null(offset) ){
                y_pred <- y_pred + offset
            }
        }
    } else {
        #  y_pred <- predict(model, newdata=design_matrix )
        # form <- attr( model$model, "terms")
        form <- model$formula
        Xdes <- stats::model.matrix( object=form, data=design_matrix )
        offset_values <- offset_values_extract(formula=form, data=design_matrix )
        y_pred <- Xdes %*% beta + offset_values
    }
    w <- model$weights
    if ( is.null(w) ){
        w <- rep( 1, length(y) )
    }
    y_sd <- mdmb_weighted_sd( x=y_pred, w=w )

    #--- extract parameters
    df <- model$df
    if (model$est_df){
        logdf <- pars[model$index_df]
        df <- mdmb_compute_df(x=logdf, df=Inf, est_df=TRUE)
    }
    lambda <- pars[model$index_lambda]
    sigma <- pars[model$index_sigma]
    use_probit <- model$probit
    #*** y values on the transformed metric
    if (class_model=="bct_regression"){
        yt <- bc_trafo( y=y, lambda=lambda )
    }
    if (class_model=="yjt_regression"){
        yt <- yj_trafo( y=y, lambda=lambda, probit=use_probit )
    }
    y_sd0 <- mdmb_weighted_sd( x=yt, w=w )
    if ( ! is.null(model$sigma) ){
        y_sd <- model$sigma
    }

    #--- R^2
    R2 <- mean( y_sd^2 / y_sd0^2 )
    #****** evaluated density
    if (class_model    =="bct_regression"){
        d1 <- dbct_scaled( y, location=y_pred, shape=sigma, lambda=lambda, df=df )
    }
    if (class_model    =="yjt_regression"){
        d1 <- dyjt_scaled( y, location=y_pred, shape=sigma,
                    lambda=lambda, df=df, probit=use_probit )
    }
    d2 <- frm_normalize_posterior( post=d1, case=case )
    res <- list( "like"=d1, "post"=d2, "sigma"=y_sd, R2=R2)
    return(res)
}
