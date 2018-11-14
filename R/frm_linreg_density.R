## File Name: frm_linreg_density.R
## File Version: 0.9708

frm_linreg_density <- function(model, y, design_matrix=NULL, case=NULL,
        X=NULL, offset=NULL, use_in_frm_fb=FALSE )
{
    if ( is.null(design_matrix) ){
        if (is.null(X) ){
            y_pred <- predict(model)
        } else {
            beta <- model$coefficients
            y_pred <- X %*% beta
            if (! is.null(offset) ){
                y_pred <- y_pred + offset
            }
        }
    } else {
        #  y_pred <- predict(model, newdata=design_matrix )
        form <- attr( model$model, "terms")
        Xdes <- stats::model.matrix( object=form, data=design_matrix )
        offset_values <- offset_values_extract(formula=form, data=design_matrix )
        beta <- coef(model)
        if ( ncol(Xdes) > 0 ){
            y_pred <- Xdes %*% beta + offset_values
        } else {
            y_pred <- offset_values
        }
    }

    w <- model$weights
    yr <- residuals(model)
    if ( is.null(w) ){
        w <- rep( 1, length(y) )
    }
    w1 <- w
    if (use_in_frm_fb){
        w1 <- rep(1, length(y) )
    }

    y_sd <- mdmb_rcpp_weighted_sd_centered( x=yr, w=w )
    if ( ! use_in_frm_fb ){
        y1 <- y
        if ( length(y1) > length(w) ){
            y1 <- y[ ! is.na(y) ]
            if ( length(y1) > length(w) ){
                y1 <- y1[ seq(1,length(w))]
            }
        }
        y <- y1
    }

    y_sd0 <- mdmb_rcpp_weighted_sd( x=y, w=w1 )
    if ( ! is.null(model$sigma) ){
        y_sd <- model$sigma
    }

    # R^2
    R2 <- mean( 1 - y_sd^2 / y_sd0^2 )

    #-- evaluate normal density
    if ( is.matrix(y_pred) ){
        y_pred <- y_pred[,1]
    }
    if ( length(y_sd)==1 ){
        d1 <- mdmb_rcpp_dnorm( x=y, mu=y_pred, sigma=y_sd)
    } else {
        d1 <- stats::dnorm( x=y, mean=y_pred, sd=y_sd)
    }

    d2 <- NULL
    if ( ! use_in_frm_fb ){
        d2 <- frm_normalize_posterior( post=d1, case=case )
    }

    #--- output
    res <- list( like=d1, post=d2, sigma=y_sd, R2=R2)
    return(res)
}

# z0 <- TAM:::tamcat(" ~~~~ mdmb_rcpp_dnorm",z0,TRUE)
