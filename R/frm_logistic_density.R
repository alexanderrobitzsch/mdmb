## File Name: frm_logistic_density.R
## File Version: 0.12

frm_logistic_density <- function(model, y, design_matrix=NULL, case=NULL)
{
    if ( is.null(design_matrix) ){
        y_pred <- predict(model)
    } else {
        y_pred <- predict(model, newdata=design_matrix)
    }
    d1 <- y_pred
    d1 <- ifelse( y==1, d1, 1 - d1 )
    d2 <- frm_normalize_posterior( post=d1, case=case )
    res <- list( "like"=d1, "post"=d2 )
    return(res)
}
