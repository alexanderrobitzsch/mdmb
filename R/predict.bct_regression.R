## File Name: predict.bct_regression.R
## File Version: 0.06

predict.bct_regression <- function( object, newdata=NULL, trafo=TRUE, ...)
{
    fitted.values <- mdmb_regression_predict_yjt_bct( object=object,
                        newdata=newdata, trafo=trafo, ...)
    return(fitted.values)
}
