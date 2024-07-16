## File Name: frm_formula_extract_terms.R
## File Version: 0.121

frm_formula_extract_terms <- function(formula)
{
    dv_form <- formula[[2]]
    iv_form <- formula[[3]]
    h1 <- all.vars(formula)
    h2 <- stats::terms(formula)
    terms_formula_transform <- FALSE
    X_factors <- colnames( attr(h2,'factors') )
    if ( length(X_factors) > 0 ){
        if ( mean( X_factors %in% h1) < 1 ){
            terms_fomula_transform  <- FALSE
        }
    } else {
        X_factors <- NULL
    }
    res <- list( dv_form=dv_form, iv_form=iv_form, all_vars=h1, terms_formula=h2,
                terms_formula_transform=terms_formula_transform,
                X_factors=X_factors )
    res$dv_vars <- h1[1]
    res$iv_vars <- h1[-1]
    return(res)
}
