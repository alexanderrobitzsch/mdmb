## File Name: frm_mlreg_create_design_matrices.R
## File Version: 0.02


frm_mlreg_create_design_matrices <- function(data, formula_terms )
{
    res <- miceadds::ma_lme4_formula_design_matrices(formula=NULL, data=data, 
                formula_terms=formula_terms, only_design_matrices=TRUE )
    res$Z_list <- res$Z
    return(res)
}
