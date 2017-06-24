

frm_formula_character <- function( ind_mm)
{
	formula_mm <- paste0( ind_mm$formula[2] , " ~ " , ind_mm$formula[3] )
	formula_mm <- paste0( ind_mm$R_fct_name , "( " , formula_mm , " )" )
	return(formula_mm)
}	
