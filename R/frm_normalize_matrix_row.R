## File Name: frm_normalize_matrix_row.R
## File Version: 0.02
## File Last Change: 2017-01-23 19:35:13

frm_normalize_matrix_row <- function(matr)
{
	res <- matr / rowSums(matr)
	return(res)
}

