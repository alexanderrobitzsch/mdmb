
frm_normalize_matrix_row <- function(matr)
{
	res <- matr / rowSums(matr)
	return(res)
}

