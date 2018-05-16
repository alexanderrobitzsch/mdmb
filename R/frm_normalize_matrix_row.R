## File Name: frm_normalize_matrix_row.R
## File Version: 0.02

frm_normalize_matrix_row <- function(matr)
{
    res <- matr / rowSums(matr)
    return(res)
}

