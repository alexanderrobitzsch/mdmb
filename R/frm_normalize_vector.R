## File Name: frm_normalize_vector.R
## File Version: 0.02

frm_normalize_vector <- function(vec)
{
	res <- vec / sum(vec)
	return(res)
}

