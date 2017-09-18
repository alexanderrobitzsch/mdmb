## File Name: frm_normalize_vector.R
## File Version: 0.02
## File Last Change: 2017-01-23 19:35:13

frm_normalize_vector <- function(vec)
{
	res <- vec / sum(vec)
	return(res)
}

