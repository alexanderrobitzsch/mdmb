## File Name: mdmb_diff_quotient.R
## File Version: 0.02

mdmb_diff_quotient <- function(ll0, ll1, h)
{
	res <- ( ll1 - ll0 ) / h
	return(res)
}
