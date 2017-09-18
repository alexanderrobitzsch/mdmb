## File Name: frm_normalize_posterior.R
## File Version: 0.04
## File Last Change: 2017-01-23 19:35:13

frm_normalize_posterior <- function( post , case )
{
	post1 <- post
	if ( ! is.null(case) ){
		rs1 <- rowsum( post , case )
		post1 <- post / rs1[ case ]
	}
	return(post1)
}
#!!! case must be numbered from 1 to N
