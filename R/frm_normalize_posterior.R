## File Name: frm_normalize_posterior.R
## File Version: 0.12

frm_normalize_posterior <- function( post , case )
{
	post1 <- post
	if ( ! is.null(case) ){
		# rs1 <- rowsum( post , case )
		# post1 <- post / rs1[ case ]
		post1 <- mdmb_rcpp_frm_normalize_posterior( post=post, case_=case )		
	}	
	return(post1)
}
