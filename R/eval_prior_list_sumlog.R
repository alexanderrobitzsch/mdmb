

eval_prior_list_sumlog <- function( par , par_prior )
{
	eps <- 1E-50
	v1 <- eval_prior_list( par=par , par_prior=par_prior )
	res <- sum( log( v1 +eps) )
	return(res)
}
