
frm2datlist <- function( object)
{
	imputations_mcmc <- object$imputations_mcmc
	values <- imputations_mcmc$values
	Nimp <- imputations_mcmc$Nimp
	dat0 <- object$dat
	NV <- imputations_mcmc$NV
	impute_vars <- imputations_mcmc$impute_vars
	datlist <- as.list( 1:Nimp )
	for (ii in 1:Nimp){
		dat <- dat0
		for (vv in 1:NV){
			# vv <- 1
			var_vv <- impute_vars[vv] 
			val_vv <- values[[ var_vv ]]
			ind_miss_vv <- imputations_mcmc$ind_miss[[ var_vv ]]
			dat[ ind_miss_vv , var_vv ] <- val_vv[ , ii ]
		}		
		datlist[[ii]] <- dat
	}
	datlist <- miceadds::datlist_create(datlist)
	return(datlist)
}
