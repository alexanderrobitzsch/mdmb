## File Name: frm2datlist.R
## File Version: 0.091

frm2datlist <- function( object, as_mids=FALSE)
{
    imputations_mcmc <- object$imputations_mcmc
    values <- imputations_mcmc$values
    Nimp <- imputations_mcmc$Nimp
    dat0 <- object$dat
    NV <- imputations_mcmc$NV
    impute_vars <- imputations_mcmc$impute_vars
    datlist <- as.list( 1L:Nimp )
    for (ii in 1L:Nimp){
        dat <- dat0
        for (vv in 1L:NV){
            var_vv <- impute_vars[vv]
            val_vv <- values[[ var_vv ]]
            ind_miss_vv <- imputations_mcmc$ind_miss[[ var_vv ]]
            dat[ ind_miss_vv, var_vv ] <- val_vv[, ii ]
        }
        datlist[[ii]] <- dat
    }
    datlist <- miceadds::datlist_create(datlist)
    if ( as_mids ){
        datlist <- miceadds::datalist2mids(datlist)
    }
    return(datlist)
}
