## File Name: frm_partable_thresholds.R
## File Version: 0.03

frm_partable_thresholds <- function(partable)
{
    dfr <- partable
    ind <- grep( "difflogthresh", paste(dfr$parm) )
    if (length(ind) > 0 ){
        dfr$parm <- paste(dfr$parm)
        dfr[ind, "ON"] <- 0
        dfr[ind, "parm"] <- gsub( " ON ", " ", dfr[ind, "parm"] )
    }
    return(dfr)
}
