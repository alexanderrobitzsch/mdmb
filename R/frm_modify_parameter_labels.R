## File Name: frm_modify_parameter_labels.R
## File Version: 0.123

frm_modify_parameter_labels <- function( dfr, ind0, NM )
{
    dfr$parm <- paste(dfr$parm)
    #--- labels bctreg and yjreg
    for (mm in 1L:(NM+1)){
        model_mm <- ind0[[mm]]$model
        dv_mm <- ind0[[mm]]$dv_vars
        #---- bctreg and yjtreg
        if ( ind0[[mm]]$model %in% c('bctreg', 'yjtreg')){
            indices_mm <- which( dfr$model==mm )
            dfr1 <- dfr[ indices_mm, ]
            dfr1$parm <- gsub( 'ON sigma', 'sigma', dfr1$parm )
            dfr1$parm <- gsub( 'ON lambda', 'lambda', dfr1$parm )
            dfr1$parm <- gsub( 'ON logdf', 'logdf', dfr1$parm )
            ind_fill <- grep( ' ON ', dfr1$parm)
            if ( length(ind_fill) > 0 ){
                dfr1$ON <- 0
                dfr1[ind_fill, 'ON'] <- 1
            }
            dfr[ indices_mm, ] <- dfr1
        }
    }
    return(dfr)
}
