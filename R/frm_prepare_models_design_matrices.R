## File Name: frm_prepare_models_design_matrices.R
## File Version: 0.092

frm_prepare_models_design_matrices <- function( ind0, dat, NM )
{
    for (mm in 1L:(NM+1) ){
        ind_mm <- ind0[[mm]]
        R_des <- NULL
        form <- ind_mm$formula
        R_des[['x']] <- as.matrix( stats::model.matrix( object=form, data=dat ) )
        R_des[['y']] <- as.vector( dat[, ind_mm$dv_vars ] )
        R_des[['offset']] <- offset_values_extract(formula=form, data=dat )
        ind_mm$R_des <- R_des
        ind0[[mm]] <- ind_mm
    }
    return(ind0)
}
