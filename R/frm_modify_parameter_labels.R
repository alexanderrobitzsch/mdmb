## File Name: frm_modify_parameter_labels.R
## File Version: 0.02

frm_modify_parameter_labels <- function( dfr, ind0, NM )
{
	dfr$parm <- paste(dfr$parm)
	#--- labels bctreg and yjreg
	for (mm in 1:(NM+1)){
		model_mm <- ind0[[mm]]$model
		dv_mm <- ind0[[mm]]$dv_vars
		#---- bctreg and yjtreg
		if ( ind0[[mm]]$model %in% c("bctreg" , "yjtreg")){
			indices_mm <- which( dfr$model == mm )
			dfr1 <- dfr[ indices_mm , ]
			NR <- nrow(dfr1)
			index_mm <- seq(NR-1,NR)
			dfr1[ index_mm, "ON"] <- 0
			dfr1[ index_mm, "parm"] <-  paste0( dv_mm , c(" sigma", " lambda" ) )
			dfr[ indices_mm , ] <- dfr1
		}						
	}
	return(dfr)
}
