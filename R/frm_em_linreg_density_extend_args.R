## File Name: frm_em_linreg_density_extend_args.R
## File Version: 0.02
## File Last Change: 2017-01-23 19:35:12

frm_em_linreg_density_extend_args <- function(args, ind_mm)
{
	if (ind_mm$model == "linreg"){
		args$X <- ind_mm$R_des$x
		args$offset <- ind_mm$R_des$offset
	}
	return(args)
}
