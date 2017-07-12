
frm_define_model_R_function <- function(model)
{
	R_fct <- NULL
	R_args <- NULL
	R_density_fct <- NULL	
	#--- linear regression normal distribution
	if (model$model == "linreg"){ 
		# R_fct <- stats::lm
		R_fct <- "lm"
		R_fct_name <- "stats::lm"
		R_density_fct <- "frm_linreg_density"
	}
	#--- logistic regression
	if (model$model == "logistic"){ 
		R_fct <- logistic_regression 
		R_fct_name <- "mdmb::logistic_regression"		
		# R_args <- list("family"="binomial")
		R_density_fct <- "frm_logistic_density"
	}	
	#--- linear regression with Box-Cox Transformation
	if (model$model %in% c("bctreg") ){ 
		R_fct <- bct_regression
		R_fct_name <- "mdmb::bct_regression"
		R_density_fct <- "frm_mdmb_regression_density"
	}	
	#--- linear regression with Yeo-Johnson Transformation
	if (model$model %in% c("yjtreg") ){ 
		R_fct <- yjt_regression
		R_fct_name <- "mdmb::yjt_regression"
		R_density_fct <- "frm_mdmb_regression_density"
	}			
	#--- output
	res <- list( R_fct = R_fct , R_args = R_args,
				R_density_fct = R_density_fct , R_fct_name = R_fct_name )
	return(res)
}
