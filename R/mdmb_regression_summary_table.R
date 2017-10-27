## File Name: mdmb_regression_summary_table.R
## File Version: 0.06

mdmb_regression_summary_table <- function( beta, vcov1 )
{	
	dfr <- data.frame( "parm" = names(beta) )
	dfr$est <- beta
	dfr$se <- sqrt( diag( vcov1) )
	dfr$t <- dfr$est / dfr$se
	dfr$p <- 2 * stats::pnorm( - abs( dfr$t ) )	
	quant <- stats::qnorm(.975)
	dfr$lower95 <- dfr$est - quant * dfr$se
	dfr$upper95 <- dfr$est + quant * dfr$se
	return(dfr)
}
