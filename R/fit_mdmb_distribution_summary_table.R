## File Name: fit_mdmb_distribution_summary_table.R
## File Version: 0.221

fit_mdmb_distribution_summary_table <- function( beta, vcov1 )
{
    dfr <- data.frame(parm=names(beta) )
    dfr$est <- beta
    dfr$se <- sqrt( diag(vcov1) )
    quant <- stats::qnorm(.975)
    dfr$lower95 <- dfr$est - quant * dfr$se
    dfr$upper95 <- dfr$est + quant * dfr$se
    return(dfr)
}
