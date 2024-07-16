## File Name: frm_fb_descriptives_variables.R
## File Version: 0.113

frm_fb_descriptives_variables <- function(dat, predictorMatrix,
        freq_miss_values, dat0, parms_mcmc )
{
    vars <- colnames(predictorMatrix)
    NV <- length(vars)
    dfr <- data.frame(variable=vars, M=NA, SD=NA )
    for (vv in 1L:NV){
        var_vv <- vars[vv]
        dfr$M[vv] <- mean( parms_mcmc$M_mcmc[, var_vv ] )
        dfr$SD[vv] <- mean( parms_mcmc$SD_mcmc[, var_vv ] )
    }
    N <- nrow(dat0)
    dfr1 <- data.frame( variable=names(freq_miss_values),
                        N_obs=N-freq_miss_values,
                        N_miss=freq_miss_values )
    dfr <- merge( x=dfr1, y=dfr, by='variable', all=TRUE)
    dfr <- dfr[ match( vars, dfr$variable ), ]
    return(dfr)
}
