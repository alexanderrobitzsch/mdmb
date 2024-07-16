## File Name: frm_descriptives_variables.R
## File Version: 0.172

frm_descriptives_variables <- function(dat, predictorMatrix,
        freq_miss_values, dat0 )
{
    vars <- colnames(predictorMatrix)
    NV <- length(vars)
    dfr <- data.frame(variable=vars, M=NA, SD=NA )
    for (vv in 1L:NV){
        var_vv <- vars[vv]
        dfr$M[vv] <- stats::weighted.mean( x=dat[,var_vv], w=dat$weights, na.rm=TRUE)
        dfr$SD[vv] <- mdmb_weighted_sd( x=dat[,var_vv], w=dat$weights)
    }
    N <- nrow(dat0)
    N_obs <- N-freq_miss_values
    dfr1 <- data.frame( variable=names(freq_miss_values),
                        N_obs=N_obs, N_miss=freq_miss_values )
    dfr <- merge( x=dfr1, y=dfr, by='variable', all=TRUE)
    dfr <- dfr[ match( vars, dfr$variable ), ]
    return(dfr)
}
