## File Name: frm_prepare_data_em.R
## File Version: 0.25

frm_prepare_data_em <- function(dat, dep, ind, weights0, dat0)
{
    NM <- attr(ind,"NM")
    N <- nrow(dat)
    dat$case <- 1:N
    dat$weights0 <- weights0
    dat$weights <- 1
    dat$resp_all <- 1
    # vector of dependent variables
    dv_vars <- c()
    for (mm in 1:NM){
        # mm <- 1
        ind_mm <- ind[[mm]]
        var_mm <- ind_mm$dv_vars
        dv_vars <- c( dv_vars, var_mm)
        nodes_mm <- ind_mm$nodes
        dat <- frm_prepare_data_include_latent_data( dat=dat, var_mm=var_mm, nodes_mm=nodes_mm,
                    ind_mm=ind[[mm]] )
    }
    #** prepare dependent variables
    dat <- frm_prepare_data_include_latent_data( dat=dat,
                    var_mm=dep$dv_vars, nodes_mm=dep$nodes, ind_mm=dep )
    dv_vars <- c( dv_vars, dep$dv_vars )
    dat$weights <- dat$weights * dat$weights0
    # extract matrix with response indicators
    dat_resp <- dat[, paste0( "resp_", dv_vars ) ]
    ind_miss <- ind_resp <- list()
    freq_miss_values <- rep(0,NM+1)
    names(freq_miss_values) <- dv_vars
    for (mm in 1:(NM+1) ){
        ind_resp[[mm]] <- which( dat_resp[,mm]==1)
        ind_miss[[mm]] <- which( dat_resp[,mm]==0)
        freq_miss_values[mm] <- sum( is.na( dat0[, dv_vars[mm] ] ) )
    }
    res <- list( dat=dat, dv_vars=dv_vars, dat_resp=dat_resp,
                ind_resp=ind_resp, ind_miss=ind_miss,
                freq_miss_values=freq_miss_values)
    return(res)
}
